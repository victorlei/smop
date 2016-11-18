/*

Copyright (C) 1993-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <fstream>
#include <iostream>
#include <string>

#include "cmd-edit.h"
#include "oct-env.h"
#include "singleton-cleanup.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-obj.h"
#include "pager.h"
#include "procstream.h"
#include "sighandlers.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Our actual connection to the external pager.
static oprocstream *external_pager = 0;

// TRUE means we write to the diary file.
static bool write_to_diary_file = false;

// The name of the current diary file.
static std::string diary_file ("diary");

// The diary file.
static std::ofstream external_diary_file;

static std::string
default_pager (void)
{
  std::string pager_binary = octave_env::getenv ("PAGER");

#ifdef OCTAVE_DEFAULT_PAGER
  if (pager_binary.empty ())
    pager_binary = OCTAVE_DEFAULT_PAGER;
#endif

  return pager_binary;
}

// The shell command to run as the pager.
static std::string VPAGER = default_pager ();

// The options to pass to the pager.
static std::string VPAGER_FLAGS;

// TRUE means that if output is going to the pager, it is sent as soon
// as it is available.  Otherwise, it is buffered and only sent to the
// pager when it is time to print another prompt.
static bool Vpage_output_immediately = false;

// TRUE means all output intended for the screen should be passed
// through the pager.
static bool Vpage_screen_output = true;

static bool really_flush_to_pager = false;

static bool flushing_output_to_pager = false;

static void
clear_external_pager (void)
{
  if (external_pager)
    {
      octave_child_list::remove (external_pager->pid ());

      delete external_pager;
      external_pager = 0;
    }
}

static bool
pager_event_handler (pid_t pid, int status)
{
  bool retval = false;

  if (pid > 0)
    {
      if (octave_wait::ifexited (status) || octave_wait::ifsignaled (status))
        {
          // Avoid warning() since that will put us back in the pager,
          // which would be bad news.

          std::cerr << "warning: connection to external pager lost (pid = "
                    << pid << ")" << std::endl;
          std::cerr << "warning: flushing pending output (please wait)"
                    << std::endl;

          // Request removal of this PID from the list of child
          // processes.

          retval = true;
        }
    }

  return retval;
}

static std::string
pager_command (void)
{
  std::string cmd = VPAGER;

  if (! (cmd.empty () || VPAGER_FLAGS.empty ()))
    cmd += " " + VPAGER_FLAGS;

  return cmd;
}

static void
do_sync (const char *msg, int len, bool bypass_pager)
{
  if (msg && len > 0)
    {
      if (bypass_pager)
        {
          std::cout.write (msg, len);
          std::cout.flush ();
        }
      else
        {
          if (! external_pager)
            {
              std::string pgr = pager_command ();

              if (! pgr.empty ())
                {
                  external_pager = new oprocstream (pgr.c_str ());

                  if (external_pager)
                    octave_child_list::insert (external_pager->pid (),
                                               pager_event_handler);
                }
            }

          if (external_pager)
            {
              if (external_pager->good ())
                {
                  external_pager->write (msg, len);

                  external_pager->flush ();

#if defined (EPIPE)
                  if (errno == EPIPE)
                    external_pager->setstate (std::ios::failbit);
#endif
                }
              else
                {
                  // FIXME: omething is not right with the
                  // pager.  If it died then we should receive a
                  // signal for that.  If there is some other problem,
                  // then what?
                }
            }
          else
            {
              std::cout.write (msg, len);
              std::cout.flush ();
            }
        }
    }
}

// Assume our terminal wraps long lines.

static bool
more_than_a_screenful (const char *s, int len)
{
  if (s)
    {
      int available_rows = command_editor::terminal_rows () - 2;

      int cols = command_editor::terminal_cols ();

      int count = 0;

      int chars_this_line = 0;

      for (int i = 0; i < len; i++)
        {
          if (*s++ == '\n')
            {
              count += chars_this_line / cols + 1;
              chars_this_line = 0;
            }
          else
            chars_this_line++;
        }

      if (count > available_rows)
        return true;
    }

  return false;
}

int
octave_pager_buf::sync (void)
{
  if (! interactive || forced_interactive
      || really_flush_to_pager
      || (Vpage_screen_output && Vpage_output_immediately)
      || ! Vpage_screen_output)
    {
      char *buf = eback ();

      int len = pptr () - buf;

      bool bypass_pager = (! interactive || forced_interactive
                           || ! Vpage_screen_output
                           || (really_flush_to_pager
                               && Vpage_screen_output
                               && ! Vpage_output_immediately
                               && ! more_than_a_screenful (buf, len)));

      if (len > 0)
        {
          do_sync (buf, len, bypass_pager);

          flush_current_contents_to_diary ();

          seekoff (0, std::ios::beg);
        }
    }

  return 0;
}

void
octave_pager_buf::flush_current_contents_to_diary (void)
{
  char *buf = eback () + diary_skip;

  size_t len = pptr () - buf;

  octave_diary.write (buf, len);

  diary_skip = 0;
}

void
octave_pager_buf::set_diary_skip (void)
{
  diary_skip = pptr () - eback ();
}

int
octave_diary_buf::sync (void)
{
  if (write_to_diary_file && external_diary_file)
    {
      char *buf = eback ();

      int len = pptr () - buf;

      if (len > 0)
        external_diary_file.write (buf, len);
    }

  seekoff (0, std::ios::beg);

  return 0;
}

octave_pager_stream *octave_pager_stream::instance = 0;

octave_pager_stream::octave_pager_stream (void) : std::ostream (0), pb (0)
{
  pb = new octave_pager_buf ();
  rdbuf (pb);
  setf (unitbuf);
}

octave_pager_stream::~octave_pager_stream (void)
{
  flush ();
  delete pb;
}

std::ostream&
octave_pager_stream::stream (void)
{
  return instance_ok () ? *instance : std::cout;
}

void
octave_pager_stream::flush_current_contents_to_diary (void)
{
  if (instance_ok ())
    instance->do_flush_current_contents_to_diary ();
}

void
octave_pager_stream::set_diary_skip (void)
{
  if (instance_ok ())
    instance->do_set_diary_skip ();
}

// Reinitialize the pager buffer to avoid hanging on to large internal
// buffers when they might not be needed.  This function should only be
// called when the pager is not in use.  For example, just before
// getting command-line input.

void
octave_pager_stream::reset (void)
{
  if (instance_ok ())
    instance->do_reset ();
}

void
octave_pager_stream::do_flush_current_contents_to_diary (void)
{
  if (pb)
    pb->flush_current_contents_to_diary ();
}

void
octave_pager_stream::do_set_diary_skip (void)
{
  if (pb)
    pb->set_diary_skip ();
}

void
octave_pager_stream::do_reset (void)
{
  delete pb;
  pb = new octave_pager_buf ();
  rdbuf (pb);
  setf (unitbuf);
}

bool
octave_pager_stream::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_pager_stream ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create pager_stream object!");

      retval = false;
    }

  return retval;
}

octave_diary_stream *octave_diary_stream::instance = 0;

octave_diary_stream::octave_diary_stream (void) : std::ostream (0), db (0)
{
  db = new octave_diary_buf ();
  rdbuf (db);
  setf (unitbuf);
}

octave_diary_stream::~octave_diary_stream (void)
{
  flush ();
  delete db;
}

std::ostream&
octave_diary_stream::stream (void)
{
  return instance_ok () ? *instance : std::cout;
}

// Reinitialize the diary buffer to avoid hanging on to large internal
// buffers when they might not be needed.  This function should only be
// called when the pager is not in use.  For example, just before
// getting command-line input.

void
octave_diary_stream::reset (void)
{
  if (instance_ok ())
    instance->do_reset ();
}

void
octave_diary_stream::do_reset (void)
{
  delete db;
  db = new octave_diary_buf ();
  rdbuf (db);
  setf (unitbuf);
}

bool
octave_diary_stream::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_diary_stream ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create diary_stream object!");

      retval = false;
    }

  return retval;
}

void
flush_octave_stdout (void)
{
  if (! flushing_output_to_pager)
    {
      unwind_protect frame;

      frame.protect_var (really_flush_to_pager);
      frame.protect_var (flushing_output_to_pager);

      really_flush_to_pager = true;
      flushing_output_to_pager = true;

      octave_stdout.flush ();

      clear_external_pager ();
    }
}

static void
close_diary_file (void)
{
  // Try to flush the current buffer to the diary now, so that things
  // like
  //
  // function foo ()
  //   diary on;
  //   ...
  //   diary off;
  // endfunction
  //
  // will do the right thing.

  octave_pager_stream::flush_current_contents_to_diary ();

  if (external_diary_file.is_open ())
    {
      octave_diary.flush ();
      external_diary_file.close ();
    }
}

static void
open_diary_file (void)
{
  close_diary_file ();

  // If there is pending output in the pager buf, it should not go
  // into the diary file.

  octave_pager_stream::set_diary_skip ();

  external_diary_file.open (diary_file.c_str (), std::ios::app);

  if (! external_diary_file)
    error ("diary: can't open diary file '%s'", diary_file.c_str ());
}

DEFUN (diary, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} diary\n\
@deftypefnx {Command} {} diary on\n\
@deftypefnx {Command} {} diary off\n\
@deftypefnx {Command} {} diary @var{filename}\n\
Record a list of all commands @emph{and} the output they produce, mixed\n\
together just as they appear on the terminal.\n\
\n\
Valid options are:\n\
\n\
@table @asis\n\
@item on\n\
Start recording a session in a file called @file{diary} in the\n\
current working directory.\n\
\n\
@item off\n\
Stop recording the session in the diary file.\n\
\n\
@item @var{filename}\n\
Record the session in the file named @var{filename}.\n\
@end table\n\
\n\
With no arguments, @code{diary} toggles the current diary state.\n\
@seealso{history}\n\
@end deftypefn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("diary");

  if (error_state)
    return retval;

  if (diary_file.empty ())
    diary_file = "diary";

  switch (argc)
    {
    case 1:
      write_to_diary_file = ! write_to_diary_file;
      open_diary_file ();
      break;

    case 2:
      {
        std::string arg = argv[1];

        if (arg == "on")
          {
            write_to_diary_file = true;
            open_diary_file ();
          }
        else if (arg == "off")
          {
            close_diary_file ();
            write_to_diary_file = false;
          }
        else
          {
            diary_file = arg;
            write_to_diary_file = true;
            open_diary_file ();
          }
      }
      break;

    default:
      print_usage ();
      break;
    }

  return retval;
}

DEFUN (__diaryfile__, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{fname} =} __diaryfile__ ()\n\
Undocumented internal function\n\
@end deftypefn")
{
  return ovl (diary_file);
}

DEFUN (__diarystate__, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{state} =} __diarystate__ ()\n\
Undocumented internal function\n\
@end deftypefn")
{
  return ovl (write_to_diary_file);
}

DEFUN (more, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} more\n\
@deftypefnx {Command} {} more on\n\
@deftypefnx {Command} {} more off\n\
Turn output pagination on or off.\n\
\n\
Without an argument, @code{more} toggles the current state.\n\
\n\
The current state can be determined via @code{page_screen_output}.\n\
@seealso{page_screen_output, page_output_immediately, PAGER, PAGER_FLAGS}\n\
@end deftypefn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("more");

  if (error_state)
    return retval;

  if (argc == 2)
    {
      std::string arg = argv[1];

      if (arg == "on")
        Vpage_screen_output = true;
      else if (arg == "off")
        Vpage_screen_output = false;
      else
        error ("more: unrecognized argument '%s'", arg.c_str ());
    }
  else if (argc == 1)
    Vpage_screen_output = ! Vpage_screen_output;
  else
    print_usage ();

  return retval;
}

DEFUN (terminal_size, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} terminal_size ()\n\
Return a two-element row vector containing the current size of the terminal\n\
window in characters (rows and columns).\n\
@seealso{list_in_columns}\n\
@end deftypefn")
{
  RowVector size (2, 0.0);

  size(0) = command_editor::terminal_rows ();
  size(1) = command_editor::terminal_cols ();

  return octave_value (size);
}

DEFUN (page_output_immediately, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} page_output_immediately ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} page_output_immediately (@var{new_val})\n\
@deftypefnx {Built-in Function} {} page_output_immediately (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether Octave sends\n\
output to the pager as soon as it is available.\n\
\n\
Otherwise, Octave buffers its output and waits until just before the prompt\n\
is printed to flush it to the pager.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{page_screen_output, more, PAGER, PAGER_FLAGS}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (page_output_immediately);
}

DEFUN (page_screen_output, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} page_screen_output ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} page_screen_output (@var{new_val})\n\
@deftypefnx {Built-in Function} {} page_screen_output (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether output intended\n\
for the terminal window that is longer than one page is sent through a\n\
pager.\n\
\n\
This allows you to view one screenful at a time.  Some pagers\n\
(such as @code{less}---see @ref{Installation}) are also capable of moving\n\
backward on the output.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{more, page_output_immediately, PAGER, PAGER_FLAGS}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (page_screen_output);
}

DEFUN (PAGER, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} PAGER ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} PAGER (@var{new_val})\n\
@deftypefnx {Built-in Function} {} PAGER (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the program to use\n\
to display terminal output on your system.\n\
\n\
The default value is normally @qcode{\"less\"}, @qcode{\"more\"}, or\n\
@qcode{\"pg\"}, depending on what programs are installed on your system.\n\
@xref{Installation}.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{PAGER_FLAGS, page_output_immediately, more, page_screen_output}\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (PAGER);
}

DEFUN (PAGER_FLAGS, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} PAGER_FLAGS ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} PAGER_FLAGS (@var{new_val})\n\
@deftypefnx {Built-in Function} {} PAGER_FLAGS (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the options to pass\n\
to the pager.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{PAGER, more, page_screen_output, page_output_immediately}\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (PAGER_FLAGS);
}
