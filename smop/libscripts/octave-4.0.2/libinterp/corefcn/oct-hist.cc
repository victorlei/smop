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

/*

The functions listed below were adapted from similar functions from
GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

  do_history         edit_history_readline
  do_edit_history    edit_history_add_hist

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <cstring>

#include <string>

#include <fstream>

#include <sys/types.h>
#include <unistd.h>

#include "cmd-hist.h"
#include "file-ops.h"
#include "lo-mappers.h"
#include "octave-link.h"
#include "oct-env.h"
#include "oct-time.h"
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-hist.h"
#include "oct-obj.h"
#include "pager.h"
#include "parse.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// TRUE means input is coming from temporary history file.
bool input_from_tmp_history_file = false;

static std::string
default_history_file (void)
{
  std::string file;

  std::string env_file = octave_env::getenv ("OCTAVE_HISTFILE");

  if (! env_file.empty ())
    file = env_file;

  if (file.empty ())
    file = file_ops::concat (octave_env::get_home_directory (),
                             ".octave_hist");

  return file;
}

static int
default_history_size (void)
{
  int size = 1000;

  std::string env_size = octave_env::getenv ("OCTAVE_HISTSIZE");

  if (! env_size.empty ())
    {
      int val;

      if (sscanf (env_size.c_str (), "%d", &val) == 1)
        size = val > 0 ? val : 0;
    }

  return size;
}

static std::string
default_history_timestamp_format (void)
{
  return
    std::string ("# Octave " OCTAVE_VERSION ", %a %b %d %H:%M:%S %Y %Z <")
    + octave_env::get_user_name ()
    + std::string ("@")
    + octave_env::get_host_name ()
    + std::string (">");
}

// The format of the timestamp marker written to the history file when
// Octave exits.
static std::string Vhistory_timestamp_format_string
  = default_history_timestamp_format ();

// Display, save, or load history.  Stolen and modified from bash.
//
// Arg of -w FILENAME means write file, arg of -r FILENAME
// means read file, arg of -q means don't number lines.  Arg of N
// means only display that many items.

static string_vector
do_history (const octave_value_list& args, int nargout)
{
  bool numbered_output = nargout == 0;

  unwind_protect frame;

  string_vector hlist;

  frame.add_fcn (command_history::set_file, command_history::file ());

  int nargin = args.length ();

  // Number of history lines to show (-1 = all)
  int limit = -1;

  for (octave_idx_type i = 0; i < nargin; i++)
    {
      octave_value arg = args(i);

      std::string option;

      if (arg.is_string ())
        option = arg.string_value ();
      else if (arg.is_numeric_type ())
        {
          limit = arg.int_value ();
          if (limit < 0)
            limit = -limit;
          continue;
        }
      else
        {
          gripe_wrong_type_arg ("history", arg);
          return hlist;
        }

      if (option == "-r" || option == "-w" || option == "-a"
          || option == "-n")
        {
          if (i < nargin - 1)
            {
              if (args(i+1).is_string ())
                command_history::set_file (args(++i).string_value ());
              else
                {
                  error ("history: expecting file name for %s option",
                         option.c_str ());
                  return hlist;
                }
            }
          else
            command_history::set_file (default_history_file ());

          if (option == "-a")
            // Append 'new' lines to file.
            command_history::append ();

          else if (option == "-w")
            // Write entire history.
            command_history::write ();

          else if (option == "-r")
            {
              // Read entire file.
              command_history::read ();
              octave_link::set_history (command_history::list ());
            }

          else if (option == "-n")
            {
              // Read 'new' history from file.
              command_history::read_range ();
              octave_link::set_history (command_history::list ());
            }

          else
            panic_impossible ();

          return hlist;
        }
      else if (option == "-c")
        {
          command_history::clear ();
          octave_link::clear_history ();
        }
      else if (option == "-q")
        numbered_output = false;
      else if (option == "--")
        {
          i++;
          break;
        }
      else
        {
          // The last argument found in the command list that looks like
          // an integer will be used
          int tmp;

          if (sscanf (option.c_str (), "%d", &tmp) == 1)
            {
              if (tmp > 0)
                limit = tmp;
              else
                limit = -tmp;
            }

          else
            {
              if (option.length () > 0 && option[0] == '-')
                error ("history: unrecognized option '%s'", option.c_str ());
              else
                error ("history: bad non-numeric arg '%s'", option.c_str ());

              return  hlist;
            }
        }
    }

  hlist = command_history::list (limit, numbered_output);

  int len = hlist.length ();

  if (nargout == 0)
    {
      for (octave_idx_type i = 0; i < len; i++)
        octave_stdout << hlist[i] << "\n";
    }

  return hlist;
}

// Read the edited history lines from STREAM and return them
// one at a time.  This can read unlimited length lines.  The
// caller should free the storage.

static char *
edit_history_readline (std::fstream& stream)
{
  char c;
  int line_len = 128;
  int lindex = 0;
  char *line = new char [line_len];
  line[0] = '\0';

  while (stream.get (c))
    {
      if (lindex + 2 >= line_len)
        {
          char *tmp_line = new char [line_len += 128];
          strcpy (tmp_line, line);
          delete [] line;
          line = tmp_line;
        }

      if (c == '\n')
        {
          line[lindex++] = '\n';
          line[lindex++] = '\0';
          return line;
        }
      else
        line[lindex++] = c;
    }

  if (! lindex)
    {
      delete [] line;
      return 0;
    }

  if (lindex + 2 >= line_len)
    {
      char *tmp_line = new char [lindex+3];
      strcpy (tmp_line, line);
      delete [] line;
      line = tmp_line;
    }

  // Finish with newline if none in file.

  line[lindex++] = '\n';
  line[lindex++] = '\0';
  return line;
}

static void
edit_history_add_hist (const std::string& line)
{
  if (! line.empty ())
    {
      std::string tmp = line;

      int len = tmp.length ();

      if (len > 0 && tmp[len-1] == '\n')
        tmp.resize (len - 1);

      if (! tmp.empty ())
        if (command_history::add (tmp))
          octave_link::append_history (tmp);
    }
}

static bool
get_int_arg (const octave_value& arg, int& val)
{
  bool ok = true;

  if (arg.is_string ())
    {
      std::string tmp = arg.string_value ();

      ok = sscanf (tmp.c_str (), "%d", &val) == 1;
    }
  else if (arg.is_numeric_type ())
    val = arg.int_value ();
  else
    ok = false;

  return ok;
}

static std::string
mk_tmp_hist_file (const octave_value_list& args,
                  bool insert_curr, const char *warn_for)
{
  std::string retval;

  string_vector hlist = command_history::list ();

  int hist_count = hlist.length () - 1;  // switch to zero-based indexing

  // The current command line is already part of the history list by
  // the time we get to this point.  Delete the cmd from the list when
  // executing 'edit_history' so that it doesn't show up in the history
  // but the actual commands performed will.

  if (! insert_curr)
    command_history::remove (hist_count);

  hist_count--;  // skip last entry in history list

  // If no numbers have been specified, the default is to edit the
  // last command in the history list.

  int hist_beg = hist_count;
  int hist_end = hist_count;

  bool reverse = false;

  // Process options.

  int nargin = args.length ();

  bool usage_error = false;
  if (nargin == 2)
    {
      if (get_int_arg (args(0), hist_beg)
          && get_int_arg (args(1), hist_end))
        {
          if (hist_beg < 0)
            hist_beg += (hist_count + 1);
          else
            hist_beg--;
          if (hist_end < 0)
            hist_end += (hist_count + 1);
          else
            hist_end--;
        }
      else
        usage_error = true;
    }
  else if (nargin == 1)
    {
      if (get_int_arg (args(0), hist_beg))
        {
          if (hist_beg < 0)
            hist_beg += (hist_count + 1);
          else
            hist_beg--;
          hist_end = hist_beg;
        }
      else
        usage_error = true;
    }

  if (usage_error)
    {
      usage ("%s [first] [last]", warn_for);
      return retval;
    }

  if (hist_beg > hist_count || hist_end > hist_count)
    {
      error ("%s: history specification out of range", warn_for);
      return retval;
    }

  if (hist_end < hist_beg)
    {
      std::swap (hist_end, hist_beg);
      reverse = true;
    }

  std::string name = octave_tempnam ("", "oct-");

  std::fstream file (name.c_str (), std::ios::out);

  if (! file)
    {
      error ("%s: couldn't open temporary file '%s'", warn_for,
             name.c_str ());
      return retval;
    }

  if (reverse)
    {
      for (int i = hist_end; i >= hist_beg; i--)
        file << hlist[i] << "\n";
    }
  else
    {
      for (int i = hist_beg; i <= hist_end; i++)
        file << hlist[i] << "\n";
    }

  file.close ();

  return name;
}

static void
unlink_cleanup (const char *file)
{
  gnulib::unlink (file);
}

static void
do_edit_history (const octave_value_list& args)
{
  std::string name = mk_tmp_hist_file (args, false, "edit_history");

  if (name.empty ())
    return;

  // Call up our favorite editor on the file of commands.

  std::string cmd = VEDITOR;
  cmd.append (" \"" + name + "\"");

  // Ignore interrupts while we are off editing commands.  Should we
  // maybe avoid using system()?

  volatile octave_interrupt_handler old_interrupt_handler
    = octave_ignore_interrupts ();

  int status = system (cmd.c_str ());

  octave_set_interrupt_handler (old_interrupt_handler);

  // Check if text edition was successfull.  Abort the operation
  // in case of failure.
  if (status != EXIT_SUCCESS)
    {
      error ("edit_history: text editor command failed");
      return;
    }

  // Write the commands to the history file since source_file
  // disables command line history while it executes.

  std::fstream file (name.c_str (), std::ios::in);

  char *line;
  //int first = 1;
  while ((line = edit_history_readline (file)) != 0)
    {
      // Skip blank lines.

      if (line[0] == '\n')
        {
          delete [] line;
          continue;
        }

      edit_history_add_hist (line);

      delete [] line;
    }

  file.close ();

  // Turn on command echo, so the output from this will make better
  // sense.

  unwind_protect frame;

  frame.add_fcn (unlink_cleanup, name.c_str ());
  frame.protect_var (Vecho_executing_commands);
  frame.protect_var (input_from_tmp_history_file);

  Vecho_executing_commands = ECHO_CMD_LINE;
  input_from_tmp_history_file = true;

  source_file (name);
}

static void
do_run_history (const octave_value_list& args)
{
  std::string name = mk_tmp_hist_file (args, false, "run_history");

  if (name.empty ())
    return;

  // Turn on command echo so the output from this will make better sense.

  unwind_protect frame;

  frame.add_fcn (unlink_cleanup, name.c_str ());
  frame.protect_var (Vecho_executing_commands);
  frame.protect_var (input_from_tmp_history_file);

  Vecho_executing_commands = ECHO_CMD_LINE;
  input_from_tmp_history_file = true;

  source_file (name);
}

void
initialize_history (bool read_history_file)
{
  command_history::initialize (read_history_file,
                               default_history_file (),
                               default_history_size (),
                               octave_env::getenv ("OCTAVE_HISTCONTROL"));

  octave_link::set_history (command_history::list ());
}

void
octave_history_write_timestamp (void)
{
  octave_localtime now;

  std::string timestamp = now.strftime (Vhistory_timestamp_format_string);

  if (! timestamp.empty ())
    if (command_history::add (timestamp))
      octave_link::append_history (timestamp);
}

DEFUN (edit_history, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} edit_history\n\
@deftypefnx {Command} {} edit_history @var{cmd_number}\n\
@deftypefnx {Command} {} edit_history @var{first} @var{last}\n\
Edit the history list using the editor named by the variable @env{EDITOR}.\n\
\n\
The commands to be edited are first copied to a temporary file.  When you\n\
exit the editor, Octave executes the commands that remain in the file.  It\n\
is often more convenient to use @code{edit_history} to define functions\n\
rather than attempting to enter them directly on the command line.\n\
The block of commands is executed as soon as you exit the editor.\n\
To avoid executing any commands, simply delete all the lines from the buffer\n\
before leaving the editor.\n\
\n\
When invoked with no arguments, edit the previously executed command;\n\
With one argument, edit the specified command @var{cmd_number};\n\
With two arguments, edit the list of commands between @var{first} and\n\
@var{last}.  Command number specifiers may also be negative where -1\n\
refers to the most recently executed command.\n\
The following are equivalent and edit the most recently executed command.\n\
\n\
@example\n\
@group\n\
edit_history\n\
edit_history -1\n\
@end group\n\
@end example\n\
\n\
When using ranges, specifying a larger number for the first command than the\n\
last command reverses the list of commands before they are placed in the\n\
buffer to be edited.\n\
@seealso{run_history, history}\n\
@end deftypefn")
{
  octave_value_list retval;

  do_edit_history (args);

  return retval;
}

DEFUN (history, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} history\n\
@deftypefnx {Command} {} history @var{opt1} @dots{}\n\
@deftypefnx {Built-in Function} {@var{h} =} history ()\n\
@deftypefnx {Built-in Function} {@var{h} =} history (@var{opt1}, @dots{})\n\
If invoked with no arguments, @code{history} displays a list of commands\n\
that you have executed.\n\
\n\
Valid options are:\n\
\n\
@table @code\n\
@item   @var{n}\n\
@itemx -@var{n}\n\
Display only the most recent @var{n} lines of history.\n\
\n\
@item -c\n\
Clear the history list.\n\
\n\
@item -q\n\
Don't number the displayed lines of history.  This is useful for cutting\n\
and pasting commands using the X Window System.\n\
\n\
@item -r @var{file}\n\
Read the file @var{file}, appending its contents to the current\n\
history list.  If the name is omitted, use the default history file\n\
(normally @file{~/.octave_hist}).\n\
\n\
@item -w @var{file}\n\
Write the current history to the file @var{file}.  If the name is\n\
omitted, use the default history file (normally @file{~/.octave_hist}).\n\
@end table\n\
\n\
For example, to display the five most recent commands that you have\n\
typed without displaying line numbers, use the command\n\
@kbd{history -q 5}.\n\
\n\
If invoked with a single output argument, the history will be saved to that\n\
argument as a cell string and will not be output to screen.\n\
@seealso{edit_history, run_history}\n\
@end deftypefn")
{
  octave_value retval;

  string_vector hlist = do_history (args, nargout);

  if (nargout > 0)
    retval = Cell (hlist);

  return retval;
}

DEFUN (run_history, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} run_history\n\
@deftypefnx {Command} {} run_history @var{cmd_number}\n\
@deftypefnx {Command} {} run_history @var{first} @var{last}\n\
Run commands from the history list.\n\
\n\
When invoked with no arguments, run the previously executed command;\n\
\n\
With one argument, run the specified command @var{cmd_number};\n\
\n\
With two arguments, run the list of commands between @var{first} and\n\
@var{last}.  Command number specifiers may also be negative where -1\n\
refers to the most recently executed command.  For example, the command\n\
\n\
@example\n\
@group\n\
run_history\n\
     OR\n\
run_history -1\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
executes the most recent command again.\n\
The command\n\
\n\
@example\n\
run_history 13 169\n\
@end example\n\
\n\
@noindent\n\
executes commands 13 through 169.\n\
\n\
Specifying a larger number for the first command than the last command\n\
reverses the list of commands before executing them.\n\
For example:\n\
\n\
@example\n\
@group\n\
disp (1)\n\
disp (2)\n\
run_history -1 -2\n\
@result{}\n\
 2\n\
 1\n\
@end group\n\
@end example\n\
\n\
@seealso{edit_history, history}\n\
@end deftypefn")
{
  octave_value_list retval;

  do_run_history (args);

  return retval;
}

DEFUN (history_control, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} history_control ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} history_control (@var{new_val})\n\
Query or set the internal variable that specifies how commands are saved\n\
to the history list.\n\
\n\
The default value is an empty character string, but may be overridden by the\n\
environment variable @w{@env{OCTAVE_HISTCONTROL}}.\n\
\n\
The value of @code{history_control} is a colon-separated list of values\n\
controlling how commands are saved on the history list.  If the list\n\
of values includes @code{ignorespace}, lines which begin with a space\n\
character are not saved in the history list.  A value of @code{ignoredups}\n\
causes lines matching the previous history entry to not be saved.\n\
A value of @code{ignoreboth} is shorthand for @code{ignorespace} and\n\
@code{ignoredups}.  A value of @code{erasedups} causes all previous lines\n\
matching the current line to be removed from the history list before that\n\
line is saved.  Any value not in the above list is ignored.  If\n\
@code{history_control} is the empty string, all commands are saved on\n\
the history list, subject to the value of @code{history_save}.\n\
@seealso{history_file, history_size, history_timestamp_format_string, history_save}\n\
@end deftypefn")
{
  octave_value retval;

  std::string old_history_control = command_history::histcontrol ();

  std::string tmp = old_history_control;

  retval = set_internal_variable (tmp, args, nargout, "history_control");

  if (tmp != old_history_control)
    command_history::process_histcontrol (tmp);

  return retval;
}

DEFUN (history_size, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} history_size ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} history_size (@var{new_val})\n\
Query or set the internal variable that specifies how many entries\n\
to store in the history file.\n\
\n\
The default value is @code{1000}, but may be overridden by the environment\n\
variable @w{@env{OCTAVE_HISTSIZE}}.\n\
@seealso{history_file, history_timestamp_format_string, history_save}\n\
@end deftypefn")
{
  octave_value retval;

  int old_history_size = command_history::size ();

  int tmp = old_history_size;

  retval = set_internal_variable (tmp, args, nargout,
                                  "history_size", -1,
                                  std::numeric_limits<int>::max ());

  if (tmp != old_history_size)
    command_history::set_size (tmp);

  return retval;
}

DEFUN (history_file, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} history_file ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} history_file (@var{new_val})\n\
Query or set the internal variable that specifies the name of the\n\
file used to store command history.\n\
\n\
The default value is @file{~/.octave_hist}, but may be overridden by the\n\
environment variable @w{@env{OCTAVE_HISTFILE}}.\n\
@seealso{history_size, history_save, history_timestamp_format_string}\n\
@end deftypefn")
{
  octave_value retval;

  std::string old_history_file = command_history::file ();

  std::string tmp = old_history_file;

  retval = set_internal_variable (tmp, args, nargout, "history_file");

  if (tmp != old_history_file)
    command_history::set_file (tmp);

  return retval;
}

DEFUN (history_timestamp_format_string, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} history_timestamp_format_string ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} history_timestamp_format_string (@var{new_val})\n\
@deftypefnx {Built-in Function} {} history_timestamp_format_string (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the format string\n\
for the comment line that is written to the history file when Octave\n\
exits.\n\
\n\
The format string is passed to @code{strftime}.  The default value is\n\
\n\
@example\n\
\"# Octave VERSION, %a %b %d %H:%M:%S %Y %Z <USER@@HOST>\"\n\
@end example\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{strftime, history_file, history_size, history_save}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (history_timestamp_format_string);
}

DEFUN (history_save, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} history_save ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} history_save (@var{new_val})\n\
@deftypefnx {Built-in Function} {} history_save (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether commands entered\n\
on the command line are saved in the history file.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{history_control, history_file, history_size, history_timestamp_format_string}\n\
@end deftypefn")
{
  octave_value retval;

  bool old_history_save = ! command_history::ignoring_entries ();

  bool tmp = old_history_save;

  retval = set_internal_variable (tmp, args, nargout, "history_save");

  if (tmp != old_history_save)
    command_history::ignore_entries (! tmp);

  return retval;
}
