/*

Copyright (C) 1995-2015 John W. Eaton

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

#include <cassert>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <new>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include <sys/select.h>
#include <sys/types.h>
#include <unistd.h>

#include "cmd-edit.h"
#include "cmd-hist.h"
#include "file-ops.h"
#include "lo-error.h"
#include "lo-mappers.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "quit.h"
#include "singleton-cleanup.h"
#include "str-vec.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "file-io.h"
#include "graphics.h"
#include "input.h"
#include "lex.h"
#include "load-save.h"
#include "octave-link.h"
#include "oct-conf.h"
#include "oct-conf-features.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "ov.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "pt-eval.h"
#include "pt-jump.h"
#include "pt-stmt.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "syswait.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

#ifndef SHELL_PATH
#define SHELL_PATH "/bin/sh"
#endif

void (*octave_exit) (int) = ::exit;

// TRUE means the quit() call is allowed.
bool quit_allowed = true;

// TRUE means we are exiting via the builtin exit or quit functions.
bool quitting_gracefully = false;
// This stores the exit status.
int exit_status = 0;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
bool octave_interpreter_ready = false;

// TRUE means we've processed all the init code and we are good to go.
bool octave_initialized = false;

octave_call_stack *octave_call_stack::instance = 0;

std::string
octave_call_stack::stack_frame::fcn_file_name (void) const
{
  return m_fcn ? m_fcn->fcn_file_name () : std::string ();
}

std::string
octave_call_stack::stack_frame::fcn_name (bool print_subfn) const
{
  std::string retval;

  if (m_fcn)
    {
      std::string parent_fcn_name = m_fcn->parent_fcn_name ();

      if (print_subfn && ! parent_fcn_name.empty ())
        retval = parent_fcn_name + Vfilemarker;

      retval += m_fcn->name ();
    }
  else
    retval = "<unknown>";

  return retval;
}

void
octave_call_stack::create_instance (void)
{
  instance = new octave_call_stack ();

  if (instance)
    {
      instance->do_push (0, symbol_table::top_scope (), 0);

      singleton_cleanup_list::add (cleanup_instance);
    }
}

int
octave_call_stack::do_current_line (void) const
{
  int retval = -1;

  if (! cs.empty ())
    {
      const stack_frame& elt = cs[curr_frame];
      retval = elt.m_line;
    }

  return retval;
}

int
octave_call_stack::do_current_column (void) const
{
  int retval = -1;

  if (! cs.empty ())
    {
      const stack_frame& elt = cs[curr_frame];
      retval = elt.m_column;
    }

  return retval;
}

int
octave_call_stack::do_caller_user_code_line (void) const
{
  int retval = -1;

  const_iterator p = cs.end ();

  while (p != cs.begin ())
    {
      const stack_frame& elt = *(--p);

      octave_function *f = elt.m_fcn;

      if (f && f->is_user_code ())
        {
          if (elt.m_line > 0)
            {
              retval = elt.m_line;
              break;
            }
        }
    }

  return retval;
}

int
octave_call_stack::do_caller_user_code_column (void) const
{
  int retval = -1;

  const_iterator p = cs.end ();

  while (p != cs.begin ())
    {
      const stack_frame& elt = *(--p);

      octave_function *f = elt.m_fcn;

      if (f && f->is_user_code ())
        {
          if (elt.m_column)
            {
              retval = elt.m_column;
              break;
            }
        }
    }

  return retval;
}

size_t
octave_call_stack::do_num_user_code_frames
  (octave_idx_type& curr_user_frame) const
{
  size_t retval = 0;

  curr_user_frame = 0;

  // Look for the caller of dbstack.
  size_t xframe = cs[curr_frame].m_prev;

  bool found = false;

  size_t k = cs.size ();

  for (const_reverse_iterator p = cs.rbegin (); p != cs.rend (); p++)
    {
      octave_function *f = (*p).m_fcn;

      if (--k == xframe)
        found = true;

      if (f && f->is_user_code ())
        {
          if (! found)
            curr_user_frame++;

          retval++;
        }
    }

  // We counted how many user frames were not the one, in reverse.
  // Now set curr_user_frame to be the index in the other direction.
  curr_user_frame = retval - curr_user_frame - 1;

  return retval;
}

octave_user_code *
octave_call_stack::do_caller_user_code (size_t nskip) const
{
  octave_user_code *retval = 0;

  const_iterator p = cs.end ();

  while (p != cs.begin ())
    {
      const stack_frame& elt = *(--p);

      octave_function *f = elt.m_fcn;

      if (f && f->is_user_code ())
        {
          if (nskip > 0)
            nskip--;
          else
            {
              retval = dynamic_cast<octave_user_code *> (f);
              break;
            }
        }
    }

  return retval;
}

bool
octave_call_stack::do_all_scripts (void) const
{
  bool retval = true;

  const_iterator p = cs.end ();

  while (p != cs.begin ())
    {
      const stack_frame& elt = *(--p);

      octave_function *f = elt.m_fcn;

      if (f && ! f->is_user_script ())
        {
          retval = false;
          break;
        }
    }

  return retval;
}

// Use static fields for the best efficiency.
// NOTE: C++0x will allow these two to be merged into one.
static const char *bt_fieldnames[] = { "file", "name", "line",
                                       "column", "scope", "context", 0
                                     };
static const octave_fields bt_fields (bt_fieldnames);

octave_map
octave_call_stack::empty_backtrace (void)
{
  return octave_map (dim_vector (0, 1), bt_fields);
}

std::list<octave_call_stack::stack_frame>
octave_call_stack::do_backtrace_frames (size_t nskip,
                                        octave_idx_type& curr_user_frame) const
{
  std::list<octave_call_stack::stack_frame> retval;

  size_t user_code_frames = do_num_user_code_frames (curr_user_frame);

  size_t nframes = nskip <= user_code_frames ? user_code_frames - nskip : 0;

  // Our list is reversed.
  curr_user_frame = nframes - curr_user_frame - 1;

  if (nframes > 0)
    {
      for (const_reverse_iterator p = cs.rbegin (); p != cs.rend (); p++)
        {
          const stack_frame& elt = *p;

          octave_function *f = elt.m_fcn;

          if (f && f->is_user_code ())
            {
              if (nskip > 0)
                nskip--;
              else
                retval.push_back (elt);
            }
        }
    }

  return retval;
}

octave_map
octave_call_stack::do_backtrace (size_t nskip,
                                 octave_idx_type& curr_user_frame,
                                 bool print_subfn) const
{
  std::list<octave_call_stack::stack_frame> frames
    = do_backtrace_frames (nskip, curr_user_frame);

  size_t nframes = frames.size ();

  octave_map retval (dim_vector (nframes, 1), bt_fields);

  Cell& file = retval.contents (0);
  Cell& name = retval.contents (1);
  Cell& line = retval.contents (2);
  Cell& column = retval.contents (3);
  Cell& scope = retval.contents (4);
  Cell& context = retval.contents (5);

  octave_idx_type k = 0;

  for (std::list<octave_call_stack::stack_frame>::const_iterator p = frames.begin ();
       p != frames.end (); p++)
    {
      const stack_frame& elt = *p;

      scope(k) = elt.m_scope;
      context(k) = elt.m_context;
      file(k) = elt.fcn_file_name ();
      name(k) = elt.fcn_name (print_subfn);
      line(k) = elt.m_line;
      column(k) = elt.m_column;

      k++;
    }

  return retval;
}

bool
octave_call_stack::do_goto_frame (size_t n, bool verbose)
{
  bool retval = false;

  if (n < cs.size ())
    {
      retval = true;

      curr_frame = n;

      const stack_frame& elt = cs[n];

      symbol_table::set_scope_and_context (elt.m_scope, elt.m_context);

      if (verbose)
        octave_stdout << "stopped in " << elt.fcn_name ()
                      << " at line " << elt.m_line
                      << " column " << elt.m_column
                      << " (" << elt.m_scope << "[" << elt.m_context << "])"
                      << std::endl;
    }

  return retval;
}

bool
octave_call_stack::do_goto_frame_relative (int nskip, bool verbose)
{
  bool retval = false;

  int incr = 0;

  if (nskip < 0)
    incr = -1;
  else if (nskip > 0)
    incr = 1;

  // Start looking with the caller of dbup/dbdown/keyboard.
  size_t xframe = cs[curr_frame].m_prev;

  while (true)
    {
      if ((incr < 0 && xframe == 0) || (incr > 0 && xframe == cs.size () - 1))
        break;

      xframe += incr;

      const stack_frame& elt = cs[xframe];

      octave_function *f = elt.m_fcn;

      if (xframe == 0 || (f && f->is_user_code ()))
        {
          if (nskip > 0)
            nskip--;
          else if (nskip < 0)
            nskip++;

          if (nskip == 0)
            {
              curr_frame = xframe;
              cs[cs.size () - 1].m_prev = curr_frame;

              symbol_table::set_scope_and_context (elt.m_scope, elt.m_context);

              if (verbose)
                {
                  std::ostringstream buf;

                  if (f)
                    buf << "stopped in " << f->name ()
                        << " at line " << elt.m_line << std::endl;
                  else
                    buf << "at top level" << std::endl;

                  octave_stdout << buf.str ();
                }

              retval = true;
              break;
            }
        }
      else if (incr == 0)  // Break out of infinite loop by choosing an incr.
        incr = -1;

      // There is no need to set scope and context here.  That will
      // happen when the dbup/dbdown/keyboard frame is popped and we
      // jump to the new "prev" frame set above.
    }

  return retval;
}

void
octave_call_stack::do_goto_caller_frame (void)
{
  size_t xframe = curr_frame;

  bool skipped = false;

  while (xframe != 0)
    {
      xframe = cs[xframe].m_prev;

      const stack_frame& elt = cs[xframe];

      octave_function *f = elt.m_fcn;

      if (elt.m_scope == cs[0].m_scope || (f && f->is_user_code ()))
        {
          if (! skipped)
            // We found the current user code frame, so skip it.
            skipped = true;
          else
            {
              // We found the caller user code frame.
              stack_frame tmp (elt);
              tmp.m_prev = curr_frame;

              curr_frame = cs.size ();

              cs.push_back (tmp);

              symbol_table::set_scope_and_context (tmp.m_scope, tmp.m_context);

              break;
            }
        }
    }
}

void
octave_call_stack::do_goto_base_frame (void)
{
  stack_frame tmp (cs[0]);
  tmp.m_prev = curr_frame;

  curr_frame = cs.size ();

  cs.push_back (tmp);

  symbol_table::set_scope_and_context (tmp.m_scope, tmp.m_context);
}

void
recover_from_exception (void)
{
  can_interrupt = true;
  octave_interrupt_immediately = 0;
  octave_interrupt_state = 0;
  octave_signal_caught = 0;
  octave_exception_state = octave_no_exception;
  octave_restore_signal_mask ();
  octave_catch_interrupts ();
}

int
main_loop (void)
{
  octave_save_signal_mask ();

  can_interrupt = true;

  octave_signal_hook = octave_signal_handler;
  octave_interrupt_hook = 0;
  octave_bad_alloc_hook = 0;

  octave_catch_interrupts ();

  octave_initialized = true;

  // The big loop.

  octave_lexer *lxr = (interactive
                       ? new octave_lexer ()
                       : new octave_lexer (stdin));

  octave_parser parser (*lxr);

  int retval = 0;
  do
    {
      try
        {
          reset_error_handler ();

          parser.reset ();

          if (symbol_table::at_top_level ())
            tree_evaluator::reset_debug_state ();

          retval = parser.run ();

          if (retval == 0)
            {
              if (parser.stmt_list)
                {
                  parser.stmt_list->accept (*current_evaluator);

                  octave_quit ();

                  if (! interactive)
                    {
                      bool quit = (tree_return_command::returning
                                   || tree_break_command::breaking);

                      if (tree_return_command::returning)
                        tree_return_command::returning = 0;

                      if (tree_break_command::breaking)
                        tree_break_command::breaking--;

                      if (quit)
                        break;
                    }

                  if (error_state)
                    {
                      if (! interactive)
                        {
                          // We should exit with a nonzero status.
                          retval = 1;
                          break;
                        }
                    }
                  else
                    {
                      if (octave_completion_matches_called)
                        octave_completion_matches_called = false;
                      else
                        command_editor::increment_current_command_number ();
                    }
                }
              else if (parser.lexer.end_of_input)
                break;
            }
        }
      catch (octave_interrupt_exception)
        {
          recover_from_exception ();
          octave_stdout << "\n";
          if (quitting_gracefully)
            return exit_status;
        }
      catch (octave_execution_exception)
        {
          recover_from_exception ();
          std::cerr << "error: unhandled execution exception -- trying to return to prompt"
                    << std::endl;
        }
      catch (std::bad_alloc)
        {
          recover_from_exception ();
          std::cerr << "error: out of memory -- trying to return to prompt"
                    << std::endl;
        }
    }
  while (retval == 0);

  octave_stdout << "\n";

  if (retval == EOF)
    retval = 0;

  return retval;
}

// Fix up things before exiting.

static std::list<std::string> octave_atexit_functions;

static void
do_octave_atexit (void)
{
  static bool deja_vu = false;

  OCTAVE_SAFE_CALL (remove_input_event_hook_functions, ());

  while (! octave_atexit_functions.empty ())
    {
      std::string fcn = octave_atexit_functions.front ();

      octave_atexit_functions.pop_front ();

      OCTAVE_SAFE_CALL (reset_error_handler, ());

      OCTAVE_SAFE_CALL (feval, (fcn, octave_value_list (), 0));

      OCTAVE_SAFE_CALL (flush_octave_stdout, ());
    }

  if (! deja_vu)
    {
      deja_vu = true;

      // Process pending events and disasble octave_link event
      // processing with this call.

      octave_link::process_events (true);

      // Do this explicitly so that destructors for mex file objects
      // are called, so that functions registered with mexAtExit are
      // called.
      OCTAVE_SAFE_CALL (clear_mex_functions, ());

      OCTAVE_SAFE_CALL (command_editor::restore_terminal_state, ());

      // FIXME: is this needed?  Can it cause any trouble?
      OCTAVE_SAFE_CALL (raw_mode, (0));

      OCTAVE_SAFE_CALL (octave_history_write_timestamp, ());

      if (! command_history::ignoring_entries ())
        OCTAVE_SAFE_CALL (command_history::clean_up_and_save, ());

      OCTAVE_SAFE_CALL (gh_manager::close_all_figures, ());

      OCTAVE_SAFE_CALL (gtk_manager::unload_all_toolkits, ());

      OCTAVE_SAFE_CALL (close_files, ());

      OCTAVE_SAFE_CALL (cleanup_tmp_files, ());

      OCTAVE_SAFE_CALL (symbol_table::cleanup, ());

      OCTAVE_SAFE_CALL (sysdep_cleanup, ());

      OCTAVE_SAFE_CALL (octave_finalize_hdf5, ());

      OCTAVE_SAFE_CALL (flush_octave_stdout, ());

      if (! quitting_gracefully && interactive)
        {
          octave_stdout << "\n";

          // Yes, we want this to be separate from the call to
          // flush_octave_stdout above.

          OCTAVE_SAFE_CALL (flush_octave_stdout, ());
        }

      // Don't call singleton_cleanup_list::cleanup until we have the
      // problems with registering/unregistering types worked out.  For
      // example, uncomment the following line, then use the make_int
      // function from the examples directory to create an integer
      // object and then exit Octave.  Octave should crash with a
      // segfault when cleaning up the typinfo singleton.  We need some
      // way to force new octave_value_X types that are created in
      // .oct files to be unregistered when the .oct file shared library
      // is unloaded.
      //
      // OCTAVE_SAFE_CALL (singleton_cleanup_list::cleanup, ());

      OCTAVE_SAFE_CALL (octave_chunk_buffer::clear, ());
    }
}

void
clean_up_and_exit (int status, bool safe_to_return)
{
  do_octave_atexit ();

  if (octave_link::exit (status))
    {
      if (safe_to_return)
        return;
      else
        {
          // What should we do here?  We might be called from some
          // location other than the end of octave_execute_interpreter,
          // so it might not be safe to return.

          // We have nothing else to do at this point, and the
          // octave_link::exit function is supposed to take care of
          // exiting for us.  Assume that job won't take more than a
          // day...

          gnulib::sleep (86400);
        }
    }
  else
    {
      if (octave_exit)
        (*octave_exit) (status);
    }
}

DEFUN (quit, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} exit\n\
@deftypefnx {Built-in Function} {} exit (@var{status})\n\
@deftypefnx {Built-in Function} {} quit\n\
@deftypefnx {Built-in Function} {} quit (@var{status})\n\
Exit the current Octave session.\n\
\n\
If the optional integer value @var{status} is supplied, pass that value to\n\
the operating system as Octave's exit status.  The default value is zero.\n\
\n\
When exiting, Octave will attempt to run the m-file @file{finish.m} if it\n\
exists.  User commands to save the workspace or clean up temporary files\n\
may be placed in that file.  Alternatively, another m-file may be scheduled\n\
to run using @code{atexit}.\n\
@seealso{atexit}\n\
@end deftypefn")
{
  octave_value_list retval;

  // Confirm OK to shutdown.  Note: A dynamic function installation similar
  // to overriding polymorphism for which the GUI can install its own "quit"
  // yet call this base "quit" could be nice.  No link would be needed here.
  if (! octave_link::confirm_shutdown ())
    return retval;

  if (! quit_allowed)
    error ("quit: not supported in embedded mode");
  else
    {
      if (args.length () > 0)
        {
          int tmp = args(0).nint_value ();

          if (! error_state)
            exit_status = tmp;
        }

      if (! error_state)
        {
          // Instead of simply calling exit, we simulate an interrupt
          // with a request to exit cleanly so that no matter where the
          // call to quit occurs, we will run the unwind_protect stack,
          // clear the OCTAVE_LOCAL_BUFFER allocations, etc. before
          // exiting.

          quitting_gracefully = true;

          octave_interrupt_state = -1;

          octave_throw_interrupt_exception ();
        }
    }

  return retval;
}

DEFALIAS (exit, quit);

DEFUN (warranty, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} warranty ()\n\
Describe the conditions for copying and distributing Octave.\n\
@end deftypefn")
{
  octave_value_list retval;

  octave_stdout << "\n" << octave_name_version_and_copyright () << "\n\
\n\
GNU Octave is free software; you can redistribute it and/or modify\n\
it under the terms of the GNU General Public License as published by\n\
the Free Software Foundation; either version 3 of the License, or\n\
(at your option) any later version.\n\
\n\
GNU Octave is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
\n";

  return retval;
}

// Execute a shell command.

static int
wait_for_input (int fid)
{
  int retval = -1;

#if defined (HAVE_SELECT)
  if (fid >= 0)
    {
      fd_set set;

      FD_ZERO (&set);
      FD_SET (fid, &set);

      retval = gnulib::select (FD_SETSIZE, &set, 0, 0, 0);
    }
#else
  retval = 1;
#endif

  return retval;
}

static octave_value_list
run_command_and_return_output (const std::string& cmd_str)
{
  octave_value_list retval;
  unwind_protect frame;

  iprocstream *cmd = new iprocstream (cmd_str.c_str ());

  frame.add_delete (cmd);
  frame.add_fcn (octave_child_list::remove, cmd->pid ());

  if (*cmd)
    {
      int fid = cmd->file_number ();

      std::ostringstream output_buf;

      char ch;

      for (;;)
        {
          if (cmd->get (ch))
            output_buf.put (ch);
          else
            {
              if (! cmd->eof () && errno == EAGAIN)
                {
                  cmd->clear ();

                  if (wait_for_input (fid) != 1)
                    break;
                }
              else
                break;
            }
        }

      int cmd_status = cmd->close ();

      if (octave_wait::ifexited (cmd_status))
        cmd_status = octave_wait::exitstatus (cmd_status);
      else
        cmd_status = 127;

      retval(1) = output_buf.str ();
      retval(0) = cmd_status;
    }
  else
    error ("unable to start subprocess for '%s'", cmd_str.c_str ());

  return retval;
}

enum system_exec_type { et_sync, et_async };

DEFUN (system, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} system (\"@var{string}\")\n\
@deftypefnx {Built-in Function} {} system (\"@var{string}\", @var{return_output})\n\
@deftypefnx {Built-in Function} {} system (\"@var{string}\", @var{return_output}, @var{type})\n\
@deftypefnx {Built-in Function} {[@var{status}, @var{output}] =} system (@dots{})\n\
Execute a shell command specified by @var{string}.\n\
\n\
If the optional argument @var{type} is @qcode{\"async\"}, the process is\n\
started in the background and the process ID of the child process is\n\
returned immediately.  Otherwise, the child process is started and Octave\n\
waits until it exits.  If the @var{type} argument is omitted, it defaults to\n\
the value @qcode{\"sync\"}.\n\
\n\
If @var{system} is called with one or more output arguments, or if the\n\
optional argument @var{return_output} is true and the subprocess is started\n\
synchronously, then the output from the command is returned as a variable.\n\
Otherwise, if the subprocess is executed synchronously, its output is sent\n\
to the standard output.  To send the output of a command executed with\n\
@code{system} through the pager, use a command like\n\
\n\
@example\n\
@group\n\
[output, text] = system (\"cmd\");\n\
disp (text);\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
or\n\
\n\
@example\n\
printf (\"%s\\n\", nthargout (2, \"system\", \"cmd\"));\n\
@end example\n\
\n\
The @code{system} function can return two values.  The first is the\n\
exit status of the command and the second is any output from the\n\
command that was written to the standard output stream.  For example,\n\
\n\
@example\n\
[status, output] = system (\"echo foo; exit 2\");\n\
@end example\n\
\n\
@noindent\n\
will set the variable @code{output} to the string @samp{foo}, and the\n\
variable @code{status} to the integer @samp{2}.\n\
\n\
For commands run asynchronously, @var{status} is the process id of the\n\
command shell that is started to run the command.\n\
@seealso{unix, dos}\n\
@end deftypefn")
{
  octave_value_list retval;

  unwind_protect frame;

  int nargin = args.length ();

  if (nargin > 0 && nargin < 4)
    {
      bool return_output = (nargin == 1 && nargout > 1);

      system_exec_type type = et_sync;

      if (nargin == 3)
        {
          if (args(2).is_string ())
            {
              std::string type_str = args(2).string_value ();

              if (type_str == "sync")
                type = et_sync;
              else if (type_str == "async")
                type = et_async;
              else
                {
                  error ("system: TYPE must be \"sync\" or \"async\"");
                  return retval;
                }
            }
          else
            {
              error ("system: TYPE must be a string");
              return retval;
            }
        }

      if (nargin > 1)
        {
          return_output = args(1).is_true ();

          if (error_state)
            {
              error ("system: RETURN_OUTPUT must be boolean value true or false");
              return retval;
            }
        }

      if (return_output && type == et_async)
        {
          error ("system: can't return output from commands run asynchronously");
          return retval;
        }

      std::string cmd_str = args(0).string_value ();

      if (! error_state)
        {
#if defined (__WIN32__) && ! defined (__CYGWIN__)
          // Work around weird double-quote handling on Windows systems.
          if (type == et_sync)
            cmd_str = "\"" + cmd_str + "\"";
#endif

          if (type == et_async)
            {
              // FIXME: maybe this should go in sysdep.cc?
#ifdef HAVE_FORK
              pid_t pid = fork ();

              if (pid < 0)
                error ("system: fork failed -- can't create child process");
              else if (pid == 0)
                {
                  // FIXME: should probably replace this
                  // call with something portable.

                  execl (SHELL_PATH, "sh", "-c", cmd_str.c_str (),
                         static_cast<void *> (0));

                  panic_impossible ();
                }
              else
                retval(0) = pid;
#elif defined (__WIN32__)
              STARTUPINFO si;
              PROCESS_INFORMATION pi;
              ZeroMemory (&si, sizeof (si));
              ZeroMemory (&pi, sizeof (pi));
              OCTAVE_LOCAL_BUFFER (char, xcmd_str, cmd_str.length ()+1);
              strcpy (xcmd_str, cmd_str.c_str ());

              if (! CreateProcess (0, xcmd_str, 0, 0, FALSE, 0, 0, 0, &si, &pi))
                error ("system: CreateProcess failed -- can't create child process");
              else
                {
                  retval(0) = pi.dwProcessId;
                  CloseHandle (pi.hProcess);
                  CloseHandle (pi.hThread);
                }
#else
              error ("asynchronous system calls are not supported");
#endif
            }
          else if (return_output)
            retval = run_command_and_return_output (cmd_str);
          else
            {
              int status = system (cmd_str.c_str ());

              // The value in status is as returned by waitpid.  If
              // the process exited normally, extract the actual exit
              // status of the command.  Otherwise, return 127 as a
              // failure code.

              if (octave_wait::ifexited (status))
                status = octave_wait::exitstatus (status);

              retval(0) = status;
            }
        }
      else
        error ("system: expecting string as first argument");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! cmd = ls_command ();
%! [status, output] = system (cmd);
%! assert (status, 0);
%! assert (ischar (output));
%! assert (! isempty (output));

%!error system ()
%!error system (1, 2, 3)
*/

void
octave_add_atexit_function (const std::string& fname)
{
  octave_atexit_functions.push_front (fname);
}

bool
octave_remove_atexit_function (const std::string& fname)
{
  bool found = false;

  for (std::list<std::string>::iterator p = octave_atexit_functions.begin ();
       p != octave_atexit_functions.end (); p++)
    {
      if (*p == fname)
        {
          octave_atexit_functions.erase (p);
          found = true;
          break;
        }
    }

  return found;
}


DEFUN (atexit, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} atexit (@var{fcn})\n\
@deftypefnx {Built-in Function} {} atexit (@var{fcn}, @var{flag})\n\
Register a function to be called when Octave exits.\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
function last_words ()\n\
  disp (\"Bye bye\");\n\
endfunction\n\
atexit (\"last_words\");\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
will print the message @qcode{\"Bye bye\"} when Octave exits.\n\
\n\
The additional argument @var{flag} will register or unregister @var{fcn}\n\
from the list of functions to be called when Octave exits.  If @var{flag} is\n\
true, the function is registered, and if @var{flag} is false, it is\n\
unregistered.  For example, after registering the function @code{last_words}\n\
above,\n\
\n\
@example\n\
atexit (\"last_words\", false);\n\
@end example\n\
\n\
@noindent\n\
will remove the function from the list and Octave will not call\n\
@code{last_words} when it exits.\n\
\n\
Note that @code{atexit} only removes the first occurrence of a function\n\
from the list, so if a function was placed in the list multiple times with\n\
@code{atexit}, it must also be removed from the list multiple times.\n\
@seealso{quit}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      if (args(0).is_string ())
        {
          std::string arg = args(0).string_value ();

          bool add_mode = true;

          if (nargin == 2)
            {
              add_mode = args(1).bool_value ();

              if (error_state)
                error ("atexit: FLAG argument must be a logical value");
            }

          if (! error_state)
            {
              if (add_mode)
                octave_add_atexit_function (arg);
              else
                {
                  bool found = octave_remove_atexit_function (arg);

                  if (nargout > 0)
                    retval(0) = found;
                }
            }
        }
      else
        error ("atexit: FCN argument must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (octave_config_info, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} octave_config_info ()\n\
@deftypefnx {Built-in Function} {} octave_config_info (@var{option})\n\
Return a structure containing configuration and installation information for\n\
Octave.\n\
\n\
If @var{option} is a string, return the configuration information for the\n\
specified option.\n\
\n\
@seealso{computer}\n\
@end deftypefn")
{
  octave_value retval;

#if defined (ENABLE_DYNAMIC_LINKING)
  bool octave_supports_dynamic_linking = true;
#else
  bool octave_supports_dynamic_linking = false;
#endif

  static bool initialized = false;
  static octave_scalar_map m;

  struct conf_info_struct
  {
    bool subst_home;
    const char *key;
    const char *val;
  };

  static const conf_info_struct conf_info[] =
  {
    { false, "ALL_CFLAGS", OCTAVE_CONF_ALL_CFLAGS },
    { false, "ALL_CXXFLAGS", OCTAVE_CONF_ALL_CXXFLAGS },
    { false, "ALL_FFLAGS", OCTAVE_CONF_ALL_FFLAGS },
    { false, "ALL_LDFLAGS", OCTAVE_CONF_ALL_LDFLAGS },
    { false, "AMD_CPPFLAGS", OCTAVE_CONF_AMD_CPPFLAGS },
    { false, "AMD_LDFLAGS", OCTAVE_CONF_AMD_LDFLAGS },
    { false, "AMD_LIBS", OCTAVE_CONF_AMD_LIBS },
    { false, "AR", OCTAVE_CONF_AR },
    { false, "ARFLAGS", OCTAVE_CONF_ARFLAGS },
    { false, "ARPACK_CPPFLAGS", OCTAVE_CONF_ARPACK_CPPFLAGS },
    { false, "ARPACK_LDFLAGS", OCTAVE_CONF_ARPACK_LDFLAGS },
    { false, "ARPACK_LIBS", OCTAVE_CONF_ARPACK_LIBS },
    { false, "BLAS_LIBS", OCTAVE_CONF_BLAS_LIBS },
    { false, "CAMD_CPPFLAGS", OCTAVE_CONF_CAMD_CPPFLAGS },
    { false, "CAMD_LDFLAGS", OCTAVE_CONF_CAMD_LDFLAGS },
    { false, "CAMD_LIBS", OCTAVE_CONF_CAMD_LIBS },
    { false, "CARBON_LIBS", OCTAVE_CONF_CARBON_LIBS },
    { false, "CC", OCTAVE_CONF_CC },
    { false, "CCOLAMD_CPPFLAGS", OCTAVE_CONF_CCOLAMD_CPPFLAGS },
    { false, "CCOLAMD_LDFLAGS", OCTAVE_CONF_CCOLAMD_LDFLAGS },
    { false, "CCOLAMD_LIBS", OCTAVE_CONF_CCOLAMD_LIBS },
    { false, "CFLAGS", OCTAVE_CONF_CFLAGS },
    { false, "CHOLMOD_CPPFLAGS", OCTAVE_CONF_CHOLMOD_CPPFLAGS },
    { false, "CHOLMOD_LDFLAGS", OCTAVE_CONF_CHOLMOD_LDFLAGS },
    { false, "CHOLMOD_LIBS", OCTAVE_CONF_CHOLMOD_LIBS },
    { false, "COLAMD_CPPFLAGS", OCTAVE_CONF_COLAMD_CPPFLAGS },
    { false, "COLAMD_LDFLAGS", OCTAVE_CONF_COLAMD_LDFLAGS },
    { false, "COLAMD_LIBS", OCTAVE_CONF_COLAMD_LIBS },
    { false, "CPICFLAG", OCTAVE_CONF_CPICFLAG },
    { false, "CPPFLAGS", OCTAVE_CONF_CPPFLAGS },
    { false, "CURL_CPPFLAGS", OCTAVE_CONF_CURL_CPPFLAGS },
    { false, "CURL_LDFLAGS", OCTAVE_CONF_CURL_LDFLAGS },
    { false, "CURL_LIBS", OCTAVE_CONF_CURL_LIBS },
    { false, "CXSPARSE_CPPFLAGS", OCTAVE_CONF_CXSPARSE_CPPFLAGS },
    { false, "CXSPARSE_LDFLAGS", OCTAVE_CONF_CXSPARSE_LDFLAGS },
    { false, "CXSPARSE_LIBS", OCTAVE_CONF_CXSPARSE_LIBS },
    { false, "CXX", OCTAVE_CONF_CXX },
    { false, "CXXCPP", OCTAVE_CONF_CXXCPP },
    { false, "CXXFLAGS", OCTAVE_CONF_CXXFLAGS },
    { false, "CXXPICFLAG", OCTAVE_CONF_CXXPICFLAG },
    { false, "DEFAULT_PAGER", OCTAVE_DEFAULT_PAGER },
    { false, "DEFS", OCTAVE_CONF_DEFS },
    { false, "DL_LD", OCTAVE_CONF_DL_LD },
    { false, "DL_LDFLAGS", OCTAVE_CONF_DL_LDFLAGS },
    { false, "DL_LIBS", OCTAVE_CONF_DL_LIBS },
    { false, "GCC_VERSION", OCTAVE_CONF_GCC_VERSION },
    { false, "GXX_VERSION", OCTAVE_CONF_GXX_VERSION },
    { false, "ENABLE_DYNAMIC_LINKING", OCTAVE_CONF_ENABLE_DYNAMIC_LINKING },
    { false, "EXEEXT", OCTAVE_CONF_EXEEXT },
    { false, "F77", OCTAVE_CONF_F77 },
    { false, "F77_FLOAT_STORE_FLAG", OCTAVE_CONF_F77_FLOAT_STORE_FLAG },
    { false, "F77_INTEGER_8_FLAG", OCTAVE_CONF_F77_INTEGER_8_FLAG },
    { false, "FFLAGS", OCTAVE_CONF_FFLAGS },
    { false, "FFTW3_CPPFLAGS", OCTAVE_CONF_FFTW3_CPPFLAGS },
    { false, "FFTW3_LDFLAGS", OCTAVE_CONF_FFTW3_LDFLAGS },
    { false, "FFTW3_LIBS", OCTAVE_CONF_FFTW3_LIBS },
    { false, "FFTW3F_CPPFLAGS", OCTAVE_CONF_FFTW3F_CPPFLAGS },
    { false, "FFTW3F_LDFLAGS", OCTAVE_CONF_FFTW3F_LDFLAGS },
    { false, "FFTW3F_LIBS", OCTAVE_CONF_FFTW3F_LIBS },
    { false, "FLIBS", OCTAVE_CONF_FLIBS },
    { false, "FLTK_CPPFLAGS", OCTAVE_CONF_FLTK_CPPFLAGS },
    { false, "FLTK_LDFLAGS", OCTAVE_CONF_FLTK_LDFLAGS },
    { false, "FLTK_LIBS", OCTAVE_CONF_FLTK_LIBS },
    { false, "FONTCONFIG_CPPFLAGS", OCTAVE_CONF_FONTCONFIG_CPPFLAGS },
    { false, "FONTCONFIG_LIBS", OCTAVE_CONF_FONTCONFIG_LIBS },
    { false, "FPICFLAG", OCTAVE_CONF_FPICFLAG },
    { false, "FT2_CPPFLAGS", OCTAVE_CONF_FT2_CPPFLAGS },
    { false, "FT2_LIBS", OCTAVE_CONF_FT2_LIBS },
    { false, "GLPK_CPPFLAGS", OCTAVE_CONF_GLPK_CPPFLAGS },
    { false, "GLPK_LDFLAGS", OCTAVE_CONF_GLPK_LDFLAGS },
    { false, "GLPK_LIBS", OCTAVE_CONF_GLPK_LIBS },
    { false, "GNUPLOT", OCTAVE_CONF_GNUPLOT },
    { false, "HDF5_CPPFLAGS", OCTAVE_CONF_HDF5_CPPFLAGS },
    { false, "HDF5_LDFLAGS", OCTAVE_CONF_HDF5_LDFLAGS },
    { false, "HDF5_LIBS", OCTAVE_CONF_HDF5_LIBS },
    { false, "LAPACK_LIBS", OCTAVE_CONF_LAPACK_LIBS },
    { false, "LDFLAGS", OCTAVE_CONF_LDFLAGS },
    { false, "LD_CXX", OCTAVE_CONF_LD_CXX },
    { false, "LD_STATIC_FLAG", OCTAVE_CONF_LD_STATIC_FLAG },
    { false, "LEX", OCTAVE_CONF_LEX },
    { false, "LEXLIB", OCTAVE_CONF_LEXLIB },
    { false, "LFLAGS", OCTAVE_CONF_LFLAGS },
    { false, "LIBEXT", OCTAVE_CONF_LIBEXT },
    { false, "LIBFLAGS", OCTAVE_CONF_LIBFLAGS },
    { false, "LIBOCTAVE", OCTAVE_CONF_LIBOCTAVE },
    { false, "LIBOCTINTERP", OCTAVE_CONF_LIBOCTINTERP },
    { false, "LIBS", OCTAVE_CONF_LIBS },
    { false, "LLVM_CPPFLAGS", OCTAVE_CONF_LLVM_CPPFLAGS },
    { false, "LLVM_LDFLAGS", OCTAVE_CONF_LLVM_LDFLAGS },
    { false, "LLVM_LIBS", OCTAVE_CONF_LLVM_LIBS },
    { false, "LN_S", OCTAVE_CONF_LN_S },
    { false, "MAGICK_CPPFLAGS", OCTAVE_CONF_MAGICK_CPPFLAGS },
    { false, "MAGICK_LDFLAGS", OCTAVE_CONF_MAGICK_LDFLAGS },
    { false, "MAGICK_LIBS", OCTAVE_CONF_MAGICK_LIBS },
    { false, "MKOCTFILE_DL_LDFLAGS", OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS },
    { false, "OCTAVE_LINK_DEPS", OCTAVE_CONF_OCTAVE_LINK_DEPS },
    { false, "OCTAVE_LINK_OPTS", OCTAVE_CONF_OCTAVE_LINK_OPTS },
    { false, "OCT_LINK_DEPS", OCTAVE_CONF_OCT_LINK_DEPS },
    { false, "OCT_LINK_OPTS", OCTAVE_CONF_OCT_LINK_OPTS },
    { false, "OPENGL_LIBS", OCTAVE_CONF_OPENGL_LIBS },
    { false, "OSMESA_CPPFLAGS", OCTAVE_CONF_OSMESA_CPPFLAGS },
    { false, "OSMESA_LDFLAGS", OCTAVE_CONF_OSMESA_LDFLAGS },
    { false, "OSMESA_LIBS", OCTAVE_CONF_OSMESA_LIBS },
    { false, "PCRE_CPPFLAGS", OCTAVE_CONF_PCRE_CPPFLAGS },
    { false, "PCRE_LIBS", OCTAVE_CONF_PCRE_LIBS },
    { false, "PTHREAD_CFLAGS", OCTAVE_CONF_PTHREAD_CFLAGS },
    { false, "PTHREAD_LIBS", OCTAVE_CONF_PTHREAD_LIBS },
    { false, "QHULL_CPPFLAGS", OCTAVE_CONF_QHULL_CPPFLAGS },
    { false, "QHULL_LDFLAGS", OCTAVE_CONF_QHULL_LDFLAGS },
    { false, "QHULL_LIBS", OCTAVE_CONF_QHULL_LIBS },
    { false, "QRUPDATE_CPPFLAGS", OCTAVE_CONF_QRUPDATE_CPPFLAGS },
    { false, "QRUPDATE_LDFLAGS", OCTAVE_CONF_QRUPDATE_LDFLAGS },
    { false, "QRUPDATE_LIBS", OCTAVE_CONF_QRUPDATE_LIBS },
    { false, "QT_CPPFLAGS", OCTAVE_CONF_QT_CPPFLAGS },
    { false, "QT_LDFLAGS", OCTAVE_CONF_QT_LDFLAGS },
    { false, "QT_LIBS", OCTAVE_CONF_QT_LIBS },
    { false, "RANLIB", OCTAVE_CONF_RANLIB },
    { false, "RDYNAMIC_FLAG", OCTAVE_CONF_RDYNAMIC_FLAG },
    { false, "READLINE_LIBS", OCTAVE_CONF_READLINE_LIBS },
    { false, "SED", OCTAVE_CONF_SED },
    { false, "SHARED_LIBS", OCTAVE_CONF_SHARED_LIBS },
    { false, "SHLEXT", OCTAVE_CONF_SHLEXT },
    { false, "SHLEXT_VER", OCTAVE_CONF_SHLEXT_VER },
    { false, "SH_LD", OCTAVE_CONF_SH_LD },
    { false, "SH_LDFLAGS", OCTAVE_CONF_SH_LDFLAGS },
    { false, "SONAME_FLAGS", OCTAVE_CONF_SONAME_FLAGS },
    { false, "STATIC_LIBS", OCTAVE_CONF_STATIC_LIBS },
    { false, "TERM_LIBS", OCTAVE_CONF_TERM_LIBS },
    { false, "UMFPACK_CPPFLAGS", OCTAVE_CONF_UMFPACK_CPPFLAGS },
    { false, "UMFPACK_LDFLAGS", OCTAVE_CONF_UMFPACK_LDFLAGS },
    { false, "UMFPACK_LIBS", OCTAVE_CONF_UMFPACK_LIBS },
    { false, "USE_64_BIT_IDX_T", OCTAVE_CONF_USE_64_BIT_IDX_T },
    { false, "WARN_CFLAGS", OCTAVE_CONF_WARN_CFLAGS },
    { false, "WARN_CXXFLAGS", OCTAVE_CONF_WARN_CXXFLAGS },
    { false, "X11_INCFLAGS", OCTAVE_CONF_X11_INCFLAGS },
    { false, "X11_LIBS", OCTAVE_CONF_X11_LIBS },
    { false, "XTRA_CFLAGS", OCTAVE_CONF_XTRA_CFLAGS },
    { false, "XTRA_CXXFLAGS", OCTAVE_CONF_XTRA_CXXFLAGS },
    { false, "YACC", OCTAVE_CONF_YACC },
    { false, "YFLAGS", OCTAVE_CONF_YFLAGS },
    { false, "Z_CPPFLAGS", OCTAVE_CONF_Z_CPPFLAGS },
    { false, "Z_LDFLAGS", OCTAVE_CONF_Z_LDFLAGS },
    { false, "Z_LIBS", OCTAVE_CONF_Z_LIBS },
    { false, "api_version", OCTAVE_API_VERSION },
    { true, "archlibdir", OCTAVE_ARCHLIBDIR },
    { true, "bindir", OCTAVE_BINDIR },
    { false, "canonical_host_type", OCTAVE_CANONICAL_HOST_TYPE },
    { false, "config_opts", OCTAVE_CONF_config_opts },
    { true, "datadir", OCTAVE_DATADIR },
    { true, "datarootdir", OCTAVE_DATAROOTDIR },
    { true, "exec_prefix", OCTAVE_EXEC_PREFIX },
    { true, "fcnfiledir", OCTAVE_FCNFILEDIR },
    { true, "imagedir", OCTAVE_IMAGEDIR },
    { true, "includedir", OCTAVE_INCLUDEDIR },
    { true, "infodir", OCTAVE_INFODIR },
    { true, "infofile", OCTAVE_INFOFILE },
    { true, "libdir", OCTAVE_LIBDIR },
    { true, "libexecdir", OCTAVE_LIBEXECDIR },
    { true, "localapiarchlibdir", OCTAVE_LOCALAPIARCHLIBDIR },
    { true, "localapifcnfiledir", OCTAVE_LOCALAPIFCNFILEDIR },
    { true, "localapioctfiledir", OCTAVE_LOCALAPIOCTFILEDIR },
    { true, "localarchlibdir", OCTAVE_LOCALARCHLIBDIR },
    { true, "localfcnfiledir", OCTAVE_LOCALFCNFILEDIR },
    { true, "localoctfiledir", OCTAVE_LOCALOCTFILEDIR },
    { true, "localstartupfiledir", OCTAVE_LOCALSTARTUPFILEDIR },
    { true, "localverarchlibdir", OCTAVE_LOCALVERARCHLIBDIR },
    { true, "localverfcnfiledir", OCTAVE_LOCALVERFCNFILEDIR },
    { true, "localveroctfiledir", OCTAVE_LOCALVEROCTFILEDIR },
    { true, "man1dir", OCTAVE_MAN1DIR },
    { false, "man1ext", OCTAVE_MAN1EXT },
    { true, "mandir", OCTAVE_MANDIR },
    { true, "octdatadir", OCTAVE_OCTDATADIR },
    { true, "octfiledir", OCTAVE_OCTFILEDIR },
    { true, "octetcdir", OCTAVE_OCTETCDIR },
    { true, "octincludedir", OCTAVE_OCTINCLUDEDIR },
    { true, "octlibdir", OCTAVE_OCTLIBDIR },
    { true, "octtestsdir", OCTAVE_OCTTESTSDIR },
    { true, "prefix", OCTAVE_PREFIX },
    { true, "startupfiledir", OCTAVE_STARTUPFILEDIR },
    { false, "version", OCTAVE_VERSION },
    { false, 0, 0 }
  };

  if (! initialized)
    {
      m.assign ("dld", octave_value (octave_supports_dynamic_linking));

      oct_mach_info::float_format ff = oct_mach_info::native_float_format ();
      m.assign ("float_format",
                octave_value (oct_mach_info::float_format_as_string (ff)));

      m.assign ("words_big_endian",
                octave_value (oct_mach_info::words_big_endian ()));

      m.assign ("words_little_endian",
                octave_value (oct_mach_info::words_little_endian ()));

      m.assign ("features", octave_value (octave_config_features ()));

      int i = 0;

      while (true)
        {
          const conf_info_struct& elt = conf_info[i++];

          const char *key = elt.key;

          if (key)
            {
              if (elt.subst_home)
                m.assign (key, subst_octave_home (elt.val));
              else
                m.assign (key, elt.val);
            }
          else
            break;
        }

      bool unix_system = true;
      bool mac_system = false;
      bool windows_system = false;

#if defined (WIN32)
      windows_system = true;
#if !defined (__CYGWIN__)
      unix_system = false;
#endif
#endif

#if defined (OCTAVE_USE_OS_X_API)
      mac_system = true;
#endif

      m.assign ("unix", octave_value (unix_system));
      m.assign ("mac", octave_value (mac_system));
      m.assign ("windows", octave_value (windows_system));

      initialized = true;
    }

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string arg = args(0).string_value ();

      if (! error_state)
        {
          if (m.isfield (arg))
            {
              Cell c = m.contents (arg);

              if (c.is_empty ())
                error ("octave_config_info: no info for '%s'", arg.c_str ());
              else
                retval = c(0);
            }
          else
            error ("octave_config_info: invalid parameter '%s'", arg.c_str ());
        }
    }
  else if (nargin == 0)
    retval = m;
  else
    print_usage ();

  return retval;
}

/*
%!assert (ischar (octave_config_info ("version")))
%!test
%! x = octave_config_info ();
%! assert (isstruct (x));
%! assert (! isempty (x));

%!error octave_config_info (1, 2)
*/

#if defined (__GNUG__) && defined (DEBUG_NEW_DELETE)

int debug_new_delete = 0;

typedef void (*vfp)(void);
extern vfp __new_handler;

void *
__builtin_new (size_t sz)
{
  void *p;

  /* malloc (0) is unpredictable; avoid it.  */
  if (sz == 0)
    sz = 1;
  p = gnulib::malloc (sz);
  while (p == 0)
    {
      (*__new_handler) ();
      p = gnulib::malloc (sz);
    }

  if (debug_new_delete)
    std::cerr << "__builtin_new: " << p << std::endl;

  return p;
}

void
__builtin_delete (void *ptr)
{
  if (debug_new_delete)
    std::cerr << "__builtin_delete: " << ptr << std::endl;

  if (ptr)
    free (ptr);
}

#endif
