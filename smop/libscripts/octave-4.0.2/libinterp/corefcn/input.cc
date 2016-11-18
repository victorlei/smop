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

// Get command input interactively or from files.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cassert>

#include <iostream>
#include <sstream>
#include <string>

#include <sys/types.h>
#include <unistd.h>

#include "cmd-edit.h"
#include "file-ops.h"
#include "quit.h"
#include "str-vec.h"

#include "debug.h"
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "hook-fcn.h"
#include "input.h"
#include "lex.h"
#include "load-path.h"
#include "octave-link.h"
#include "oct-map.h"
#include "oct-hist.h"
#include "toplev.h"
#include "octave-link.h"
#include "oct-obj.h"
#include "ov-fcn-handle.h"
#include "pager.h"
#include "parse.h"
#include "pathlen.h"
#include "pt.h"
#include "pt-const.h"
#include "pt-eval.h"
#include "pt-stmt.h"
#include "sighandlers.h"
#include "symtab.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Primary prompt string.
static std::string VPS1;

// Secondary prompt string.
static std::string VPS2;

// String printed before echoed input (enabled by --echo-input).
std::string VPS4 = "+ ";

// Echo commands as they are executed?
//
//   1  ==>  echo commands read from script files
//   2  ==>  echo commands from functions
//   4  ==>  echo commands read from command line
//
// more than one state can be active at once.
int Vecho_executing_commands = ECHO_OFF;

// The time we last printed a prompt.
octave_time Vlast_prompt_time = 0.0;

// Character to append after successful command-line completion attempts.
static char Vcompletion_append_char = ' ';

// TRUE means this is an interactive shell (either forced or not)
bool interactive = false;

// TRUE means the user forced this shell to be interactive (-i).
// FALSE means the shell would be interactive, independent of user settings.
bool forced_interactive = false;

// TRUE after a call to completion_matches.
bool octave_completion_matches_called = false;

// TRUE if the plotting system has requested a call to drawnow at
// the next user prompt.
bool Vdrawnow_requested = false;

// TRUE if we are in debugging mode.
bool Vdebugging = false;

// If we are in debugging mode, this is the last command entered, so
// that we can repeat the previous command if the user just types RET.
static std::string last_debugging_command = "\n";

// TRUE if we are running in the Emacs GUD mode.
static bool Vgud_mode = false;

// The filemarker used to separate filenames from subfunction names
char Vfilemarker = '>';

static hook_function_list input_event_hook_functions;

// For octave_quit.
void
remove_input_event_hook_functions (void)
{
  input_event_hook_functions.clear ();
}

void
set_default_prompts (void)
{
  // Use literal "octave" instead of "\\s" to avoid setting the prompt
  // to "octave.exe" or "octave-gui", etc.

  VPS1 = "octave:\\#> ";
  VPS2 = "> ";
  VPS4 = "+ ";

  octave_link::set_default_prompts (VPS1, VPS2, VPS4);
}

void
octave_base_reader::do_input_echo (const std::string& input_string) const
{
  int do_echo = reading_script_file ()
    ? (Vecho_executing_commands & ECHO_SCRIPTS)
    : (Vecho_executing_commands & ECHO_CMD_LINE) && ! forced_interactive;

  if (do_echo)
    {
      if (forced_interactive)
        {
          if (pflag > 0)
            octave_stdout << command_editor::decode_prompt_string (VPS1);
          else
            octave_stdout << command_editor::decode_prompt_string (VPS2);
        }
      else
        octave_stdout << command_editor::decode_prompt_string (VPS4);

      if (! input_string.empty ())
        {
          octave_stdout << input_string;

          if (input_string[input_string.length () - 1] != '\n')
            octave_stdout << "\n";
        }
    }
}

static std::string
gnu_readline (const std::string& s, bool& eof)
{
  octave_quit ();

  eof = false;

  std::string retval = command_editor::readline (s, eof);

  if (! eof && retval.empty ())
    retval = "\n";

  return retval;
}

static inline std::string
interactive_input (const std::string& s, bool& eof)
{
  Vlast_prompt_time.stamp ();

  if (Vdrawnow_requested && interactive)
    {
      feval ("drawnow");

      flush_octave_stdout ();

      // We set Vdrawnow_requested to false even if there is an error
      // in drawnow so that the error doesn't reappear at every prompt.

      Vdrawnow_requested = false;

      if (error_state)
        return "\n";
    }

  return gnu_readline (s, eof);
}

std::string
octave_base_reader::octave_gets (bool& eof)
{
  octave_quit ();

  eof = false;

  std::string retval;

  // Process pre input event hook function prior to flushing output and
  // printing the prompt.

  if (interactive)
    {
      if (! Vdebugging)
        octave_link::exit_debugger_event ();

      octave_link::pre_input_event ();

      octave_link::set_workspace ();
    }

  bool history_skip_auto_repeated_debugging_command = false;

  std::string ps = (pflag > 0) ? VPS1 : VPS2;

  std::string prompt = command_editor::decode_prompt_string (ps);

  pipe_handler_error_count = 0;

  flush_octave_stdout ();

  octave_pager_stream::reset ();
  octave_diary_stream::reset ();

  octave_diary << prompt;

  retval = interactive_input (prompt, eof);

  // There is no need to update the load_path cache if there is no
  // user input.
  if (retval != "\n"
      && retval.find_first_not_of (" \t\n\r") != std::string::npos)
    {
      load_path::update ();

      if (Vdebugging)
        last_debugging_command = retval;
      else
        last_debugging_command = "\n";
    }
  else if (Vdebugging)
    {
      retval = last_debugging_command;
      history_skip_auto_repeated_debugging_command = true;
    }

  if (retval != "\n")
    {
      if (! history_skip_auto_repeated_debugging_command)
        {
          if (command_history::add (retval))
            octave_link::append_history (retval);
        }

      octave_diary << retval;

      if (retval[retval.length () - 1] != '\n')
        octave_diary << "\n";

      do_input_echo (retval);
    }
  else
    octave_diary << "\n";

  // Process post input event hook function after the internal history
  // list has been updated.

  if (interactive)
    octave_link::post_input_event ();

  return retval;
}

bool
octave_base_reader::reading_fcn_file (void) const
{
  return lexer ? lexer->reading_fcn_file : false;
}

bool
octave_base_reader::reading_classdef_file (void) const
{
  return lexer ? lexer->reading_classdef_file : false;
}

bool
octave_base_reader::reading_script_file (void) const
{
  return lexer ? lexer->reading_script_file : false;
}

// Fix things up so that input can come from the standard input.  This
// may need to become much more complicated, which is why it's in a
// separate function.

FILE *
get_input_from_stdin (void)
{
  command_editor::set_input_stream (stdin);
  return command_editor::get_input_stream ();
}

// FIXME: make this generate file names when appropriate.

static string_vector
generate_possible_completions (const std::string& text, std::string& prefix,
                               std::string& hint)
{
  string_vector names;

  prefix = "";

  if (looks_like_struct (text))
    names = generate_struct_completions (text, prefix, hint);
  else
    names = make_name_list ();

  // Sort and remove duplicates.

  names.sort (true);

  return names;
}

static bool
is_completing_dirfns (void)
{
  static std::string dirfns_commands[] = {"cd", "ls"};
  static const size_t dirfns_commands_length = 2;

  bool retval = false;

  std::string line = command_editor::get_line_buffer ();

  for (size_t i = 0; i < dirfns_commands_length; i++)
    {
      int index = line.find (dirfns_commands[i] + " ");

      if (index == 0)
        {
          retval = true;
          break;
        }
    }

  return retval;
}

static std::string
generate_completion (const std::string& text, int state)
{
  std::string retval;

  static std::string prefix;
  static std::string hint;

  static size_t hint_len = 0;

  static int list_index = 0;
  static int name_list_len = 0;
  static int name_list_total_len = 0;
  static string_vector name_list;
  static string_vector file_name_list;

  static int matches = 0;

  if (state == 0)
    {
      list_index = 0;

      prefix = "";

      hint = text;

      // No reason to display symbols while completing a
      // file/directory operation.

      if (is_completing_dirfns ())
        name_list = string_vector ();
      else
        name_list = generate_possible_completions (text, prefix, hint);

      name_list_len = name_list.length ();

      file_name_list = command_editor::generate_filename_completions (text);

      name_list.append (file_name_list);

      name_list_total_len = name_list.length ();

      hint_len = hint.length ();

      matches = 0;

      for (int i = 0; i < name_list_len; i++)
        if (hint == name_list[i].substr (0, hint_len))
          matches++;
    }

  if (name_list_total_len > 0 && matches > 0)
    {
      while (list_index < name_list_total_len)
        {
          std::string name = name_list[list_index];

          list_index++;

          if (hint == name.substr (0, hint_len))
            {
              if (list_index <= name_list_len && ! prefix.empty ())
                retval = prefix + "." + name;
              else
                retval = name;

              // FIXME: looks_like_struct is broken for now,
              //        so it always returns false.

              if (matches == 1 && looks_like_struct (retval))
                {
                  // Don't append anything, since we don't know
                  // whether it should be '(' or '.'.

                  command_editor::set_completion_append_character ('\0');
                }
              else
                command_editor::set_completion_append_character
                  (Vcompletion_append_char);

              break;
            }
        }
    }

  return retval;
}

static std::string
quoting_filename (const std::string &text, int, char quote)
{
  if (quote)
    return text;
  else
    return (std::string ("'") + text);
}

void
initialize_command_input (void)
{
  // If we are using readline, this allows conditional parsing of the
  // .inputrc file.

  command_editor::set_name ("Octave");

  // FIXME: this needs to include a comma too, but that
  // causes trouble for the new struct element completion code.

  static const char *s = "\t\n !\"\'*+-/:;<=>(){}[\\]^`~";

  command_editor::set_basic_word_break_characters (s);

  command_editor::set_completer_word_break_characters (s);

  command_editor::set_basic_quote_characters ("\"");

  command_editor::set_filename_quote_characters (" \t\n\\\"'@<>=;|&()#$`?*[!:{");
  command_editor::set_completer_quote_characters ("'\"");

  command_editor::set_completion_function (generate_completion);

  command_editor::set_quoting_function (quoting_filename);
}

static void
execute_in_debugger_handler (const std::pair<std::string, int>& arg)
{
  octave_link::execute_in_debugger_event (arg.first, arg.second);
}

static void
get_debug_input (const std::string& prompt)
{
  unwind_protect frame;

  bool silent = tree_evaluator::quiet_breakpoint_flag;
  tree_evaluator::quiet_breakpoint_flag = false;

  octave_user_code *caller = octave_call_stack::caller_user_code ();
  std::string nm;
  int curr_debug_line;

  bool have_file = false;

  if (caller)
    {
      nm = caller->fcn_file_name ();

      if (nm.empty ())
        nm = caller->name ();
      else
        have_file = true;

      curr_debug_line = octave_call_stack::caller_user_code_line ();
    }
  else
    curr_debug_line = octave_call_stack::current_line ();

  std::ostringstream buf;

  if (! nm.empty ())
    {
      if (Vgud_mode)
        {
          static char ctrl_z = 'Z' & 0x1f;

          buf << ctrl_z << ctrl_z << nm << ":" << curr_debug_line;
        }
      else
        {
          // FIXME: we should come up with a clean way to detect
          // that we are stopped on the no-op command that marks the
          // end of a function or script.

          if (! silent)
            {
              buf << "stopped in " << nm;

              if (curr_debug_line > 0)
                buf << " at line " << curr_debug_line;
            }

          if (have_file)
            {
              octave_link::enter_debugger_event (nm, curr_debug_line);

              octave_link::set_workspace ();

              frame.add_fcn (execute_in_debugger_handler,
                             std::pair<std::string, int> (nm, curr_debug_line));

              if (! silent)
                {
                  std::string line_buf
                    = get_file_line (nm, curr_debug_line);

                  if (! line_buf.empty ())
                    buf << "\n" << curr_debug_line << ": " << line_buf;
                }
            }
        }
    }

  if (silent)
    command_editor::erase_empty_line (true);

  std::string msg = buf.str ();

  if (! msg.empty ())
    std::cerr << msg << std::endl;

  frame.protect_var (VPS1);
  VPS1 = prompt;

  if (! interactive)
    {
      frame.protect_var (interactive);
      interactive = true;
      frame.protect_var (forced_interactive);
      forced_interactive = true;
    }

  octave_parser curr_parser;

  while (Vdebugging)
    {
      unwind_protect middle_frame;

      reset_error_handler ();

      curr_parser.reset ();

      int retval = curr_parser.run ();

      if (command_editor::interrupt (false))
        break;
      else
        {
          if (retval == 0 && curr_parser.stmt_list)
            {
              curr_parser.stmt_list->accept (*current_evaluator);

              if (octave_completion_matches_called)
                octave_completion_matches_called = false;
            }

          octave_quit ();
        }
    }
}

const std::string octave_base_reader::in_src ("invalid");

const std::string octave_terminal_reader::in_src ("terminal");

std::string
octave_terminal_reader::get_input (bool& eof)
{
  octave_quit ();

  eof = false;

  return octave_gets (eof);
}

const std::string octave_file_reader::in_src ("file");

std::string
octave_file_reader::get_input (bool& eof)
{
  octave_quit ();

  eof = false;

  return octave_fgets (file, eof);
}

const std::string octave_eval_string_reader::in_src ("eval_string");

std::string
octave_eval_string_reader::get_input (bool& eof)
{
  octave_quit ();

  eof = false;

  std::string retval;

  retval = eval_string;

  // Clear the eval string so that the next call will return
  // an empty character string with EOF = true.
  eval_string = "";

  if (retval.empty ())
    eof = true;

  return retval;
}

// If the user simply hits return, this will produce an empty matrix.

static octave_value_list
get_user_input (const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  int nargin = args.length ();

  int read_as_string = 0;

  if (nargin == 2)
    read_as_string++;

  std::string prompt = args(0).string_value ();

  if (error_state)
    {
      error ("input: unrecognized argument");
      return retval;
    }

  flush_octave_stdout ();

  octave_pager_stream::reset ();
  octave_diary_stream::reset ();

  octave_diary << prompt;

  bool eof = false;

  std::string input_buf = interactive_input (prompt.c_str (), eof);

  if (! (error_state || input_buf.empty ()))
    {
      size_t len = input_buf.length ();

      octave_diary << input_buf;

      if (input_buf[len - 1] != '\n')
        octave_diary << "\n";

      if (len < 1)
        return read_as_string ? octave_value ("") : octave_value (Matrix ());

      if (read_as_string)
        {
          // FIXME: fix gnu_readline and octave_gets instead!
          if (input_buf.length () == 1 && input_buf[0] == '\n')
            retval(0) = "";
          else
            retval(0) = input_buf;
        }
      else
        {
          int parse_status = 0;

          retval = eval_string (input_buf, true, parse_status, nargout);

          if (! Vdebugging && retval.length () == 0)
            retval(0) = Matrix ();
        }
    }
  else
    error ("input: reading user-input failed!");

  return retval;
}

DEFUN (input, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{ans} =} input (@var{prompt})\n\
@deftypefnx {Built-in Function} {@var{ans} =} input (@var{prompt}, \"s\")\n\
Print @var{prompt} and wait for user input.\n\
\n\
For example,\n\
\n\
@example\n\
input (\"Pick a number, any number! \")\n\
@end example\n\
\n\
@noindent\n\
prints the prompt\n\
\n\
@example\n\
Pick a number, any number!\n\
@end example\n\
\n\
@noindent\n\
and waits for the user to enter a value.  The string entered by the user\n\
is evaluated as an expression, so it may be a literal constant, a variable\n\
name, or any other valid Octave code.\n\
\n\
The number of return arguments, their size, and their class depend on the\n\
expression entered.\n\
\n\
If you are only interested in getting a literal string value, you can call\n\
@code{input} with the character string @qcode{\"s\"} as the second argument.\n\
This tells Octave to return the string entered by the user directly, without\n\
evaluating it first.\n\
\n\
Because there may be output waiting to be displayed by the pager, it is a\n\
good idea to always call @code{fflush (stdout)} before calling @code{input}.\n\
 This will ensure that all pending output is written to the screen before\n\
your prompt.\n\
@seealso{yes_or_no, kbhit, pause, menu, listdlg}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    retval = get_user_input (args, std::max (nargout, 1));
  else
    print_usage ();

  return retval;
}

bool
octave_yes_or_no (const std::string& prompt)
{
  std::string prompt_string = prompt + "(yes or no) ";

  while (1)
    {
      bool eof = false;

      std::string input_buf = interactive_input (prompt_string, eof);

      if (input_buf == "yes")
        return true;
      else if (input_buf == "no")
        return false;
      else
        message (0, "Please answer yes or no.");
    }
}

DEFUN (yes_or_no, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{ans} =} yes_or_no (\"@var{prompt}\")\n\
Ask the user a yes-or-no question.\n\
\n\
Return logical true if the answer is yes or false if the answer is no.\n\
\n\
Takes one argument, @var{prompt}, which is the string to display when asking\n\
the question.  @var{prompt} should end in a space; @code{yes-or-no} adds the\n\
string @samp{(yes or no) } to it.  The user must confirm the answer with\n\
@key{RET} and can edit it until it has been confirmed.\n\
@seealso{input}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0 || nargin == 1)
    {
      std::string prompt;

      if (nargin == 1)
        {
          if (args(0).is_string ())
            prompt = args(0).string_value ();
          else
            {
              error ("yes_or_no: PROMPT must be a string");
              return retval;
            }
        }

      retval = octave_yes_or_no (prompt);
    }
  else
    print_usage ();

  return retval;
}

octave_value
do_keyboard (const octave_value_list& args)
{
  octave_value retval;

  int nargin = args.length ();

  assert (nargin == 0 || nargin == 1);

  unwind_protect frame;

  frame.add_fcn (command_history::ignore_entries,
                 command_history::ignoring_entries ());

  command_history::ignore_entries (false);

  frame.protect_var (Vdebugging);

  frame.add_fcn (octave_call_stack::restore_frame,
                 octave_call_stack::current_frame ());

  // FIXME: probably we just want to print one line, not the
  // entire statement, which might span many lines...
  //
  // tree_print_code tpc (octave_stdout);
  // stmt.accept (tpc);

  Vdebugging = true;

  std::string prompt = "debug> ";
  if (nargin > 0)
    prompt = args(0).string_value ();

  if (! error_state)
    get_debug_input (prompt);

  return retval;
}

DEFUN (keyboard, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} keyboard ()\n\
@deftypefnx {Built-in Function} {} keyboard (\"@var{prompt}\")\n\
Stop m-file execution and enter debug mode.\n\
\n\
When the @code{keyboard} function is executed, Octave prints a prompt and\n\
waits for user input.  The input strings are then evaluated and the results\n\
are printed.  This makes it possible to examine the values of variables\n\
within a function, and to assign new values if necessary.  To leave the\n\
prompt and return to normal execution type @samp{return} or @samp{dbcont}.\n\
The @code{keyboard} function does not return an exit status.\n\
\n\
If @code{keyboard} is invoked without arguments, a default prompt of\n\
@samp{debug> } is used.\n\
@seealso{dbstop, dbcont, dbquit}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0 || nargin == 1)
    {
      unwind_protect frame;

      frame.add_fcn (octave_call_stack::restore_frame,
                     octave_call_stack::current_frame ());

      // Skip the frame assigned to the keyboard function.
      octave_call_stack::goto_frame_relative (0);

      tree_evaluator::debug_mode = true;
      tree_evaluator::quiet_breakpoint_flag = false;

      tree_evaluator::current_frame = octave_call_stack::current_frame ();

      do_keyboard (args);
    }
  else
    print_usage ();

  return retval;
}

DEFUN (echo, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} echo\n\
@deftypefnx {Command} {} echo on\n\
@deftypefnx {Command} {} echo off\n\
@deftypefnx {Command} {} echo on all\n\
@deftypefnx {Command} {} echo off all\n\
Control whether commands are displayed as they are executed.\n\
\n\
Valid options are:\n\
\n\
@table @code\n\
@item on\n\
Enable echoing of commands as they are executed in script files.\n\
\n\
@item off\n\
Disable echoing of commands as they are executed in script files.\n\
\n\
@item on all\n\
Enable echoing of commands as they are executed in script files and\n\
functions.\n\
\n\
@item off all\n\
Disable echoing of commands as they are executed in script files and\n\
functions.\n\
@end table\n\
\n\
@noindent\n\
With no arguments, @code{echo} toggles the current echo state.\n\
@end deftypefn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("echo");

  if (error_state)
    return retval;

  switch (argc)
    {
    case 1:
      {
        if ((Vecho_executing_commands & ECHO_SCRIPTS)
            || (Vecho_executing_commands & ECHO_FUNCTIONS))
          Vecho_executing_commands = ECHO_OFF;
        else
          Vecho_executing_commands = ECHO_SCRIPTS;
      }
      break;

    case 2:
      {
        std::string arg = argv[1];

        if (arg == "on")
          Vecho_executing_commands = ECHO_SCRIPTS;
        else if (arg == "off")
          Vecho_executing_commands = ECHO_OFF;
        else
          print_usage ();
      }
      break;

    case 3:
      {
        std::string arg = argv[1];

        if (arg == "on" && argv[2] == "all")
          {
            int tmp = (ECHO_SCRIPTS | ECHO_FUNCTIONS);
            Vecho_executing_commands = tmp;
          }
        else if (arg == "off" && argv[2] == "all")
          Vecho_executing_commands = ECHO_OFF;
        else
          print_usage ();
      }
      break;

    default:
      print_usage ();
      break;
    }

  return retval;
}

DEFUN (__echostate__, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{state} =} __echostate__ ()\n\
Undocumented internal function\n\
@end deftypefn")
{
  return ovl (Vecho_executing_commands == ECHO_SCRIPTS);
}

DEFUN (completion_matches, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} completion_matches (@var{hint})\n\
Generate possible completions given @var{hint}.\n\
\n\
This function is provided for the benefit of programs like Emacs which\n\
might be controlling Octave and handling user input.  The current\n\
command number is not incremented when this function is called.  This is\n\
a feature, not a bug.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string hint = args(0).string_value ();

      if (! error_state)
        {
          int n = 32;

          string_vector list (n);

          int k = 0;

          for (;;)
            {
              std::string cmd = generate_completion (hint, k);

              if (! cmd.empty ())
                {
                  if (k == n)
                    {
                      n *= 2;
                      list.resize (n);
                    }

                  list[k++] = cmd;
                }
              else
                {
                  list.resize (k);
                  break;
                }
            }

          if (nargout > 0)
            {
              if (! list.empty ())
                retval = list;
              else
                retval = "";
            }
          else
            {
              // We don't use string_vector::list_in_columns here
              // because it will be easier for Emacs if the names
              // appear in a single column.

              int len = list.length ();

              for (int i = 0; i < len; i++)
                octave_stdout << list[i] << "\n";
            }

          octave_completion_matches_called = true;
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (readline_read_init_file, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} readline_read_init_file (@var{file})\n\
Read the readline library initialization file @var{file}.\n\
\n\
If @var{file} is omitted, read the default initialization file\n\
(normally @file{~/.inputrc}).\n\
\n\
@xref{Readline Init File, , , readline, GNU Readline Library},\n\
for details.\n\
@seealso{readline_re_read_init_file}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    command_editor::read_init_file ();
  else if (nargin == 1)
    {
      std::string file = args(0).string_value ();

      if (! error_state)
        command_editor::read_init_file (file);
    }
  else
    print_usage ();

  return retval;
}

DEFUN (readline_re_read_init_file, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} readline_re_read_init_file ()\n\
Re-read the last readline library initialization file that was read.\n\
\n\
@xref{Readline Init File, , , readline, GNU Readline Library},\n\
for details.\n\
@seealso{readline_read_init_file}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 0)
    command_editor::re_read_init_file ();
  else
    print_usage ();

  return retval;
}

static int
internal_input_event_hook_fcn (void)
{
  input_event_hook_functions.run ();

  if (input_event_hook_functions.empty ())
    command_editor::remove_event_hook (internal_input_event_hook_fcn);

  return 0;
}

DEFUN (add_input_event_hook, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{id} =} add_input_event_hook (@var{fcn})\n\
@deftypefnx {Built-in Function} {@var{id} =} add_input_event_hook (@var{fcn}, @var{data})\n\
Add the named function or function handle @var{fcn} to the list of functions\n\
to call periodically when Octave is waiting for input.\n\
\n\
The function should have the form\n\
\n\
@example\n\
@var{fcn} (@var{data})\n\
@end example\n\
\n\
If @var{data} is omitted, Octave calls the function without any arguments.\n\
\n\
The returned identifier may be used to remove the function handle from the\n\
list of input hook functions.\n\
@seealso{remove_input_event_hook}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_value user_data;

      if (nargin == 2)
        user_data = args(1);

      hook_function hook_fcn (args(0), user_data);

      if (! error_state)
        {
          if (input_event_hook_functions.empty ())
            command_editor::add_event_hook (internal_input_event_hook_fcn);

          input_event_hook_functions.insert (hook_fcn.id (), hook_fcn);

          retval = hook_fcn.id ();
        }
      else
        error ("add_input_event_hook: FCN must be a function handle or string");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (remove_input_event_hook, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} remove_input_event_hook (@var{name})\n\
@deftypefnx {Built-in Function} {} remove_input_event_hook (@var{fcn_id})\n\
Remove the named function or function handle with the given identifier\n\
from the list of functions to call periodically when Octave is waiting\n\
for input.\n\
@seealso{add_input_event_hook}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string hook_fcn_id = args(0).string_value ();

      bool warn = (nargin < 2);

      if (! error_state)
        {
          hook_function_list::iterator p
            = input_event_hook_functions.find (hook_fcn_id);

          if (p != input_event_hook_functions.end ())
            input_event_hook_functions.erase (p);
          else if (warn)
            warning ("remove_input_event_hook: %s not found in list",
                     hook_fcn_id.c_str ());

          if (input_event_hook_functions.empty ())
            command_editor::remove_event_hook (internal_input_event_hook_fcn);
        }
      else
        error ("remove_input_event_hook: argument not valid as a hook function name or id");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (PS1, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} PS1 ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} PS1 (@var{new_val})\n\
@deftypefnx {Built-in Function} {} PS1 (@var{new_val}, \"local\")\n\
Query or set the primary prompt string.\n\
\n\
When executing interactively, Octave displays the primary prompt when it is\n\
ready to read a command.\n\
\n\
The default value of the primary prompt string is @qcode{\"octave:\\#> \"}.\n\
To change it, use a command like\n\
\n\
@example\n\
PS1 (\"\\\\u@@\\\\H> \")\n\
@end example\n\
\n\
@noindent\n\
which will result in the prompt @samp{boris@@kremvax> } for the user\n\
@samp{boris} logged in on the host @samp{kremvax.kgb.su}.  Note that two\n\
backslashes are required to enter a backslash into a double-quoted\n\
character string.  @xref{Strings}.\n\
\n\
You can also use ANSI escape sequences if your terminal supports them.\n\
This can be useful for coloring the prompt.  For example,\n\
\n\
@example\n\
PS1 (\"\\\\[\\\\033[01;31m\\\\]\\\\s:\\\\#> \\\\[\\\\033[0m\\\\]\")\n\
@end example\n\
\n\
@noindent\n\
will give the default Octave prompt a red coloring.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{PS2, PS4}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (PS1);
}

DEFUN (PS2, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} PS2 ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} PS2 (@var{new_val})\n\
@deftypefnx {Built-in Function} {} PS2 (@var{new_val}, \"local\")\n\
Query or set the secondary prompt string.\n\
\n\
The secondary prompt is printed when Octave is expecting additional input to\n\
complete a command.  For example, if you are typing a @code{for} loop that\n\
spans several lines, Octave will print the secondary prompt at the beginning\n\
of each line after the first.  The default value of the secondary prompt\n\
string is @qcode{\"> \"}.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{PS1, PS4}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (PS2);
}

DEFUN (PS4, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} PS4 ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} PS4 (@var{new_val})\n\
@deftypefnx {Built-in Function} {} PS4 (@var{new_val}, \"local\")\n\
Query or set the character string used to prefix output produced\n\
when echoing commands is enabled.\n\
\n\
The default value is @qcode{\"+ \"}.\n\
@xref{Diary and Echo Commands}, for a description of echoing commands.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{echo, echo_executing_commands, PS1, PS2}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (PS4);
}

DEFUN (completion_append_char, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} completion_append_char ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} completion_append_char (@var{new_val})\n\
@deftypefnx {Built-in Function} {} completion_append_char (@var{new_val}, \"local\")\n\
Query or set the internal character variable that is appended to\n\
successful command-line completion attempts.\n\
\n\
The default value is @qcode{\" \"} (a single space).\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (completion_append_char);
}

DEFUN (echo_executing_commands, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} echo_executing_commands ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} echo_executing_commands (@var{new_val})\n\
@deftypefnx {Built-in Function} {} echo_executing_commands (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls the echo state.\n\
\n\
It may be the sum of the following values:\n\
\n\
@table @asis\n\
@item 1\n\
Echo commands read from script files.\n\
\n\
@item 2\n\
Echo commands from functions.\n\
\n\
@item 4\n\
Echo commands read from command line.\n\
@end table\n\
\n\
More than one state can be active at once.  For example, a value of 3 is\n\
equivalent to the command @kbd{echo on all}.\n\
\n\
The value of @code{echo_executing_commands} may be set by the @kbd{echo}\n\
command or the command line option @option{--echo-commands}.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (echo_executing_commands);
}

DEFUN (__request_drawnow__, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} __request_drawnow__ ()\n\
@deftypefnx {Built-in Function} {} __request_drawnow__ (@var{flag})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    Vdrawnow_requested = true;
  else if (nargin == 1)
    Vdrawnow_requested = args(0).bool_value ();
  else
    print_usage ();

  return retval;
}

DEFUN (__gud_mode__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __gud_mode__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = Vgud_mode;
  else if (nargin == 1)
    Vgud_mode = args(0).bool_value ();
  else
    print_usage ();

  return retval;
}

DEFUN (filemarker, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} filemarker ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} filemarker (@var{new_val})\n\
@deftypefnx {Built-in Function} {} filemarker (@var{new_val}, \"local\")\n\
Query or set the character used to separate the filename from the subfunction\n\
names contained within the file.\n\
\n\
By default this is the character @samp{>}.\n\
This can be used in a generic manner to interact with subfunctions.\n\
For example,\n\
\n\
@example\n\
help ([\"myfunc\", filemarker, \"mysubfunc\"])\n\
@end example\n\
\n\
@noindent\n\
returns the help string associated with the subfunction @code{mysubfunc}\n\
located in the file @file{myfunc.m}.\n\
\n\
@code{filemarker} is also useful during debugging for placing breakpoints\n\
within subfunctions or nested functions.\n\
For example,\n\
\n\
@example\n\
dbstop ([\"myfunc\", filemarker, \"mysubfunc\"])\n\
@end example\n\
\n\
@noindent\n\
will set a breakpoint at the first line of the subfunction @code{mysubfunc}.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  char tmp = Vfilemarker;
  octave_value retval = SET_INTERNAL_VARIABLE (filemarker);

  // The character passed must not be a legal character for a function name
  if (! error_state && (::isalnum (Vfilemarker) || Vfilemarker == '_'))
    {
      Vfilemarker = tmp;
      error ("filemarker: character can not be a valid character for a function name");
    }

  return retval;
}
