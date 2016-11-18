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

// Use the GNU readline library for command line editing and hisory.

#if !defined (octave_input_h)
#define octave_input_h 1

#include <cstdio>

#include <string>

#include "oct-time.h"
#include "oct-obj.h"
#include "pager.h"

class octave_value;
class octave_base_lexer;

extern OCTINTERP_API FILE *get_input_from_stdin (void);

// TRUE means this is an interactive shell (forced or not)
extern bool interactive;

// TRUE means the user forced this shell to be interactive (-i).
// FALSE means the shell would be interactive, independent of user settings.
extern bool forced_interactive;

// TRUE after a call to completion_matches.
extern bool octave_completion_matches_called;

// TRUE if the plotting system has requested a call to drawnow at
// the next user prompt.
extern OCTINTERP_API bool Vdrawnow_requested;

// TRUE if we are in debugging mode.
extern OCTINTERP_API bool Vdebugging;

extern void initialize_command_input (void);

extern bool octave_yes_or_no (const std::string& prompt);

extern octave_value do_keyboard (const octave_value_list& args
                                 = octave_value_list ());

extern void remove_input_event_hook_functions (void);

extern void set_default_prompts (void);

extern std::string VPS4;

extern char Vfilemarker;

enum echo_state
{
  ECHO_OFF = 0,
  ECHO_SCRIPTS = 1,
  ECHO_FUNCTIONS = 2,
  ECHO_CMD_LINE = 4
};

extern int Vecho_executing_commands;

extern octave_time Vlast_prompt_time;

class
octave_base_reader
{
public:

  friend class octave_input_reader;

  octave_base_reader (octave_base_lexer *lxr)
    : count (1), pflag (0), lexer (lxr)
  { }

  octave_base_reader (const octave_base_reader& x)
    : count (1), pflag (x.pflag), lexer (x.lexer)
  { }

  virtual ~octave_base_reader (void) { }

  virtual std::string get_input (bool& eof) = 0;

  virtual std::string input_source (void) const { return in_src; }

  void reset (void) { promptflag (1); }

  void increment_promptflag (void) { pflag++; }

  void decrement_promptflag (void) { pflag--; }

  int promptflag (void) const { return pflag; }

  int promptflag (int n)
  {
    int retval = pflag;
    pflag = n;
    return retval;
  }

  std::string octave_gets (bool& eof);

  virtual bool reading_fcn_file (void) const;

  virtual bool reading_classdef_file (void) const;

  virtual bool reading_script_file (void) const;

  virtual bool input_from_terminal (void) const { return false; }

  virtual bool input_from_file (void) const { return false; }

  virtual bool input_from_eval_string (void) const { return false; }

private:

  int count;

  int pflag;

  octave_base_lexer *lexer;

  void do_input_echo (const std::string&) const;

  static const std::string in_src;
};

class
octave_terminal_reader : public octave_base_reader
{
public:

  octave_terminal_reader (octave_base_lexer *lxr = 0)
    : octave_base_reader (lxr)
  { }

  std::string get_input (bool& eof);

  std::string input_source (void) const { return in_src; }

  bool input_from_terminal (void) const { return true; }

private:

  static const std::string in_src;
};

class
octave_file_reader : public octave_base_reader
{
public:

  octave_file_reader (FILE *f_arg, octave_base_lexer *lxr = 0)
    : octave_base_reader (lxr), file (f_arg) { }

  std::string get_input (bool& eof);

  std::string input_source (void) const { return in_src; }

  bool input_from_file (void) const { return true; }

private:

  FILE *file;

  static const std::string in_src;
};

class
octave_eval_string_reader : public octave_base_reader
{
public:

  octave_eval_string_reader (const std::string& str,
                             octave_base_lexer *lxr = 0)
    : octave_base_reader (lxr), eval_string (str)
  { }

  std::string get_input (bool& eof);

  std::string input_source (void) const { return in_src; }

  bool input_from_eval_string (void) const { return true; }

private:

  std::string eval_string;

  static const std::string in_src;
};

class
octave_input_reader
{
public:
  octave_input_reader (octave_base_lexer *lxr = 0)
    : rep (new octave_terminal_reader (lxr))
  { }

  octave_input_reader (FILE *file, octave_base_lexer *lxr = 0)
    : rep (new octave_file_reader (file, lxr))
  { }

  octave_input_reader (const std::string& str, octave_base_lexer *lxr = 0)
    : rep (new octave_eval_string_reader (str, lxr))
  { }

  octave_input_reader (const octave_input_reader& ir)
  {
    rep = ir.rep;
    rep->count++;
  }

  octave_input_reader& operator = (const octave_input_reader& ir)
  {
    if (&ir != this)
      {
        rep = ir.rep;
        rep->count++;
      }

    return *this;
  }

  ~octave_input_reader (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  void reset (void) { return rep->reset (); }

  void increment_promptflag (void) { rep->increment_promptflag (); }

  void decrement_promptflag (void) { rep->decrement_promptflag (); }

  int promptflag (void) const { return rep->promptflag (); }

  int promptflag (int n) { return rep->promptflag (n); }

  std::string get_input (bool& eof)
  {
    return rep->get_input (eof);
  }

  std::string input_source (void) const
  {
    return rep->input_source ();
  }

  bool input_from_terminal (void) const
  {
    return rep->input_from_terminal ();
  }

  bool input_from_file (void) const
  {
    return rep->input_from_file ();
  }

  bool input_from_eval_string (void) const
  {
    return rep->input_from_eval_string ();
  }

private:

  octave_base_reader *rep;
};

#endif
