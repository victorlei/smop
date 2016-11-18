/*

Copyright (C) 1996-2015 John W. Eaton

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

#if !defined (octave_cmd_edit_h)
#define octave_cmd_edit_h 1

#include <cstdio>

#include <set>
#include <string>

#include "str-vec.h"

class
OCTAVE_API
command_editor
{
protected:

  command_editor (void)
    : command_number (0), interrupted (false), initial_input () { }

public:

  typedef int (*startup_hook_fcn) (void);

  typedef int (*pre_input_hook_fcn) (void);

  typedef int (*event_hook_fcn) (void);

  typedef std::string (*completion_fcn) (const std::string&, int);

  typedef std::string (*quoting_fcn) (const std::string&, int, char);

  typedef std::string (*dequoting_fcn) (const std::string&, int);

  typedef int (*char_is_quoted_fcn) (const std::string&, int);

  typedef void (*user_accept_line_fcn) (const std::string&);

  virtual ~command_editor (void) { }

  static void set_name (const std::string& n);

  static std::string readline (const std::string& prompt);

  static std::string readline (const std::string& prompt, bool& eof);

  static void set_input_stream (FILE *f);

  static FILE *get_input_stream (void);

  static void set_output_stream (FILE *f);

  static FILE *get_output_stream (void);

  static void redisplay (void);

  static int terminal_rows (void);

  static int terminal_cols (void);

  static void clear_screen (bool skip_redisplay = false);

  static void resize_terminal (void);

  static void set_screen_size (int ht, int wd);

  static std::string decode_prompt_string (const std::string& s);

  static void restore_terminal_state (void);

  static void blink_matching_paren (bool flag);

  static bool erase_empty_line (bool flag);

  static void set_basic_word_break_characters (const std::string& s);

  static void set_completer_word_break_characters (const std::string& s);

  static void set_basic_quote_characters (const std::string& s);

  static void set_filename_quote_characters (const std::string& s);

  static void set_completer_quote_characters (const std::string& s);

  static void set_completion_append_character (char c);

  static void set_completion_function (completion_fcn f);

  static void set_quoting_function (quoting_fcn f);

  static void set_dequoting_function (dequoting_fcn f);

  static void set_char_is_quoted_function (char_is_quoted_fcn f);

  static void set_user_accept_line_function (user_accept_line_fcn f);

  static completion_fcn get_completion_function (void);

  static quoting_fcn get_quoting_function (void);

  static dequoting_fcn get_dequoting_function (void);

  static char_is_quoted_fcn get_char_is_quoted_function (void);

  static user_accept_line_fcn get_user_accept_line_function (void);

  static string_vector generate_filename_completions (const std::string& text);

  static std::string get_line_buffer (void);

  static std::string get_current_line (void);

  static void replace_line (const std::string& text, bool clear_undo = true);

  static void kill_full_line (void);

  static void insert_text (const std::string& text);

  static void newline (void);

  static void accept_line (void);

  static bool undo (void);

  static void clear_undo_list (void);

  static void add_startup_hook (startup_hook_fcn f);

  static void remove_startup_hook (startup_hook_fcn f);

  static void add_pre_input_hook (pre_input_hook_fcn f);

  static void remove_pre_input_hook (pre_input_hook_fcn f);

  static void add_event_hook (event_hook_fcn f);

  static void remove_event_hook (event_hook_fcn f);

  static void run_event_hooks (void);

  static void read_init_file (const std::string& file = std::string ());

  static void re_read_init_file (void);

  static bool filename_completion_desired (bool);

  static bool filename_quoting_desired (bool);

  static bool prefer_env_winsize (bool);

  static bool interrupt (bool = true);

  static int current_command_number (void);

  static void reset_current_command_number (int n);

  static void increment_current_command_number (void);

  static void force_default_editor (void);

  static void set_initial_input (const std::string& text);

  static int insert_initial_input (void);

private:

  // No copying!

  command_editor (const command_editor&);

  command_editor& operator = (const command_editor&);

  static bool instance_ok (void);

  static void make_command_editor (void);

  static int startup_handler (void);

  static int pre_input_handler (void);

  static int event_handler (void);

  static std::set<startup_hook_fcn> startup_hook_set;

  static std::set<pre_input_hook_fcn> pre_input_hook_set;

  static std::set<event_hook_fcn> event_hook_set;

  typedef std::set<startup_hook_fcn>::iterator startup_hook_set_iterator;
  typedef std::set<startup_hook_fcn>::const_iterator startup_hook_set_const_iterator;

  typedef std::set<pre_input_hook_fcn>::iterator pre_input_hook_set_iterator;
  typedef std::set<pre_input_hook_fcn>::const_iterator pre_input_hook_set_const_iterator;

  typedef std::set<event_hook_fcn>::iterator event_hook_set_iterator;
  typedef std::set<event_hook_fcn>::const_iterator event_hook_set_const_iterator;

  // The real thing.
  static command_editor *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

protected:

  // To use something other than the GNU readline library, derive a new
  // class from command_editor, overload these functions as
  // necessary, and make instance point to the new class.

  virtual void do_set_name (const std::string&) { }

  std::string do_readline (const std::string& prompt)
  {
    bool eof;

    return do_readline (prompt, eof);
  }

  virtual std::string do_readline (const std::string&, bool&) = 0;

  virtual void do_set_input_stream (FILE *) = 0;

  virtual FILE *do_get_input_stream (void) = 0;

  virtual void do_set_output_stream (FILE *) = 0;

  virtual FILE *do_get_output_stream (void) = 0;

  virtual void do_redisplay (void) { }

  virtual int do_terminal_rows (void) { return 24; }

  virtual int do_terminal_cols (void) { return 80; }

  virtual void do_clear_screen (bool) { }

  virtual void do_resize_terminal (void) { }

  virtual void do_set_screen_size (int, int) { }

  virtual std::string do_decode_prompt_string (const std::string&);

  virtual std::string newline_chars (void) { return "\n"; }

  virtual void do_restore_terminal_state (void) { }

  virtual void do_blink_matching_paren (bool) { }

  virtual bool do_erase_empty_line (bool) { return false; }

  virtual void do_set_basic_word_break_characters (const std::string&) { }

  virtual void do_set_completer_word_break_characters (const std::string&) { }

  virtual void do_set_basic_quote_characters (const std::string&) { }

  virtual void do_set_filename_quote_characters (const std::string&) { }

  virtual void do_set_completer_quote_characters (const std::string&) { }

  virtual void do_set_completion_append_character (char) { }

  virtual void do_set_completion_function (completion_fcn) { }

  virtual void do_set_quoting_function (quoting_fcn) { }

  virtual void do_set_dequoting_function (dequoting_fcn) { }

  virtual void do_set_char_is_quoted_function (char_is_quoted_fcn) { }

  virtual void do_set_user_accept_line_function (user_accept_line_fcn) { }

  virtual completion_fcn do_get_completion_function (void) const { return 0; }

  virtual quoting_fcn do_get_quoting_function (void) const { return 0; }

  virtual dequoting_fcn do_get_dequoting_function (void) const { return 0; }

  virtual char_is_quoted_fcn do_get_char_is_quoted_function (void) const
  { return 0; }

  virtual user_accept_line_fcn do_get_user_accept_line_function (void) const
  { return 0; }

  virtual string_vector
  do_generate_filename_completions (const std::string& text) = 0;

  virtual std::string do_get_line_buffer (void) const = 0;

  virtual std::string do_get_current_line (void) const = 0;

  virtual void do_replace_line (const std::string& text, bool clear_undo) = 0;

  virtual void do_kill_full_line (void) = 0;

  virtual void do_insert_text (const std::string& text) = 0;

  virtual void do_newline (void) = 0;

  virtual void do_accept_line (void) = 0;

  virtual bool do_undo (void) { return false; }

  virtual void do_clear_undo_list (void) { }

  virtual void set_startup_hook (startup_hook_fcn) { }

  virtual void restore_startup_hook (void) { }

  virtual void set_pre_input_hook (pre_input_hook_fcn) { }

  virtual void restore_pre_input_hook (void) { }

  virtual void set_event_hook (event_hook_fcn) { }

  virtual void restore_event_hook (void) { }

  virtual void do_read_init_file (const std::string&) { }

  virtual void do_re_read_init_file (void) { }

  virtual bool do_filename_completion_desired (bool) { return false; }

  virtual bool do_filename_quoting_desired (bool) { return false; }

  virtual bool do_prefer_env_winsize (bool) { return false; }

  virtual void do_interrupt (bool) { }

  int do_insert_initial_input (void);

  int read_octal (const std::string& s);

  void error (int);

  void error (const std::string&);

  // The current command number.
  int command_number;

  bool interrupted;

  std::string initial_input;
};

#endif
