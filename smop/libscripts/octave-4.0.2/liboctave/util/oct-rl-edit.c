/*

Copyright (C) 2000-2015 John W. Eaton

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

#if defined (USE_READLINE)

#include <stdio.h>
#include <stdlib.h>

#include <readline/readline.h>

#include "oct-rl-edit.h"

#define OCTAVE_RL_SAVE_STRING(ss, s) \
  static char *ss = 0; \
 \
  if (ss) \
    { \
      free (ss); \
      ss = 0; \
    } \
 \
  ss = malloc (strlen (s) + 1); \
 \
  strcpy (ss, s)

void
octave_rl_redisplay (void)
{
  rl_redisplay ();
}

int
octave_rl_screen_height (void)
{
  int rows, cols;
  rl_get_screen_size (&rows, &cols);
  return rows;
}

int
octave_rl_screen_width (void)
{
  int rows, cols;
  rl_get_screen_size (&rows, &cols);
  return cols;
}

void
octave_rl_enable_paren_matching (int val)
{
  rl_variable_bind ("blink-matching-paren", val ? "1" : "0");
}

int
octave_rl_erase_empty_line (int val)
{
  int retval = rl_erase_empty_line;
  rl_erase_empty_line = val;
  return retval;
}

/* It would be much simpler if we could just call _rl_clear_screen to
   only clear the screen, but it is not a public function, and on some
   systems, it is not exported from shared library versions of
   readline, so we can't use it.

   Instead, temporarily redefine the redisplay function to do nothing.

   FIXME -- It would be safer to do this when protected from
   interrupts... */

static void
flush_stdout (void)
{
  fflush (stdout);
}

void
octave_rl_clear_screen (int skip_redisplay)
{
  int ignore1 = 0;
  int ignore2 = 0;

  if (skip_redisplay)
    {
      rl_voidfunc_t *saved_redisplay_function = rl_redisplay_function;

      rl_redisplay_function = flush_stdout;

      rl_clear_screen (ignore1, ignore2);

      rl_redisplay_function = saved_redisplay_function;
    }
  else
    rl_clear_screen (ignore1, ignore2);
}

void
octave_rl_resize_terminal (void)
{
  rl_resize_terminal ();
}

void
octave_rl_set_screen_size (int ht, int wd)
{
  rl_set_screen_size (ht, wd);
}

void
octave_rl_restore_terminal_state ()
{
  if (rl_deprep_term_function)
    rl_deprep_term_function ();
}

char *
octave_rl_copy_line (void)
{
  return rl_copy_text (0, rl_end);
}

void
octave_rl_replace_line (const char *s, int clear_undo)
{
  rl_replace_line (s, clear_undo);
}

void
octave_rl_kill_full_line (void)
{
  rl_kill_full_line (0, 0);
}

void
octave_rl_insert_text (const char *s)
{
  rl_insert_text (s);
}

int
octave_rl_newline (int count, int key)
{
  return rl_newline (count, key);
}

const char *
octave_rl_line_buffer (void)
{
  return rl_line_buffer;
}

int
octave_rl_do_undo (void)
{
  return rl_do_undo ();
}

void
octave_rl_clear_undo_list (void)
{
  if (rl_undo_list)
    {
      rl_free_undo_list ();

      rl_undo_list = 0;
    }
}

void
octave_rl_set_name (const char *n)
{
  OCTAVE_RL_SAVE_STRING (nm, n);

  rl_readline_name = nm;

  /* Since we've already called rl_initialize, we need to re-read the
     init file to take advantage of the conditional parsing feature
     based on rl_readline_name; */

  rl_re_read_init_file (0, 0);
}

char *
octave_rl_readline (const char *prompt)
{
  return readline (prompt);
}

void
octave_rl_set_input_stream (FILE *f)
{
  rl_instream = f;
}

FILE *
octave_rl_get_input_stream (void)
{
  return rl_instream;
}

void
octave_rl_set_output_stream (FILE *f)
{
  rl_outstream = f;
}

FILE *
octave_rl_get_output_stream (void)
{
  return rl_outstream;
}

void
octave_rl_read_init_file (const char *f)
{
  rl_read_init_file (f);
}

void
octave_rl_re_read_init_file (void)
{
  rl_re_read_init_file (0, 0);
}

int
octave_rl_filename_completion_desired (int arg)
{
  int retval = rl_filename_completion_desired;
  rl_filename_completion_desired = arg;
  return retval;
}

int
octave_rl_filename_quoting_desired (int arg)
{
  int retval = rl_filename_quoting_desired;
  rl_filename_quoting_desired = arg;
  return retval;
}

int
octave_rl_prefer_env_winsize (int arg)
{
  int retval = rl_prefer_env_winsize;
  rl_prefer_env_winsize = arg;
  return retval;
}

void
octave_rl_done (int arg)
{
  rl_done = arg;
}

char *
octave_rl_filename_completion_function (const char *text, int state)
{
  return rl_filename_completion_function (text, state);
}

void
octave_rl_set_basic_word_break_characters (const char *s)
{
  OCTAVE_RL_SAVE_STRING (ss, s);

  rl_basic_word_break_characters = ss;
}

void
octave_rl_set_completer_word_break_characters (const char *s)
{
  OCTAVE_RL_SAVE_STRING (ss, s);

  rl_completer_word_break_characters = ss;
}

void
octave_rl_set_basic_quote_characters (const char *s)
{
  OCTAVE_RL_SAVE_STRING (ss, s);

  rl_basic_quote_characters = ss;
}

void
octave_rl_set_filename_quote_characters (const char *s)
{
  OCTAVE_RL_SAVE_STRING (ss, s);

  rl_filename_quote_characters = ss;
}

void
octave_rl_set_completer_quote_characters (const char *s)
{
  OCTAVE_RL_SAVE_STRING (ss, s);

  rl_completer_quote_characters = ss;
}

void
octave_rl_set_completion_append_character (char c)
{
  rl_completion_append_character = c;
}

void
octave_rl_set_completion_function (rl_attempted_completion_fcn_ptr f)
{
  rl_attempted_completion_function = f;
}

void
octave_rl_set_quoting_function (rl_quoting_fcn_ptr f)
{
  rl_filename_quoting_function = f;
}

void
octave_rl_set_dequoting_function (rl_dequoting_fcn_ptr f)
{
  rl_filename_dequoting_function = f;
}

void
octave_rl_set_char_is_quoted_function (rl_char_is_quoted_fcn_ptr f)
{
  rl_char_is_quoted_p = f;
}

void
octave_rl_set_startup_hook (rl_startup_hook_fcn_ptr f)
{
  rl_startup_hook = f;
}

rl_startup_hook_fcn_ptr
octave_rl_get_startup_hook (void)
{
  return rl_startup_hook;
}

void
octave_rl_set_pre_input_hook (rl_pre_input_hook_fcn_ptr f)
{
  rl_pre_input_hook = f;
}

rl_pre_input_hook_fcn_ptr
octave_rl_get_pre_input_hook (void)
{
  return rl_pre_input_hook;
}

void
octave_rl_set_event_hook (rl_event_hook_fcn_ptr f)
{
  rl_event_hook = f;
}

rl_event_hook_fcn_ptr
octave_rl_get_event_hook (void)
{
  return rl_event_hook;
}

char **
octave_rl_completion_matches (const char *text, rl_completer_fcn_ptr f)
{
  return rl_completion_matches (text, f);
}

char
octave_rl_prompt_start_ignore (void)
{
  return RL_PROMPT_START_IGNORE;
}

char
octave_rl_prompt_end_ignore (void)
{
  return RL_PROMPT_END_IGNORE;
}

void
octave_rl_add_defun (const char *name, rl_fcn_ptr f, char key)
{
  rl_add_defun (name, f, key);
}

void
octave_rl_set_terminal_name (const char *term)
{
  OCTAVE_RL_SAVE_STRING (saved_term, term);

  rl_terminal_name = saved_term;
}

void
octave_rl_initialize (void)
{
#if defined (__WIN32__) && ! defined (__CYGWIN__)
  rl_catch_signals = 0;
#endif

  rl_initialize ();
}

int
octave_rl_history_search_forward (int count, int ignore)
{
  return rl_history_search_forward (count, ignore);
}

int
octave_rl_history_search_backward (int count, int ignore)
{
  return rl_history_search_backward (count, ignore);
}

char
octave_rl_ctrl (char c)
{
  return CTRL (c);
}

char
octave_rl_meta (char c)
{
  return META (c);
}

#endif
