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

#include "oct-rl-hist.h"

#if defined (USE_READLINE)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <readline/history.h>

/* check_history_control, hc_erasedup, and the core of
   octave_add_history were borrowed from Bash.  */

/* Check LINE against what HISTCONTROL says to do.  Returns 1 if the line
   should be saved; 0 if it should be discarded.  */
static int
check_history_control (const char *line, int history_control)
{
  HIST_ENTRY *temp;
  int r;

  if (history_control == 0)
    return 1;

  /* ignorespace or ignoreboth */
  if ((history_control & HC_IGNSPACE) && *line == ' ')
    return 0;

  /* ignoredups or ignoreboth */
  if (history_control & HC_IGNDUPS)
    {
      using_history ();
      temp = previous_history ();

      r = (temp == 0 || strcmp (temp->line, line));

      using_history ();

      if (r == 0)
        return r;
    }

  return 1;
}

/* Remove all entries matching LINE from the history list.  Triggered when
   HISTCONTROL includes `erasedups'.  */

static void
hc_erasedups (const char *line)
{
  HIST_ENTRY *temp;
  int r;

  using_history ();
  while ((temp = previous_history ()))
    {
      if (! strcmp (temp->line, line))
        {
          r = where_history ();
          remove_history (r);
        }
    }
  using_history ();
}

/* Check LINE against HISTCONTROL and add it to the history if it's OK.
   Returns 1 if the line was saved in the history, 0 otherwise.  */

int
octave_add_history (const char *line, int history_control)
{
  if (check_history_control (line, history_control))
    {
      /* We're committed to saving the line.  If the user has requested it,
         remove other matching lines from the history.  */

      if (history_control & HC_ERASEDUPS)
        hc_erasedups (line);

      add_history (line);

      return 1;
    }

  return 0;
}

int
octave_where_history (void)
{
  return where_history ();
}

int
octave_history_length (void)
{
  return history_length;
}

int
octave_max_input_history (void)
{
  return max_input_history;
}

int
octave_history_base (void)
{
  return history_base;
}

void
octave_stifle_history (int n)
{
  stifle_history (n);
}

int
octave_unstifle_history (void)
{
  return unstifle_history ();
}

int
octave_history_is_stifled (void)
{
  return history_is_stifled ();
}

int
octave_history_set_pos (int n)
{
  return history_set_pos (n);
}

int
octave_read_history (const char *f)
{
  return read_history (f);
}

void
octave_using_history (void)
{
  using_history ();
}

int
octave_read_history_range (const char *f, int b, int e)
{
  return read_history_range (f, b, e);
}

int
octave_write_history (const char *f)
{
  return write_history (f);
}

int
octave_append_history (int n, const char *f)
{
  return append_history (n, f);
}

int
octave_history_truncate_file (const char *f, int n)
{
  return history_truncate_file (f, n);
}

void
octave_remove_history (int n)
{
  HIST_ENTRY *discard = remove_history (n);

  if (discard)
    {
      if (discard->line)
        free (discard->line);

      free (discard);
    }
}

void
octave_clear_history (void)
{
  clear_history ();
}

char *
octave_history_goto_mark (int n)
{
  HIST_ENTRY *h;

  char *retval = 0;

  if (history_set_pos (n))
    {
      h = current_history ();

      if (h)
        retval = h->line;
    }

  return retval;
}

char *
octave_history_get (int n)
{
  char *retval = 0;

  HIST_ENTRY *h = history_get (n);

  if (h)
    retval = h->line;

  return retval;
}

char **
octave_history_list (int limit, int number_lines)
{
  static char **retval = 0;

  HIST_ENTRY **hlist = 0;

  if (retval)
    {
      char **p = retval;

      while (*p)
        free (*p++);

      free (retval);

      retval = 0;
    }

  hlist = history_list ();

  if (hlist)
    {
      int i, k;

      int beg = 0;
      int end = 0;
      while (hlist[end])
        end++;

      beg = (limit < 0 || end < limit) ? 0 : (end - limit);

      retval = malloc ((end - beg + 1) * sizeof (char **));

      k = 0;
      for (i = beg; i < end; i++)
        {
          char *line = hlist[i]->line;
          int len = line ? strlen (line) : 0;
          char *tmp = malloc (len + 64);

          if (number_lines)
            sprintf (tmp, "%5d %s", i + history_base,
                     line ? line : "");
          else
            strcpy (tmp, line ? line : "");

          retval[k++] = tmp;
        }

      retval[k] = 0;
    }

  return retval;
}

void
octave_replace_history_entry (int which, const char *line)
{
  HIST_ENTRY *discard = replace_history_entry (which, line, 0);

  if (discard)
    {
      if (discard->line)
        free (discard->line);

      free (discard);
    }
}

#endif
