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

#if !defined (octave_oct_rl_hist_h)
#define octave_oct_rl_hist_h 1

#ifdef __cplusplus
extern "C"
{
#endif

enum {
  HC_IGNSPACE = 0x01,
  HC_IGNDUPS = 0x02,
  HC_ERASEDUPS = 0x04
};

extern int octave_add_history (const char *, int);

extern int octave_where_history (void);

extern int octave_history_length (void);

extern int octave_max_input_history (void);

extern int octave_history_base (void);

extern void octave_stifle_history (int);

extern int octave_unstifle_history (void);

extern int octave_history_is_stifled (void);

extern int octave_history_set_pos (int);

extern int octave_read_history (const char *);

extern void octave_using_history (void);

extern int octave_read_history_range (const char *, int, int);

extern int octave_write_history (const char *);

extern int octave_append_history (int, const char *);

extern int octave_history_truncate_file (const char *, int);

extern void octave_remove_history (int);

extern void octave_clear_history (void);

extern char *octave_history_goto_mark (int n);

extern char *octave_history_get (int n);

extern char **octave_history_list (int, int);

extern void octave_replace_history_entry (int, const char *);

#ifdef __cplusplus
}
#endif

#endif
