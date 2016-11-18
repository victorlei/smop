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

#if !defined (octave_siglist_h)
#define octave_siglist_h 1

#ifdef __cplusplus
extern "C"
{
#endif

/* This is borrowed from Emacs.  */

#if ! defined (HAVE_DECL_SYS_SIGLIST)
extern char *sys_siglist[];
#endif

extern void init_signals (void);

#if ! defined (HAVE_STRSIGNAL)
extern char *strsignal (int);
#endif

#ifdef __cplusplus
}
#endif

#endif
