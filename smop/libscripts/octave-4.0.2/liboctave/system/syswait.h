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

#if !defined (octave_syswait_h)
#define octave_syswait_h 1

#ifdef __cplusplus
extern "C" {
#endif

/* This mess suggested by the autoconf manual.  */

#include <sys/types.h>

#if defined HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifndef WIFEXITED
#define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#ifndef WEXITSTATUS
#define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif

#ifndef WIFSIGNALLED
#define WIFSIGNALLED(stat_val) \
  (((stat_val) & 0177) != 0177 && ((stat_val) & 0177) != 0)
#endif

#if defined (__MINGW32__) || defined (_MSC_VER)
#define HAVE_WAITPID 1
#include <process.h>
#define WAITPID(a, b, c) _cwait (b, a, c)
/* Action argument is ignored for _cwait, so arbitrary definition.  */
#define WNOHANG 0
#else
#define WAITPID(a, b, c) waitpid (a, b, c)
#endif

#ifdef __cplusplus
}
#endif

#endif
