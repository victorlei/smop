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

#if !defined (octave_sysdir_h)
#define octave_sysdir_h 1

// This mess suggested by the autoconf manual.

// unistd.h defines _POSIX_VERSION on POSIX.1 systems.

#include <sys/types.h>
#include <unistd.h>

#if defined (HAVE_DIRENT_H) || defined (_POSIX_VERSION)
#include <dirent.h>
#define NLENGTH(dirent) (strlen((dirent)->d_name))
#else
#define dirent direct
#define NLENGTH(dirent) ((dirent)->d_namlen)
#if defined (HAVE_SYS_NDIR_H)
#include <sys/ndir.h>
#endif
#if defined (HAVE_SYS_DIR_H)
#include <sys/dir.h>
#endif
#if defined (HAVE_NDIR_H)
#include <ndir.h>
#endif
#endif

#endif
