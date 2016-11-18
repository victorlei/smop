/*

Copyright (C) 2010-2015 David Bateman

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

#if !defined (octave_oct_openmp_h)
#define octave_oct_openmp_h 1

/* A macro to make using OpenMP easier, and easier to disable */
#ifdef HAVE_OPENMP
#include <omp.h>
#define OCTAVE_OMP_PRAGMA(x) _Pragma (#x)
#else
#define OCTAVE_OMP_PRAGMA(x)
#endif

#endif
