/*

Copyright (C) 2007-2015 John W. Eaton

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

#if !defined (octave_lo_math_h)
#define octave_lo_math_h 1

#if defined (__cplusplus)
#include <cmath>
// if #undef log2 is missing in cmath, undef it here
#if defined (log2)
#undef log2
#endif
#else
#include <math.h>
#endif

#if defined (HAVE_SUNMATH_H)
#include <sunmath.h>
#endif

#endif
