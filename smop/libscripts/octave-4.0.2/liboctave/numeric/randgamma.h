/*

Copyright (C) 2006-2015 John W. Eaton

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

/* Original version written by Paul Kienzle distributed as free
   software in the in the public domain.  */

#ifndef _RANDGAMMA_H

#ifdef  __cplusplus
extern "C" {
#endif

extern OCTAVE_API double oct_randg (double a);
extern OCTAVE_API void oct_fill_randg (double a, octave_idx_type n, double *p);

extern OCTAVE_API float oct_float_randg (float a);
extern OCTAVE_API void oct_fill_float_randg (float a, octave_idx_type n,
                                             float *p);

#ifdef  __cplusplus
}
#endif
#endif
