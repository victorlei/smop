/*

Copyright (C) 2008-2015 VZLU Prague, a.s.

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

// author: Jaroslav Hajek <highegg@gmail.com>

#if !defined (octave_oct_norm_h)
#define octave_oct_norm_h 1

#include "oct-cmplx.h"

#define DECLARE_XNORM_FUNCS(PREFIX, RTYPE) \
  class PREFIX##Matrix; \
  class PREFIX##ColumnVector; \
  class PREFIX##RowVector; \
  \
  extern OCTAVE_API RTYPE xnorm (const PREFIX##ColumnVector&, RTYPE p = 2); \
  extern OCTAVE_API RTYPE xnorm (const PREFIX##RowVector&, RTYPE p = 2); \
  extern OCTAVE_API RTYPE xnorm (const PREFIX##Matrix&, RTYPE p = 2); \
  extern OCTAVE_API RTYPE xfrobnorm (const PREFIX##Matrix&);

DECLARE_XNORM_FUNCS(, double)
DECLARE_XNORM_FUNCS(Complex, double)
DECLARE_XNORM_FUNCS(Float, float)
DECLARE_XNORM_FUNCS(FloatComplex, float)

DECLARE_XNORM_FUNCS(Sparse, double)
DECLARE_XNORM_FUNCS(SparseComplex, double)

#define DECLARE_COLROW_NORM_FUNCS(PREFIX, RPREFIX, RTYPE) \
  extern OCTAVE_API RPREFIX##RowVector xcolnorms (const PREFIX##Matrix&, RTYPE p = 2); \
  extern OCTAVE_API RPREFIX##ColumnVector xrownorms (const PREFIX##Matrix&, RTYPE p = 2); \

DECLARE_COLROW_NORM_FUNCS(, , double)
DECLARE_COLROW_NORM_FUNCS(Complex, , double)
DECLARE_COLROW_NORM_FUNCS(Float, Float, float)
DECLARE_COLROW_NORM_FUNCS(FloatComplex, Float, float)

DECLARE_COLROW_NORM_FUNCS(Sparse, , double)
DECLARE_COLROW_NORM_FUNCS(SparseComplex, , double)

#endif
