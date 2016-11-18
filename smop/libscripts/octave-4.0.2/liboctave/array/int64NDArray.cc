// N-D Array  manipulations.
/*

Copyright (C) 2004-2015 John W. Eaton

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

#include "int64NDArray.h"
#include "mx-op-defs.h"
#include "intNDArray.cc"

#include "bsxfun-defs.cc"

template class OCTAVE_API intNDArray<octave_int64>;

template OCTAVE_API
std::ostream&
operator << (std::ostream& os, const intNDArray<octave_int64>& a);

template OCTAVE_API
std::istream&
operator >> (std::istream& is, intNDArray<octave_int64>& a);

NDS_CMP_OPS (int64NDArray, octave_int64)
NDS_BOOL_OPS (int64NDArray, octave_int64)

SND_CMP_OPS (octave_int64, int64NDArray)
SND_BOOL_OPS (octave_int64, int64NDArray)

NDND_CMP_OPS (int64NDArray, int64NDArray)
NDND_BOOL_OPS (int64NDArray, int64NDArray)

MINMAX_FCNS (int64NDArray, octave_int64)

BSXFUN_STDOP_DEFS_MXLOOP (int64NDArray)
BSXFUN_STDREL_DEFS_MXLOOP (int64NDArray)

BSXFUN_OP_DEF_MXLOOP (pow, int64NDArray, mx_inline_pow)

BSXFUN_POW_MIXED_MXLOOP (int64NDArray)
