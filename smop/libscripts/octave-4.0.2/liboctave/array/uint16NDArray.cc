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

#include "uint16NDArray.h"
#include "mx-op-defs.h"
#include "intNDArray.cc"

#include "bsxfun-defs.cc"

template class OCTAVE_API intNDArray<octave_uint16>;

template OCTAVE_API
std::ostream&
operator << (std::ostream& os, const intNDArray<octave_uint16>& a);

template OCTAVE_API
std::istream&
operator >> (std::istream& is, intNDArray<octave_uint16>& a);

NDS_CMP_OPS (uint16NDArray, octave_uint16)
NDS_BOOL_OPS (uint16NDArray, octave_uint16)

SND_CMP_OPS (octave_uint16, uint16NDArray)
SND_BOOL_OPS (octave_uint16, uint16NDArray)

NDND_CMP_OPS (uint16NDArray, uint16NDArray)
NDND_BOOL_OPS (uint16NDArray, uint16NDArray)

MINMAX_FCNS (uint16NDArray, octave_uint16)

BSXFUN_STDOP_DEFS_MXLOOP (uint16NDArray)
BSXFUN_STDREL_DEFS_MXLOOP (uint16NDArray)

BSXFUN_OP_DEF_MXLOOP (pow, uint16NDArray, mx_inline_pow)
BSXFUN_POW_MIXED_MXLOOP (uint16NDArray)
