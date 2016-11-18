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

#if !defined (octave_ov_uint8_h)
#define octave_ov_uint8_h 1

#define OCTAVE_INT_T octave_uint8

#define OCTAVE_VALUE_INT_MATRIX_T octave_uint8_matrix
#define OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION uint8_array_value

#define OCTAVE_VALUE_INT_SCALAR_T octave_uint8_scalar
#define OCTAVE_VALUE_INT_SCALAR_EXTRACTOR_FUNCTION uint8_scalar_value

#define OCTAVE_TYPE_PREDICATE_FUNCTION is_uint8_type

#define OCTAVE_INT_MX_CLASS mxUINT8_CLASS

#define OCTAVE_INT_BTYP btyp_uint8

#include "ov-intx.h"

#undef OCTAVE_INT_T

#undef OCTAVE_VALUE_INT_MATRIX_T
#undef OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION

#undef OCTAVE_VALUE_INT_SCALAR_T
#undef OCTAVE_VALUE_INT_SCALAR_EXTRACTOR_FUNCTION

#undef OCTAVE_TYPE_PREDICATE_FUNCTION

#undef OCTAVE_INT_MX_CLASS

#undef OCTAVE_INT_BTYP

#endif
