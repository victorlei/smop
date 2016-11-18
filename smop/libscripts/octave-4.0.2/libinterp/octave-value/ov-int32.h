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

#if !defined (octave_ov_int32_h)
#define octave_ov_int32_h 1

#define OCTAVE_INT_T octave_int32

#define OCTAVE_VALUE_INT_MATRIX_T octave_int32_matrix
#define OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION int32_array_value

#define OCTAVE_VALUE_INT_SCALAR_T octave_int32_scalar
#define OCTAVE_VALUE_INT_SCALAR_EXTRACTOR_FUNCTION int32_scalar_value

#define OCTAVE_TYPE_PREDICATE_FUNCTION is_int32_type

#define OCTAVE_INT_MX_CLASS mxINT32_CLASS

#define OCTAVE_INT_BTYP btyp_int32

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
