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

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-range.h"
#include "ov-float.h"
#include "ov-flt-re-mat.h"
#include "ov-str-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"

// conversion ops

DEFFLTCONVFN (int8_matrix_to_float_matrix, int8_matrix, int8_array)
DEFFLTCONVFN (int16_matrix_to_float_matrix, int16_matrix, int16_array)
DEFFLTCONVFN (int32_matrix_to_float_matrix, int32_matrix, int32_array)
DEFFLTCONVFN (int64_matrix_to_float_matrix, int64_matrix, int64_array)

DEFFLTCONVFN (uint8_matrix_to_float_matrix, uint8_matrix, uint8_array)
DEFFLTCONVFN (uint16_matrix_to_float_matrix, uint16_matrix, uint16_array)
DEFFLTCONVFN (uint32_matrix_to_float_matrix, uint32_matrix, uint32_array)
DEFFLTCONVFN (uint64_matrix_to_float_matrix, uint64_matrix, uint64_array)

DEFFLTCONVFN (int8_scalar_to_float_matrix, int8_scalar, int8_array)
DEFFLTCONVFN (int16_scalar_to_float_matrix, int16_scalar, int16_array)
DEFFLTCONVFN (int32_scalar_to_float_matrix, int32_scalar, int32_array)
DEFFLTCONVFN (int64_scalar_to_float_matrix, int64_scalar, int64_array)

DEFFLTCONVFN (uint8_scalar_to_float_matrix, uint8_scalar, uint8_array)
DEFFLTCONVFN (uint16_scalar_to_float_matrix, uint16_scalar, uint16_array)
DEFFLTCONVFN (uint32_scalar_to_float_matrix, uint32_scalar, uint32_array)
DEFFLTCONVFN (uint64_scalar_to_float_matrix, uint64_scalar, uint64_array)

DEFFLTCONVFN (bool_matrix_to_float_matrix, bool_matrix, bool_array)
DEFFLTCONVFN (bool_scalar_to_float_matrix, bool, bool_array)

DEFFLTCONVFN (range_to_float_matrix, range, array)

DEFSTRFLTCONVFN(char_matrix_str_to_float_matrix, char_matrix_str)
DEFSTRFLTCONVFN(char_matrix_sq_str_to_float_matrix, char_matrix_sq_str)

DEFFLTCONVFN (float_scalar_to_float_matrix, scalar, array)

void
install_float_conv_ops (void)
{
  INSTALL_CONVOP (octave_int8_matrix, octave_float_matrix,
                  int8_matrix_to_float_matrix);
  INSTALL_CONVOP (octave_int16_matrix, octave_float_matrix,
                  int16_matrix_to_float_matrix);
  INSTALL_CONVOP (octave_int32_matrix, octave_float_matrix,
                  int32_matrix_to_float_matrix);
  INSTALL_CONVOP (octave_int64_matrix, octave_float_matrix,
                  int64_matrix_to_float_matrix);

  INSTALL_CONVOP (octave_uint8_matrix, octave_float_matrix,
                  uint8_matrix_to_float_matrix);
  INSTALL_CONVOP (octave_uint16_matrix, octave_float_matrix,
                  uint16_matrix_to_float_matrix);
  INSTALL_CONVOP (octave_uint32_matrix, octave_float_matrix,
                  uint32_matrix_to_float_matrix);
  INSTALL_CONVOP (octave_uint64_matrix, octave_float_matrix,
                  uint64_matrix_to_float_matrix);

  INSTALL_CONVOP (octave_int8_scalar, octave_float_matrix,
                  int8_scalar_to_float_matrix);
  INSTALL_CONVOP (octave_int16_scalar, octave_float_matrix,
                  int16_scalar_to_float_matrix);
  INSTALL_CONVOP (octave_int32_scalar, octave_float_matrix,
                  int32_scalar_to_float_matrix);
  INSTALL_CONVOP (octave_int64_scalar, octave_float_matrix,
                  int64_scalar_to_float_matrix);

  INSTALL_CONVOP (octave_uint8_scalar, octave_float_matrix,
                  uint8_scalar_to_float_matrix);
  INSTALL_CONVOP (octave_uint16_scalar, octave_float_matrix,
                  uint16_scalar_to_float_matrix);
  INSTALL_CONVOP (octave_uint32_scalar, octave_float_matrix,
                  uint32_scalar_to_float_matrix);
  INSTALL_CONVOP (octave_uint64_scalar, octave_float_matrix,
                  uint64_scalar_to_float_matrix);

  INSTALL_CONVOP (octave_bool_matrix, octave_float_matrix,
                  bool_matrix_to_float_matrix);
  INSTALL_CONVOP (octave_bool, octave_float_matrix,
                  bool_scalar_to_float_matrix);

  INSTALL_CONVOP (octave_range, octave_float_matrix, range_to_float_matrix);

  INSTALL_CONVOP (octave_char_matrix_str, octave_float_matrix,
                  char_matrix_str_to_float_matrix);
  INSTALL_CONVOP (octave_char_matrix_sq_str, octave_float_matrix,
                  char_matrix_sq_str_to_float_matrix);

  INSTALL_CONVOP (octave_scalar, octave_float_matrix,
                  float_scalar_to_float_matrix);
}
