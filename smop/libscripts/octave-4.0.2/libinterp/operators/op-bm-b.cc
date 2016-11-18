/*

Copyright (C) 2001-2015 Cai Jianming

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
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-str-mat.h"
#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// bool matrix by bool ops.

DEFNDBINOP_FN (el_and, bool_matrix, bool, bool_array, bool, mx_el_and)
DEFNDBINOP_FN (el_or, bool_matrix, bool, bool_array, bool, mx_el_or)

DEFNDBINOP_FN (el_not_and, bool_matrix, bool, bool_array, bool, mx_el_not_and)
DEFNDBINOP_FN (el_not_or, bool_matrix, bool, bool_array, bool, mx_el_not_or)

DEFNDCATOP_FN (bm_b, bool_matrix, bool, bool_array, bool_array, concat)
DEFNDCATOP_FN (bm_s, bool_matrix, scalar, array, array, concat)
DEFNDCATOP_FN (m_b, matrix, bool, array, array, concat)
DEFNDCATOP_FN (bm_f, bool_matrix, float_scalar, float_array, float_array,
               concat)
DEFNDCATOP_FN (fm_b, float_matrix, bool, float_array, float_array, concat)

DEFNDASSIGNOP_FN (assign, bool_matrix, bool, bool_array, assign)

static octave_value
oct_assignop_conv_and_assign (octave_base_value& a1,
                              const octave_value_list& idx,
                              const octave_base_value& a2)
{
  octave_bool_matrix& v1 = dynamic_cast<octave_bool_matrix&> (a1);

  // FIXME: perhaps add a warning for this conversion
  //        if the values are not all 0 or 1?

  boolNDArray v2 = a2.bool_array_value (true);

  if (! error_state)
    v1.assign (idx, v2);

  return octave_value ();
}

void
install_bm_b_ops (void)
{
  INSTALL_BINOP (op_el_and, octave_bool_matrix, octave_bool, el_and);
  INSTALL_BINOP (op_el_or, octave_bool_matrix, octave_bool, el_or);
  INSTALL_BINOP (op_el_not_and, octave_bool_matrix, octave_bool, el_not_and);
  INSTALL_BINOP (op_el_not_or, octave_bool_matrix, octave_bool, el_not_or);

  INSTALL_CATOP (octave_bool_matrix, octave_bool, bm_b);
  INSTALL_CATOP (octave_bool_matrix, octave_scalar, bm_s);
  INSTALL_CATOP (octave_matrix, octave_bool, m_b);
  INSTALL_CATOP (octave_bool_matrix, octave_float_scalar, bm_f);
  INSTALL_CATOP (octave_float_matrix, octave_bool, fm_b);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_bool, assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_scalar,
                    conv_and_assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_int8_scalar,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_int16_scalar,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_int32_scalar,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_int64_scalar,
                    conv_and_assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_uint8_scalar,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_uint16_scalar,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_uint32_scalar,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_uint64_scalar,
                    conv_and_assign);
}
