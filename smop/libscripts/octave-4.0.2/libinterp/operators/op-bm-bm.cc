/*

Copyright (C) 1996-2015 John W. Eaton

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
#include "ov-bool-mat.h"
#include "ov-scalar.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-re-sparse.h"
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
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// unary bool matrix ops.

DEFNDUNOP_OP (not, bool_matrix, bool_array, !)
DEFNDUNOP_OP (uplus, bool_matrix, array, +)
DEFNDUNOP_OP (uminus, bool_matrix, array, -)

DEFNCUNOP_METHOD (invert, bool_matrix, invert)

DEFUNOP (transpose, bool_matrix)
{
  CAST_UNOP_ARG (const octave_bool_matrix&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.bool_matrix_value ().transpose ());
}

// bool matrix by bool matrix ops.

DEFNDBINOP_FN (eq, bool_matrix, bool_matrix, bool_array, bool_array, mx_el_eq)
DEFNDBINOP_FN (ne, bool_matrix, bool_matrix, bool_array, bool_array, mx_el_ne)

DEFNDBINOP_FN (el_and, bool_matrix, bool_matrix, bool_array, bool_array,
               mx_el_and)

DEFNDBINOP_FN (el_or,  bool_matrix, bool_matrix, bool_array, bool_array,
               mx_el_or)

DEFNDBINOP_FN (el_not_and, bool_matrix, bool_matrix, bool_array, bool_array,
               mx_el_not_and)

DEFNDBINOP_FN (el_not_or,  bool_matrix, bool_matrix, bool_array, bool_array,
               mx_el_not_or)

DEFNDBINOP_FN (el_and_not, bool_matrix, bool_matrix, bool_array, bool_array,
               mx_el_and_not)

DEFNDBINOP_FN (el_or_not,  bool_matrix, bool_matrix, bool_array, bool_array,
               mx_el_or_not)

DEFNDCATOP_FN (bm_bm, bool_matrix, bool_matrix, bool_array, bool_array, concat)
DEFNDCATOP_FN (bm_m, bool_matrix, matrix, array, array, concat)
DEFNDCATOP_FN (m_bm, matrix, bool_matrix, array, array, concat)
DEFNDCATOP_FN (bm_fm, bool_matrix, float_matrix, float_array, float_array,
               concat)
DEFNDCATOP_FN (fm_bm, float_matrix, bool_matrix, float_array, float_array,
               concat)

DEFNDASSIGNOP_FN (assign, bool_matrix, bool_matrix, bool_array, assign)
DEFNDASSIGNOP_FNOP (assign_and, bool_matrix, bool_matrix, bool_array,
                    mx_el_and_assign)
DEFNDASSIGNOP_FNOP (assign_or, bool_matrix, bool_matrix, bool_array,
                    mx_el_or_assign)

DEFNULLASSIGNOP_FN (null_assign, bool_matrix, delete_elements)

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

DEFCONVFN (matrix_to_bool_matrix, matrix, bool)
DEFCONVFN (scalar_to_bool_matrix, scalar, bool)

void
install_bm_bm_ops (void)
{
  INSTALL_UNOP (op_not, octave_bool_matrix, not);
  INSTALL_UNOP (op_uplus, octave_bool_matrix, uplus);
  INSTALL_UNOP (op_uminus, octave_bool_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_bool_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_bool_matrix, transpose);

  INSTALL_NCUNOP (op_not, octave_bool_matrix, invert);

  INSTALL_BINOP (op_eq, octave_bool_matrix, octave_bool_matrix, eq);
  INSTALL_BINOP (op_ne, octave_bool_matrix, octave_bool_matrix, ne);

  INSTALL_BINOP (op_el_and, octave_bool_matrix, octave_bool_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_bool_matrix, octave_bool_matrix, el_or);
  INSTALL_BINOP (op_el_not_and, octave_bool_matrix, octave_bool_matrix,
                 el_not_and);
  INSTALL_BINOP (op_el_not_or, octave_bool_matrix, octave_bool_matrix,
                 el_not_or);
  INSTALL_BINOP (op_el_and_not, octave_bool_matrix, octave_bool_matrix,
                 el_and_not);
  INSTALL_BINOP (op_el_or_not, octave_bool_matrix, octave_bool_matrix,
                 el_or_not);

  INSTALL_CATOP (octave_bool_matrix, octave_bool_matrix, bm_bm);
  INSTALL_CATOP (octave_bool_matrix, octave_matrix, bm_m);
  INSTALL_CATOP (octave_matrix, octave_bool_matrix, m_bm);
  INSTALL_CATOP (octave_bool_matrix, octave_float_matrix, bm_fm);
  INSTALL_CATOP (octave_float_matrix, octave_bool_matrix, fm_bm);

  INSTALL_CONVOP (octave_matrix, octave_bool_matrix, matrix_to_bool_matrix);
  INSTALL_CONVOP (octave_scalar, octave_bool_matrix, scalar_to_bool_matrix);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_bool_matrix, assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_matrix,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_char_matrix_str,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_char_matrix_sq_str,
                    conv_and_assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_range,
                    conv_and_assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_sparse_matrix,
                    conv_and_assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_int8_matrix,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_int16_matrix,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_int32_matrix,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_int64_matrix,
                    conv_and_assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_uint8_matrix,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_uint16_matrix,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_uint32_matrix,
                    conv_and_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_uint64_matrix,
                    conv_and_assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_null_matrix,
                    null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_null_str,
                    null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_null_sq_str,
                    null_assign);

  INSTALL_ASSIGNOP (op_el_and_eq, octave_bool_matrix, octave_bool_matrix,
                    assign_and);
  INSTALL_ASSIGNOP (op_el_or_eq, octave_bool_matrix, octave_bool_matrix,
                    assign_or);
}
