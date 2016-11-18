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
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-flt-complex.h"
#include "ov-complex.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// complex matrix by complex scalar ops.

DEFNDBINOP_OP (add, float_complex_matrix, float_complex,
               float_complex_array, float_complex, +)
DEFNDBINOP_OP (sub, float_complex_matrix, float_complex,
               float_complex_array, float_complex, -)
DEFNDBINOP_OP (mul, float_complex_matrix, float_complex,
               float_complex_array, float_complex, *)

DEFBINOP (div, float_complex_matrix, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex&);

  FloatComplex d = v2.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v1.float_complex_array_value () / d);
}

DEFBINOP_FN (pow, float_complex_matrix, float_complex, xpow)

DEFBINOP (ldiv, float_complex_matrix, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex&);

  FloatComplexMatrix m1 = v1.float_complex_matrix_value ();
  FloatComplexMatrix m2 = v2.float_complex_matrix_value ();
  MatrixType typ = v1.matrix_type ();

  FloatComplexMatrix ret = xleftdiv (m1, m2, typ);
  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, float_complex_matrix, float_complex,
                    float_complex_array, float_complex, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, float_complex_matrix, float_complex,
                    float_complex_array, float_complex, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, float_complex_matrix, float_complex,
                    float_complex_array, float_complex, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, float_complex_matrix, float_complex,
                    float_complex_array, float_complex, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, float_complex_matrix, float_complex,
                    float_complex_array, float_complex, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, float_complex_matrix, float_complex,
                    float_complex_array, float_complex, mx_el_ne)

DEFNDBINOP_OP (el_mul, float_complex_matrix, float_complex,
               float_complex_array, float_complex, *)

DEFBINOP (el_div, float_complex_matrix, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex&);

  FloatComplex d = v2.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v1.float_complex_array_value () / d);
}

DEFNDBINOP_FN (el_pow, float_complex_matrix, float_complex,
               float_complex_array, float_complex, elem_xpow)

DEFBINOP (el_ldiv, float_complex_matrix, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex&);

  return x_el_div (v2.float_complex_value (), v1.float_complex_array_value ());
}

DEFNDBINOP_FN (el_and, float_complex_matrix, float_complex,
               float_complex_array, float_complex, mx_el_and)
DEFNDBINOP_FN (el_or,  float_complex_matrix, float_complex,
               float_complex_array, float_complex, mx_el_or)

DEFNDCATOP_FN (fcm_fcs, float_complex_matrix, float_complex,
               float_complex_array, float_complex_array, concat)

DEFNDCATOP_FN (cm_fcs, complex_matrix, float_complex,
               float_complex_array, float_complex_array, concat)

DEFNDCATOP_FN (fcm_cs, float_complex_matrix, complex,
               float_complex_array, float_complex_array, concat)

DEFNDASSIGNOP_FN (assign, float_complex_matrix, float_complex,
                  float_complex, assign)
DEFNDASSIGNOP_FN (dbl_assign, complex_matrix, float_complex,
                  complex, assign)

DEFNDASSIGNOP_OP (assign_add, float_complex_matrix, float_complex_scalar,
                  float_complex, +=)
DEFNDASSIGNOP_OP (assign_sub, float_complex_matrix, float_complex_scalar,
                  float_complex, -=)
DEFNDASSIGNOP_OP (assign_mul, float_complex_matrix, float_complex_scalar,
                  float_complex, *=)
DEFNDASSIGNOP_OP (assign_div, float_complex_matrix, float_complex_scalar,
                  float_complex, /=)

void
install_fcm_fcs_ops (void)
{
  INSTALL_BINOP (op_add, octave_float_complex_matrix,
                 octave_float_complex, add);
  INSTALL_BINOP (op_sub, octave_float_complex_matrix,
                 octave_float_complex, sub);
  INSTALL_BINOP (op_mul, octave_float_complex_matrix,
                 octave_float_complex, mul);
  INSTALL_BINOP (op_div, octave_float_complex_matrix,
                 octave_float_complex, div);
  INSTALL_BINOP (op_pow, octave_float_complex_matrix,
                 octave_float_complex, pow);
  INSTALL_BINOP (op_ldiv, octave_float_complex_matrix,
                 octave_float_complex, ldiv);
  INSTALL_BINOP (op_lt, octave_float_complex_matrix, octave_float_complex, lt);
  INSTALL_BINOP (op_le, octave_float_complex_matrix, octave_float_complex, le);
  INSTALL_BINOP (op_eq, octave_float_complex_matrix, octave_float_complex, eq);
  INSTALL_BINOP (op_ge, octave_float_complex_matrix, octave_float_complex, ge);
  INSTALL_BINOP (op_gt, octave_float_complex_matrix, octave_float_complex, gt);
  INSTALL_BINOP (op_ne, octave_float_complex_matrix, octave_float_complex, ne);
  INSTALL_BINOP (op_el_mul, octave_float_complex_matrix,
                 octave_float_complex, el_mul);
  INSTALL_BINOP (op_el_div, octave_float_complex_matrix,
                 octave_float_complex, el_div);
  INSTALL_BINOP (op_el_pow, octave_float_complex_matrix,
                 octave_float_complex, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_float_complex_matrix,
                 octave_float_complex, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_float_complex_matrix,
                 octave_float_complex, el_and);
  INSTALL_BINOP (op_el_or, octave_float_complex_matrix,
                 octave_float_complex, el_or);

  INSTALL_CATOP (octave_float_complex_matrix, octave_float_complex, fcm_fcs);
  INSTALL_CATOP (octave_complex_matrix, octave_float_complex, cm_fcs);
  INSTALL_CATOP (octave_float_complex_matrix, octave_complex, fcm_cs);

  INSTALL_ASSIGNOP (op_asn_eq, octave_float_complex_matrix,
                    octave_float_complex, assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_complex_matrix,
                    octave_float_complex, dbl_assign);

  INSTALL_ASSIGNOP (op_add_eq, octave_float_complex_matrix,
                    octave_float_complex_scalar, assign_add);
  INSTALL_ASSIGNOP (op_sub_eq, octave_float_complex_matrix,
                    octave_float_complex_scalar, assign_sub);
  INSTALL_ASSIGNOP (op_mul_eq, octave_float_complex_matrix,
                    octave_float_complex_scalar, assign_mul);
  INSTALL_ASSIGNOP (op_div_eq, octave_float_complex_matrix,
                    octave_float_complex_scalar, assign_div);
}
