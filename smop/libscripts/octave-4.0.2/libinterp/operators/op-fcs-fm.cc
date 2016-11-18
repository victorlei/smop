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

#include "mx-fcs-fnda.h"
#include "mx-fnda-fcs.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// complex scalar by matrix ops.

DEFNDBINOP_OP (add, float_complex, float_matrix, float_complex, float_array, +)
DEFNDBINOP_OP (sub, float_complex, float_matrix, float_complex, float_array, -)
DEFNDBINOP_OP (mul, float_complex, float_matrix, float_complex, float_array, *)

DEFBINOP (div, float_complex, float_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_matrix&);

  FloatComplexMatrix m1 = v1.float_complex_matrix_value ();
  FloatMatrix m2 = v2.float_matrix_value ();
  MatrixType typ = v2.matrix_type ();

  FloatComplexMatrix ret = xdiv (m1, m2, typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOP_FN (pow, float_complex, float_matrix, xpow)

DEFBINOP (ldiv, float_complex, float_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_matrix&);

  FloatComplex d = v1.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v2.float_array_value () / d);
}

DEFNDCMPLXCMPOP_FN (lt, float_complex, float_matrix, float_complex,
                    float_array, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, float_complex, float_matrix, float_complex,
                    float_array, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, float_complex, float_matrix, float_complex,
                    float_array, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, float_complex, float_matrix, float_complex,
                    float_array, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, float_complex, float_matrix, float_complex,
                    float_array, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, float_complex, float_matrix, float_complex,
                    float_array, mx_el_ne)

DEFNDBINOP_OP (el_mul, float_complex, float_matrix, float_complex,
               float_array, *)
DEFNDBINOP_FN (el_div, float_complex, float_matrix, float_complex,
               float_array, x_el_div)
DEFNDBINOP_FN (el_pow, float_complex, float_matrix, float_complex,
               float_array, elem_xpow)

DEFBINOP (el_ldiv, float_complex, float_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_matrix&);

  FloatComplex d = v1.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v2.float_array_value () / d);
}

DEFNDBINOP_FN (el_and, float_complex, float_matrix, float_complex,
               float_array, mx_el_and)
DEFNDBINOP_FN (el_or,  float_complex, float_matrix, float_complex,
               float_array, mx_el_or)

DEFNDCATOP_FN (fcs_fm, float_complex, float_matrix, float_complex_array,
               float_array, concat)

DEFNDCATOP_FN (cs_fm, complex, float_matrix, float_complex_array,
               float_array, concat)

DEFNDCATOP_FN (fcs_m, float_complex, matrix, float_complex_array,
               float_array, concat)

void
install_fcs_fm_ops (void)
{
  INSTALL_BINOP (op_add, octave_float_complex, octave_float_matrix, add);
  INSTALL_BINOP (op_sub, octave_float_complex, octave_float_matrix, sub);
  INSTALL_BINOP (op_mul, octave_float_complex, octave_float_matrix, mul);
  INSTALL_BINOP (op_div, octave_float_complex, octave_float_matrix, div);
  INSTALL_BINOP (op_pow, octave_float_complex, octave_float_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_float_complex, octave_float_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_float_complex, octave_float_matrix, lt);
  INSTALL_BINOP (op_le, octave_float_complex, octave_float_matrix, le);
  INSTALL_BINOP (op_eq, octave_float_complex, octave_float_matrix, eq);
  INSTALL_BINOP (op_ge, octave_float_complex, octave_float_matrix, ge);
  INSTALL_BINOP (op_gt, octave_float_complex, octave_float_matrix, gt);
  INSTALL_BINOP (op_ne, octave_float_complex, octave_float_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_float_complex, octave_float_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_float_complex, octave_float_matrix, el_div);
  INSTALL_BINOP (op_el_pow, octave_float_complex, octave_float_matrix, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_float_complex, octave_float_matrix,
                 el_ldiv);
  INSTALL_BINOP (op_el_and, octave_float_complex, octave_float_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_float_complex, octave_float_matrix, el_or);

  INSTALL_CATOP (octave_float_complex, octave_float_matrix, fcs_fm);
  INSTALL_CATOP (octave_complex, octave_float_matrix, cs_fm);
  INSTALL_CATOP (octave_float_complex, octave_matrix, fcs_m);

  INSTALL_ASSIGNCONV (octave_float_complex, octave_float_matrix,
                      octave_float_complex_matrix);
  INSTALL_ASSIGNCONV (octave_complex, octave_float_matrix,
                      octave_complex_matrix);
}
