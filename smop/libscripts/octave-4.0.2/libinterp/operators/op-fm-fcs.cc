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

#include "mx-fm-fcs.h"
#include "mx-fcs-fm.h"
#include "mx-fnda-fcs.h"
#include "mx-fcs-fnda.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-flt-complex.h"
#include "ov-complex.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// matrix by complex scalar ops.

DEFNDBINOP_OP (add, float_matrix, float_complex, float_array, float_complex, +)
DEFNDBINOP_OP (sub, float_matrix, float_complex, float_array, float_complex, -)
DEFNDBINOP_OP (mul, float_matrix, float_complex, float_array, float_complex, *)

DEFBINOP (div, float_matrix, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_matrix&, const octave_float_complex&);

  FloatComplex d = v2.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v1.float_array_value () / d);
}

DEFBINOP_FN (pow, float_matrix, float_complex, xpow)

DEFBINOP (ldiv, float_matrix, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_matrix&, const octave_float_complex&);

  FloatMatrix m1 = v1.float_matrix_value ();
  FloatComplexMatrix m2 = v2.float_complex_matrix_value ();
  MatrixType typ = v1.matrix_type ();

  FloatComplexMatrix ret = xleftdiv (m1, m2, typ);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, float_matrix, float_complex, float_array,
                    float_complex, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, float_matrix, float_complex, float_array,
                    float_complex, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, float_matrix, float_complex, float_array,
                    float_complex, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, float_matrix, float_complex, float_array,
                    float_complex, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, float_matrix, float_complex, float_array,
                    float_complex, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, float_matrix, float_complex, float_array,
                    float_complex, mx_el_ne)

DEFNDBINOP_OP (el_mul, float_matrix, float_complex, float_array,
               float_complex, *)

DEFBINOP (el_div, float_matrix, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_matrix&, const octave_float_complex&);

  FloatComplex d = v2.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v1.float_array_value () / d);
}

DEFNDBINOP_FN (el_pow, float_matrix, float_complex, float_array,
               float_complex, elem_xpow)

DEFBINOP (el_ldiv, float_matrix, flaot_complex)
{
  CAST_BINOP_ARGS (const octave_float_matrix&, const octave_float_complex&);

  return x_el_div (v2.float_complex_value (), v1.float_array_value ());
}

DEFNDBINOP_FN (el_and, float_matrix, float_complex, float_array,
               float_complex, mx_el_and)
DEFNDBINOP_FN (el_or, float_matrix, float_complex, float_array,
               float_complex, mx_el_or)

DEFNDCATOP_FN (fm_fcs, float_matrix, float_complex, float_array,
               float_complex_array, concat)

DEFNDCATOP_FN (m_fcs, matrix, float_complex, float_array,
               float_complex_array, concat)

DEFNDCATOP_FN (fm_cs, float_matrix, complex, float_array,
               float_complex_array, concat)

void
install_fm_fcs_ops (void)
{
  INSTALL_BINOP (op_add, octave_float_matrix, octave_float_complex, add);
  INSTALL_BINOP (op_sub, octave_float_matrix, octave_float_complex, sub);
  INSTALL_BINOP (op_mul, octave_float_matrix, octave_float_complex, mul);
  INSTALL_BINOP (op_div, octave_float_matrix, octave_float_complex, div);
  INSTALL_BINOP (op_pow, octave_float_matrix, octave_float_complex, pow);
  INSTALL_BINOP (op_ldiv, octave_float_matrix, octave_float_complex, ldiv);
  INSTALL_BINOP (op_lt, octave_float_matrix, octave_float_complex, lt);
  INSTALL_BINOP (op_le, octave_float_matrix, octave_float_complex, le);
  INSTALL_BINOP (op_eq, octave_float_matrix, octave_float_complex, eq);
  INSTALL_BINOP (op_ge, octave_float_matrix, octave_float_complex, ge);
  INSTALL_BINOP (op_gt, octave_float_matrix, octave_float_complex, gt);
  INSTALL_BINOP (op_ne, octave_float_matrix, octave_float_complex, ne);
  INSTALL_BINOP (op_el_mul, octave_float_matrix, octave_float_complex, el_mul);
  INSTALL_BINOP (op_el_div, octave_float_matrix, octave_float_complex, el_div);
  INSTALL_BINOP (op_el_pow, octave_float_matrix, octave_float_complex, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_float_matrix, octave_float_complex,
                 el_ldiv);
  INSTALL_BINOP (op_el_and, octave_float_matrix, octave_float_complex, el_and);
  INSTALL_BINOP (op_el_or, octave_float_matrix, octave_float_complex, el_or);

  INSTALL_CATOP (octave_float_matrix, octave_float_complex, fm_fcs);
  INSTALL_CATOP (octave_matrix, octave_float_complex, m_fcs);
  INSTALL_CATOP (octave_float_matrix, octave_complex, fm_cs);

  INSTALL_ASSIGNCONV (octave_float_matrix, octave_float_complex,
                      octave_float_complex_matrix);
  INSTALL_ASSIGNCONV (octave_matrix, octave_float_complex,
                      octave_complex_matrix);
}
