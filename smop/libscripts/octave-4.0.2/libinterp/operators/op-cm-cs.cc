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
#include "ov-complex.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// complex matrix by complex scalar ops.

DEFNDBINOP_OP (add, complex_matrix, complex, complex_array, complex, +)
DEFNDBINOP_OP (sub, complex_matrix, complex, complex_array, complex, -)
DEFNDBINOP_OP (mul, complex_matrix, complex, complex_array, complex, *)

DEFBINOP (div, complex_matrix, complex)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_array_value () / d);
}

DEFBINOP_FN (pow, complex_matrix, complex, xpow)

DEFBINOP (ldiv, complex_matrix, complex)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_complex&);

  ComplexMatrix m1 = v1.complex_matrix_value ();
  ComplexMatrix m2 = v2.complex_matrix_value ();
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (m1, m2, typ);
  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, complex_matrix, complex, complex_array, complex,
                    mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, complex_matrix, complex, complex_array, complex,
                    mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, complex_matrix, complex, complex_array, complex,
                    mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, complex_matrix, complex, complex_array, complex,
                    mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, complex_matrix, complex, complex_array, complex,
                    mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, complex_matrix, complex, complex_array, complex,
                    mx_el_ne)

DEFNDBINOP_OP (el_mul, complex_matrix, complex, complex_array, complex, *)

DEFBINOP (el_div, complex_matrix, complex)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_array_value () / d);
}

DEFNDBINOP_FN (el_pow, complex_matrix, complex, complex_array, complex,
               elem_xpow)

DEFBINOP (el_ldiv, complex_matrix, complex)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_complex&);

  return x_el_div (v2.complex_value (), v1.complex_array_value ());
}

DEFNDBINOP_FN (el_and, complex_matrix, complex, complex_array, complex,
               mx_el_and)
DEFNDBINOP_FN (el_or,  complex_matrix, complex, complex_array, complex,
               mx_el_or)

DEFNDCATOP_FN (cm_cs, complex_matrix, complex, complex_array, complex_array,
               concat)

DEFNDASSIGNOP_FN (assign, complex_matrix, complex, complex, assign)
DEFNDASSIGNOP_FN (sgl_assign, float_complex_matrix, complex, float_complex,
                  assign)

DEFNDASSIGNOP_OP (assign_add, complex_matrix, complex_scalar, complex, +=)
DEFNDASSIGNOP_OP (assign_sub, complex_matrix, complex_scalar, complex, -=)
DEFNDASSIGNOP_OP (assign_mul, complex_matrix, complex_scalar, complex, *=)
DEFNDASSIGNOP_OP (assign_div, complex_matrix, complex_scalar, complex, /=)

void
install_cm_cs_ops (void)
{
  INSTALL_BINOP (op_add, octave_complex_matrix, octave_complex, add);
  INSTALL_BINOP (op_sub, octave_complex_matrix, octave_complex, sub);
  INSTALL_BINOP (op_mul, octave_complex_matrix, octave_complex, mul);
  INSTALL_BINOP (op_div, octave_complex_matrix, octave_complex, div);
  INSTALL_BINOP (op_pow, octave_complex_matrix, octave_complex, pow);
  INSTALL_BINOP (op_ldiv, octave_complex_matrix, octave_complex, ldiv);
  INSTALL_BINOP (op_lt, octave_complex_matrix, octave_complex, lt);
  INSTALL_BINOP (op_le, octave_complex_matrix, octave_complex, le);
  INSTALL_BINOP (op_eq, octave_complex_matrix, octave_complex, eq);
  INSTALL_BINOP (op_ge, octave_complex_matrix, octave_complex, ge);
  INSTALL_BINOP (op_gt, octave_complex_matrix, octave_complex, gt);
  INSTALL_BINOP (op_ne, octave_complex_matrix, octave_complex, ne);
  INSTALL_BINOP (op_el_mul, octave_complex_matrix, octave_complex, el_mul);
  INSTALL_BINOP (op_el_div, octave_complex_matrix, octave_complex, el_div);
  INSTALL_BINOP (op_el_pow, octave_complex_matrix, octave_complex, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_complex_matrix, octave_complex, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_complex_matrix, octave_complex, el_and);
  INSTALL_BINOP (op_el_or, octave_complex_matrix, octave_complex, el_or);

  INSTALL_CATOP (octave_complex_matrix, octave_complex, cm_cs);

  INSTALL_ASSIGNOP (op_asn_eq, octave_complex_matrix, octave_complex, assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_float_complex_matrix, octave_complex,
                    sgl_assign);

  INSTALL_ASSIGNOP (op_add_eq, octave_complex_matrix, octave_complex_scalar,
                    assign_add);
  INSTALL_ASSIGNOP (op_sub_eq, octave_complex_matrix, octave_complex_scalar,
                    assign_sub);
  INSTALL_ASSIGNOP (op_mul_eq, octave_complex_matrix, octave_complex_scalar,
                    assign_mul);
  INSTALL_ASSIGNOP (op_div_eq, octave_complex_matrix, octave_complex_scalar,
                    assign_div);
}
