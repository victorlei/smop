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

#include "mx-cm-s.h"
#include "mx-cnda-s.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// complex matrix by scalar ops.

DEFNDBINOP_OP (add, complex_matrix, scalar, complex_array, scalar, +)
DEFNDBINOP_OP (sub, complex_matrix, scalar, complex_array, scalar, -)
DEFNDBINOP_OP (mul, complex_matrix, scalar, complex_array, scalar, *)

DEFBINOP (div, complex_matrix, scalar)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_array_value () / d);
}

DEFBINOP_FN (pow, complex_matrix, scalar, xpow)

DEFBINOP (ldiv, complex_matrix, scalar)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  ComplexMatrix m1 = v1.complex_matrix_value ();
  Matrix m2 = v2.matrix_value ();
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (m1, m2, typ);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, complex_matrix, scalar, complex_array, scalar, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, complex_matrix, scalar, complex_array, scalar, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, complex_matrix, scalar, complex_array, scalar, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, complex_matrix, scalar, complex_array, scalar, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, complex_matrix, scalar, complex_array, scalar, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, complex_matrix, scalar, complex_array, scalar, mx_el_ne)

DEFNDBINOP_OP (el_mul, complex_matrix, scalar, complex_array, scalar, *)

DEFBINOP (el_div, complex_matrix, scalar)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_array_value () / d);
}

DEFNDBINOP_FN (el_pow, complex_matrix, scalar, complex_array, scalar, elem_xpow)

DEFBINOP (el_ldiv, complex_matrix, scalar)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  return x_el_div (v2.double_value (), v1.complex_array_value ());
}

DEFNDBINOP_FN (el_and, complex_matrix, scalar, complex_array, scalar, mx_el_and)
DEFNDBINOP_FN (el_or,  complex_matrix, scalar, complex_array, scalar, mx_el_or)

DEFNDCATOP_FN (cm_s, complex_matrix, scalar, complex_array, array, concat)

DEFNDASSIGNOP_FN (assign, complex_matrix, scalar, complex_array, assign)

DEFNDASSIGNOP_OP (assign_mul, complex_matrix, scalar, scalar, *=)
DEFNDASSIGNOP_OP (assign_div, complex_matrix, scalar, scalar, /=)

void
install_cm_s_ops (void)
{
  INSTALL_BINOP (op_add, octave_complex_matrix, octave_scalar, add);
  INSTALL_BINOP (op_sub, octave_complex_matrix, octave_scalar, sub);
  INSTALL_BINOP (op_mul, octave_complex_matrix, octave_scalar, mul);
  INSTALL_BINOP (op_div, octave_complex_matrix, octave_scalar, div);
  INSTALL_BINOP (op_pow, octave_complex_matrix, octave_scalar, pow);
  INSTALL_BINOP (op_ldiv, octave_complex_matrix, octave_scalar, ldiv);
  INSTALL_BINOP (op_lt, octave_complex_matrix, octave_scalar, lt);
  INSTALL_BINOP (op_le, octave_complex_matrix, octave_scalar, le);
  INSTALL_BINOP (op_eq, octave_complex_matrix, octave_scalar, eq);
  INSTALL_BINOP (op_ge, octave_complex_matrix, octave_scalar, ge);
  INSTALL_BINOP (op_gt, octave_complex_matrix, octave_scalar, gt);
  INSTALL_BINOP (op_ne, octave_complex_matrix, octave_scalar, ne);
  INSTALL_BINOP (op_el_mul, octave_complex_matrix, octave_scalar, el_mul);
  INSTALL_BINOP (op_el_div, octave_complex_matrix, octave_scalar, el_div);
  INSTALL_BINOP (op_el_pow, octave_complex_matrix, octave_scalar, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_complex_matrix, octave_scalar, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_complex_matrix, octave_scalar, el_and);
  INSTALL_BINOP (op_el_or, octave_complex_matrix, octave_scalar, el_or);

  INSTALL_CATOP (octave_complex_matrix, octave_scalar, cm_s);

  INSTALL_ASSIGNOP (op_asn_eq, octave_complex_matrix, octave_scalar, assign);

  INSTALL_ASSIGNOP (op_mul_eq, octave_complex_matrix, octave_scalar,
                    assign_mul);
  INSTALL_ASSIGNOP (op_div_eq, octave_complex_matrix, octave_scalar,
                    assign_div);
}
