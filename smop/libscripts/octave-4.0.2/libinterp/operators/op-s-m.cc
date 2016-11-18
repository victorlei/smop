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
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// scalar by matrix ops.

DEFNDBINOP_OP (add, scalar, matrix, scalar, array, +)
DEFNDBINOP_OP (sub, scalar, matrix, scalar, array, -)
DEFNDBINOP_OP (mul, scalar, matrix, scalar, array, *)

DEFBINOP (div, scalar, matrix)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_matrix&);

  Matrix m1 = v1.matrix_value ();
  Matrix m2 = v2.matrix_value ();
  MatrixType typ = v2.matrix_type ();

  Matrix ret = xdiv (m1, m2, typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOP_FN (pow, scalar, matrix, xpow)

DEFBINOP (ldiv, scalar, matrix)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_matrix&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.array_value () / d);
}

DEFNDBINOP_FN (lt, scalar, matrix, scalar, array, mx_el_lt)
DEFNDBINOP_FN (le, scalar, matrix, scalar, array, mx_el_le)
DEFNDBINOP_FN (eq, scalar, matrix, scalar, array, mx_el_eq)
DEFNDBINOP_FN (ge, scalar, matrix, scalar, array, mx_el_ge)
DEFNDBINOP_FN (gt, scalar, matrix, scalar, array, mx_el_gt)
DEFNDBINOP_FN (ne, scalar, matrix, scalar, array, mx_el_ne)

DEFNDBINOP_OP (el_mul, scalar, matrix, scalar, array, *)
DEFNDBINOP_FN (el_div, scalar, matrix, scalar, array, x_el_div)
DEFNDBINOP_FN (el_pow, scalar, matrix, scalar, array, elem_xpow)

DEFBINOP (el_ldiv, scalar, matrix)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_matrix&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.array_value () / d);
}

DEFNDBINOP_FN (el_and, scalar, matrix, scalar, array, mx_el_and)
DEFNDBINOP_FN (el_or,  scalar, matrix, scalar, array, mx_el_or)

DEFNDCATOP_FN (s_m, scalar, matrix, array, array, concat)

DEFCONV (matrix_conv, scalar, matrix)
{
  CAST_CONV_ARG (const octave_scalar&);

  return new octave_matrix (v.matrix_value ());
}

void
install_s_m_ops (void)
{
  INSTALL_BINOP (op_add, octave_scalar, octave_matrix, add);
  INSTALL_BINOP (op_sub, octave_scalar, octave_matrix, sub);
  INSTALL_BINOP (op_mul, octave_scalar, octave_matrix, mul);
  INSTALL_BINOP (op_div, octave_scalar, octave_matrix, div);
  INSTALL_BINOP (op_pow, octave_scalar, octave_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_scalar, octave_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_scalar, octave_matrix, lt);
  INSTALL_BINOP (op_le, octave_scalar, octave_matrix, le);
  INSTALL_BINOP (op_eq, octave_scalar, octave_matrix, eq);
  INSTALL_BINOP (op_ge, octave_scalar, octave_matrix, ge);
  INSTALL_BINOP (op_gt, octave_scalar, octave_matrix, gt);
  INSTALL_BINOP (op_ne, octave_scalar, octave_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_scalar, octave_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_scalar, octave_matrix, el_div);
  INSTALL_BINOP (op_el_pow, octave_scalar, octave_matrix, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_scalar, octave_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_scalar, octave_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_scalar, octave_matrix, el_or);

  INSTALL_CATOP (octave_scalar, octave_matrix, s_m);

  INSTALL_ASSIGNCONV (octave_scalar, octave_matrix, octave_matrix);
  INSTALL_ASSIGNCONV (octave_float_scalar, octave_matrix, octave_float_matrix);

  INSTALL_WIDENOP (octave_scalar, octave_matrix, matrix_conv);
}
