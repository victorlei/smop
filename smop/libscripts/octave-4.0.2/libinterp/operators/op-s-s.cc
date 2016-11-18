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

#include "Array-util.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// scalar unary ops.

DEFUNOP (not, scalar)
{
  CAST_UNOP_ARG (const octave_scalar&);
  double x = v.scalar_value ();
  if (xisnan (x))
    gripe_nan_to_logical_conversion ();
  return octave_value (x == 0.0);
}

DEFUNOP_OP (uplus, scalar, /* no-op */)
DEFUNOP_OP (uminus, scalar, -)
DEFUNOP_OP (transpose, scalar, /* no-op */)
DEFUNOP_OP (hermitian, scalar, /* no-op */)

DEFNCUNOP_METHOD (incr, scalar, increment)
DEFNCUNOP_METHOD (decr, scalar, decrement)

// scalar by scalar ops.

DEFBINOP_OP (add, scalar, scalar, +)
DEFBINOP_OP (sub, scalar, scalar, -)
DEFBINOP_OP (mul, scalar, scalar, *)

DEFBINOP (div, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.double_value () / d);
}

DEFBINOP_FN (pow, scalar, scalar, xpow)

DEFBINOP (ldiv, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.double_value () / d);
}

DEFBINOP_OP (lt, scalar, scalar, <)
DEFBINOP_OP (le, scalar, scalar, <=)
DEFBINOP_OP (eq, scalar, scalar, ==)
DEFBINOP_OP (ge, scalar, scalar, >=)
DEFBINOP_OP (gt, scalar, scalar, >)
DEFBINOP_OP (ne, scalar, scalar, !=)

DEFBINOP_OP (el_mul, scalar, scalar, *)

DEFBINOP (el_div, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.double_value () / d);
}

DEFBINOP_FN (el_pow, scalar, scalar, xpow)

DEFBINOP (el_ldiv, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.double_value () / d);
}

DEFSCALARBOOLOP_OP (el_and, scalar, scalar, &&)
DEFSCALARBOOLOP_OP (el_or, scalar, scalar, ||)

DEFNDCATOP_FN (s_s, scalar, scalar, array, array, concat)

void
install_s_s_ops (void)
{
  INSTALL_UNOP (op_not, octave_scalar, not);
  INSTALL_UNOP (op_uplus, octave_scalar, uplus);
  INSTALL_UNOP (op_uminus, octave_scalar, uminus);
  INSTALL_UNOP (op_transpose, octave_scalar, transpose);
  INSTALL_UNOP (op_hermitian, octave_scalar, hermitian);

  INSTALL_NCUNOP (op_incr, octave_scalar, incr);
  INSTALL_NCUNOP (op_decr, octave_scalar, decr);

  INSTALL_BINOP (op_add, octave_scalar, octave_scalar, add);
  INSTALL_BINOP (op_sub, octave_scalar, octave_scalar, sub);
  INSTALL_BINOP (op_mul, octave_scalar, octave_scalar, mul);
  INSTALL_BINOP (op_div, octave_scalar, octave_scalar, div);
  INSTALL_BINOP (op_pow, octave_scalar, octave_scalar, pow);
  INSTALL_BINOP (op_ldiv, octave_scalar, octave_scalar, ldiv);
  INSTALL_BINOP (op_lt, octave_scalar, octave_scalar, lt);
  INSTALL_BINOP (op_le, octave_scalar, octave_scalar, le);
  INSTALL_BINOP (op_eq, octave_scalar, octave_scalar, eq);
  INSTALL_BINOP (op_ge, octave_scalar, octave_scalar, ge);
  INSTALL_BINOP (op_gt, octave_scalar, octave_scalar, gt);
  INSTALL_BINOP (op_ne, octave_scalar, octave_scalar, ne);
  INSTALL_BINOP (op_el_mul, octave_scalar, octave_scalar, el_mul);
  INSTALL_BINOP (op_el_div, octave_scalar, octave_scalar, el_div);
  INSTALL_BINOP (op_el_pow, octave_scalar, octave_scalar, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_scalar, octave_scalar, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_scalar, octave_scalar, el_and);
  INSTALL_BINOP (op_el_or, octave_scalar, octave_scalar, el_or);

  INSTALL_CATOP (octave_scalar, octave_scalar, s_s);

  INSTALL_ASSIGNCONV (octave_scalar, octave_scalar, octave_matrix);
  INSTALL_ASSIGNCONV (octave_float_scalar, octave_scalar, octave_float_matrix);

  INSTALL_ASSIGNCONV (octave_scalar, octave_null_matrix, octave_matrix);
  INSTALL_ASSIGNCONV (octave_scalar, octave_null_str, octave_matrix);
  INSTALL_ASSIGNCONV (octave_scalar, octave_null_sq_str, octave_matrix);
}
