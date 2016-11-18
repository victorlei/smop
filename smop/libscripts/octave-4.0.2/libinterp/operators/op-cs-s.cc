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
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// complex scalar by scalar ops.

DEFBINOP_OP (add, complex, scalar, +)
DEFBINOP_OP (sub, complex, scalar, -)
DEFBINOP_OP (mul, complex, scalar, *)

DEFBINOP (div, complex, scalar)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_value () / d);
}

DEFBINOP_FN (pow, complex, scalar, xpow)

DEFBINOP (ldiv, complex, scalar)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_scalar&);

  Complex d = v1.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.double_value () / d);
}

DEFCMPLXCMPOP_OP (lt, complex, scalar, <)
DEFCMPLXCMPOP_OP (le, complex, scalar, <=)
DEFCMPLXCMPOP_OP (eq, complex, scalar, ==)
DEFCMPLXCMPOP_OP (ge, complex, scalar, >=)
DEFCMPLXCMPOP_OP (gt, complex, scalar, >)
DEFCMPLXCMPOP_OP (ne, complex, scalar, !=)

DEFBINOP_OP (el_mul, complex, scalar, *)

DEFBINOP (el_div, complex, scalar)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_value () / d);
}

DEFBINOP_FN (el_pow, complex, scalar, xpow)

DEFBINOP (el_ldiv, complex, scalar)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_scalar&);

  Complex d = v1.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.double_value () / d);
}

DEFBINOP (el_and, complex, scalar)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_scalar&);

  return v1.complex_value () != 0.0 && v2.double_value ();
}

DEFBINOP (el_or, complex, scalar)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_scalar&);

  return v1.complex_value () != 0.0 || v2.double_value ();
}

DEFNDCATOP_FN (cs_s, complex, scalar, complex_array, array, concat)

void
install_cs_s_ops (void)
{
  INSTALL_BINOP (op_add, octave_complex, octave_scalar, add);
  INSTALL_BINOP (op_sub, octave_complex, octave_scalar, sub);
  INSTALL_BINOP (op_mul, octave_complex, octave_scalar, mul);
  INSTALL_BINOP (op_div, octave_complex, octave_scalar, div);
  INSTALL_BINOP (op_pow, octave_complex, octave_scalar, pow);
  INSTALL_BINOP (op_ldiv, octave_complex, octave_scalar, ldiv);
  INSTALL_BINOP (op_lt, octave_complex, octave_scalar, lt);
  INSTALL_BINOP (op_le, octave_complex, octave_scalar, le);
  INSTALL_BINOP (op_eq, octave_complex, octave_scalar, eq);
  INSTALL_BINOP (op_ge, octave_complex, octave_scalar, ge);
  INSTALL_BINOP (op_gt, octave_complex, octave_scalar, gt);
  INSTALL_BINOP (op_ne, octave_complex, octave_scalar, ne);
  INSTALL_BINOP (op_el_mul, octave_complex, octave_scalar, el_mul);
  INSTALL_BINOP (op_el_div, octave_complex, octave_scalar, el_div);
  INSTALL_BINOP (op_el_pow, octave_complex, octave_scalar, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_complex, octave_scalar, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_complex, octave_scalar, el_and);
  INSTALL_BINOP (op_el_or, octave_complex, octave_scalar, el_or);

  INSTALL_CATOP (octave_complex, octave_scalar, cs_s);

  INSTALL_ASSIGNCONV (octave_complex, octave_scalar, octave_complex_matrix);
}
