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
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// scalar by complex scalar ops.

DEFBINOP_OP (add, scalar, complex, +)
DEFBINOP_OP (sub, scalar, complex, -)
DEFBINOP_OP (mul, scalar, complex, *)

DEFBINOP (div, scalar, complex)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.double_value () / d);
}

DEFBINOP_FN (pow, scalar, complex, xpow)

DEFBINOP (ldiv, scalar, complex)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_complex&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.complex_value () / d);
}

DEFCMPLXCMPOP_OP (lt, scalar, complex, <)
DEFCMPLXCMPOP_OP (le, scalar, complex, <=)
DEFCMPLXCMPOP_OP (eq, scalar, complex, ==)
DEFCMPLXCMPOP_OP (ge, scalar, complex, >=)
DEFCMPLXCMPOP_OP (gt, scalar, complex, >)
DEFCMPLXCMPOP_OP (ne, scalar, complex, !=)

DEFBINOP_OP (el_mul, scalar, complex, *)

DEFBINOP (el_div, scalar, complex)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.double_value () / d);
}

DEFBINOP_FN (el_pow, scalar, complex, xpow)

DEFBINOP (el_ldiv, scalar, complex)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_complex&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.complex_value () / d);
}

DEFBINOP (el_and, scalar, complex)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_complex&);

  return octave_value (v1.double_value () && (v2.complex_value () != 0.0));
}

DEFBINOP (el_or, scalar, complex)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_complex&);

  return octave_value (v1.double_value () || (v2.complex_value () != 0.0));
}

DEFNDCATOP_FN (s_cs, scalar, complex, array, complex_array, concat)

void
install_s_cs_ops (void)
{
  INSTALL_BINOP (op_add, octave_scalar, octave_complex, add);
  INSTALL_BINOP (op_sub, octave_scalar, octave_complex, sub);
  INSTALL_BINOP (op_mul, octave_scalar, octave_complex, mul);
  INSTALL_BINOP (op_div, octave_scalar, octave_complex, div);
  INSTALL_BINOP (op_pow, octave_scalar, octave_complex, pow);
  INSTALL_BINOP (op_ldiv, octave_scalar, octave_complex, ldiv);
  INSTALL_BINOP (op_lt, octave_scalar, octave_complex, lt);
  INSTALL_BINOP (op_le, octave_scalar, octave_complex, le);
  INSTALL_BINOP (op_eq, octave_scalar, octave_complex, eq);
  INSTALL_BINOP (op_ge, octave_scalar, octave_complex, ge);
  INSTALL_BINOP (op_gt, octave_scalar, octave_complex, gt);
  INSTALL_BINOP (op_ne, octave_scalar, octave_complex, ne);
  INSTALL_BINOP (op_el_mul, octave_scalar, octave_complex, el_mul);
  INSTALL_BINOP (op_el_div, octave_scalar, octave_complex, el_div);
  INSTALL_BINOP (op_el_pow, octave_scalar, octave_complex, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_scalar, octave_complex, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_scalar, octave_complex, el_and);
  INSTALL_BINOP (op_el_or, octave_scalar, octave_complex, el_or);

  INSTALL_CATOP (octave_scalar, octave_complex, s_cs);

  INSTALL_ASSIGNCONV (octave_scalar, octave_complex, octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_float_scalar, octave_complex,
                      octave_float_complex_matrix);
}
