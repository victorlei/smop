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
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// unary complex scalar ops.

DEFUNOP (not, complex)
{
  CAST_UNOP_ARG (const octave_complex&);
  Complex x = v.complex_value ();
  if (xisnan (x))
    gripe_nan_to_logical_conversion ();
  return octave_value (x == 0.0);
}

DEFUNOP_OP (uplus, complex, /* no-op */)
DEFUNOP_OP (uminus, complex, -)
DEFUNOP_OP (transpose, complex, /* no-op */)

DEFUNOP (hermitian, complex)
{
  CAST_UNOP_ARG (const octave_complex&);

  return octave_value (conj (v.complex_value ()));
}

DEFNCUNOP_METHOD (incr, complex, increment)
DEFNCUNOP_METHOD (decr, complex, decrement)

// complex scalar by complex scalar ops.

DEFBINOP_OP (add, complex, complex, +)
DEFBINOP_OP (sub, complex, complex, -)
DEFBINOP_OP (mul, complex, complex, *)

DEFBINOP (div, complex, complex)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_value () / d);
}

DEFBINOP_FN (pow, complex, complex, xpow)

DEFBINOP (ldiv, complex, complex)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_complex&);

  Complex d = v1.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.complex_value () / d);
}

DEFCMPLXCMPOP_OP (lt, complex, complex, <)
DEFCMPLXCMPOP_OP (le, complex, complex, <=)
DEFCMPLXCMPOP_OP (eq, complex, complex, ==)
DEFCMPLXCMPOP_OP (ge, complex, complex, >=)
DEFCMPLXCMPOP_OP (gt, complex, complex, >)
DEFCMPLXCMPOP_OP (ne, complex, complex, !=)

DEFBINOP_OP (el_mul, complex, complex, *)

DEFBINOP (el_div, complex, complex)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_value () / d);
}

DEFBINOP_FN (el_pow, complex, complex, xpow)

DEFBINOP (el_ldiv, complex, complex)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_complex&);

  Complex d = v1.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.complex_value () / d);
}

DEFBINOP (el_and, complex, complex)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_complex&);

  return v1.complex_value () != 0.0 && v2.complex_value () != 0.0;
}

DEFBINOP (el_or, complex, complex)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_complex&);

  return v1.complex_value () != 0.0 || v2.complex_value () != 0.0;
}

DEFNDCATOP_FN (cs_cs, complex, complex, complex_array, complex_array, concat)

CONVDECL (complex_to_float_complex)
{
  CAST_CONV_ARG (const octave_complex&);

  return new octave_float_complex_matrix (FloatComplexMatrix (1, 1,
                                          static_cast<FloatComplex>
                                            (v.complex_value ())));
}

void
install_cs_cs_ops (void)
{
  INSTALL_UNOP (op_not, octave_complex, not);
  INSTALL_UNOP (op_uplus, octave_complex, uplus);
  INSTALL_UNOP (op_uminus, octave_complex, uminus);
  INSTALL_UNOP (op_transpose, octave_complex, transpose);
  INSTALL_UNOP (op_hermitian, octave_complex, hermitian);

  INSTALL_NCUNOP (op_incr, octave_complex, incr);
  INSTALL_NCUNOP (op_decr, octave_complex, decr);

  INSTALL_BINOP (op_add, octave_complex, octave_complex, add);
  INSTALL_BINOP (op_sub, octave_complex, octave_complex, sub);
  INSTALL_BINOP (op_mul, octave_complex, octave_complex, mul);
  INSTALL_BINOP (op_div, octave_complex, octave_complex, div);
  INSTALL_BINOP (op_pow, octave_complex, octave_complex, pow);
  INSTALL_BINOP (op_ldiv, octave_complex, octave_complex, ldiv);
  INSTALL_BINOP (op_lt, octave_complex, octave_complex, lt);
  INSTALL_BINOP (op_le, octave_complex, octave_complex, le);
  INSTALL_BINOP (op_eq, octave_complex, octave_complex, eq);
  INSTALL_BINOP (op_ge, octave_complex, octave_complex, ge);
  INSTALL_BINOP (op_gt, octave_complex, octave_complex, gt);
  INSTALL_BINOP (op_ne, octave_complex, octave_complex, ne);
  INSTALL_BINOP (op_el_mul, octave_complex, octave_complex, el_mul);
  INSTALL_BINOP (op_el_div, octave_complex, octave_complex, el_div);
  INSTALL_BINOP (op_el_pow, octave_complex, octave_complex, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_complex, octave_complex, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_complex, octave_complex, el_and);
  INSTALL_BINOP (op_el_or, octave_complex, octave_complex, el_or);

  INSTALL_CATOP (octave_complex, octave_complex, cs_cs);

  INSTALL_ASSIGNCONV (octave_complex, octave_complex, octave_complex_matrix);

  INSTALL_ASSIGNCONV (octave_complex, octave_null_matrix,
                      octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_complex, octave_null_str, octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_complex, octave_null_sq_str,
                      octave_complex_matrix);

  INSTALL_CONVOP (octave_complex, octave_float_complex_matrix,
                  complex_to_float_complex);
}
