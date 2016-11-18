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
#include "ov-flt-complex.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// unary complex scalar ops.

DEFUNOP (not, float_complex)
{
  CAST_UNOP_ARG (const octave_float_complex&);
  FloatComplex x = v.float_complex_value ();
  if (xisnan (x))
    gripe_nan_to_logical_conversion ();
  return octave_value (x == 0.0f);
}

DEFUNOP_OP (uplus, float_complex, /* no-op */)
DEFUNOP_OP (uminus, float_complex, -)
DEFUNOP_OP (transpose, float_complex, /* no-op */)

DEFUNOP (hermitian, float_complex)
{
  CAST_UNOP_ARG (const octave_float_complex&);

  return octave_value (conj (v.float_complex_value ()));
}

DEFNCUNOP_METHOD (incr, float_complex, increment)
DEFNCUNOP_METHOD (decr, float_complex, decrement)

// complex scalar by complex scalar ops.

DEFBINOP_OP (add, float_complex, float_complex, +)
DEFBINOP_OP (sub, float_complex, float_complex, -)
DEFBINOP_OP (mul, float_complex, float_complex, *)

DEFBINOP (div, float_complex, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_complex&);

  FloatComplex d = v2.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v1.float_complex_value () / d);
}

DEFBINOP_FN (pow, float_complex, float_complex, xpow)

DEFBINOP (ldiv, float_complex, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_complex&);

  FloatComplex d = v1.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v2.float_complex_value () / d);
}

DEFCMPLXCMPOP_OP (lt, float_complex, float_complex, <)
DEFCMPLXCMPOP_OP (le, float_complex, float_complex, <=)
DEFCMPLXCMPOP_OP (eq, float_complex, float_complex, ==)
DEFCMPLXCMPOP_OP (ge, float_complex, float_complex, >=)
DEFCMPLXCMPOP_OP (gt, float_complex, float_complex, >)
DEFCMPLXCMPOP_OP (ne, float_complex, float_complex, !=)

DEFBINOP_OP (el_mul, float_complex, float_complex, *)

DEFBINOP (el_div, float_complex, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_complex&);

  FloatComplex d = v2.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v1.float_complex_value () / d);
}

DEFBINOP_FN (el_pow, float_complex, float_complex, xpow)

DEFBINOP (el_ldiv, float_complex, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_complex&);

  FloatComplex d = v1.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v2.float_complex_value () / d);
}

DEFBINOP (el_and, float_complex, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_complex&);

  return (v1.float_complex_value () != 0.0f
          && v2.float_complex_value () != 0.0f);
}

DEFBINOP (el_or, float_complex, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_complex&);

  return (v1.float_complex_value () != 0.0f
          || v2.float_complex_value () != 0.0f);
}

DEFNDCATOP_FN (fcs_fcs, float_complex, float_complex, float_complex_array,
               float_complex_array, concat)

DEFNDCATOP_FN (cs_fcs, complex, float_complex, float_complex_array,
               float_complex_array, concat)

DEFNDCATOP_FN (fcs_cs, float_complex, complex, float_complex_array,
               float_complex_array, concat)

CONVDECL (float_complex_to_complex)
{
  CAST_CONV_ARG (const octave_float_complex&);

  return new octave_complex_matrix
               (ComplexMatrix (1, 1,
                               static_cast<Complex>(v.float_complex_value ())));
}

void
install_fcs_fcs_ops (void)
{
  INSTALL_UNOP (op_not, octave_float_complex, not);
  INSTALL_UNOP (op_uplus, octave_float_complex, uplus);
  INSTALL_UNOP (op_uminus, octave_float_complex, uminus);
  INSTALL_UNOP (op_transpose, octave_float_complex, transpose);
  INSTALL_UNOP (op_hermitian, octave_float_complex, hermitian);

  INSTALL_NCUNOP (op_incr, octave_float_complex, incr);
  INSTALL_NCUNOP (op_decr, octave_float_complex, decr);

  INSTALL_BINOP (op_add, octave_float_complex, octave_float_complex, add);
  INSTALL_BINOP (op_sub, octave_float_complex, octave_float_complex, sub);
  INSTALL_BINOP (op_mul, octave_float_complex, octave_float_complex, mul);
  INSTALL_BINOP (op_div, octave_float_complex, octave_float_complex, div);
  INSTALL_BINOP (op_pow, octave_float_complex, octave_float_complex, pow);
  INSTALL_BINOP (op_ldiv, octave_float_complex, octave_float_complex, ldiv);
  INSTALL_BINOP (op_lt, octave_float_complex, octave_float_complex, lt);
  INSTALL_BINOP (op_le, octave_float_complex, octave_float_complex, le);
  INSTALL_BINOP (op_eq, octave_float_complex, octave_float_complex, eq);
  INSTALL_BINOP (op_ge, octave_float_complex, octave_float_complex, ge);
  INSTALL_BINOP (op_gt, octave_float_complex, octave_float_complex, gt);
  INSTALL_BINOP (op_ne, octave_float_complex, octave_float_complex, ne);
  INSTALL_BINOP (op_el_mul, octave_float_complex, octave_float_complex, el_mul);
  INSTALL_BINOP (op_el_div, octave_float_complex, octave_float_complex, el_div);
  INSTALL_BINOP (op_el_pow, octave_float_complex, octave_float_complex, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_float_complex, octave_float_complex,
                 el_ldiv);
  INSTALL_BINOP (op_el_and, octave_float_complex, octave_float_complex, el_and);
  INSTALL_BINOP (op_el_or, octave_float_complex, octave_float_complex, el_or);

  INSTALL_CATOP (octave_float_complex, octave_float_complex, fcs_fcs);
  INSTALL_CATOP (octave_complex, octave_float_complex, cs_fcs);
  INSTALL_CATOP (octave_float_complex, octave_complex, fcs_cs);

  INSTALL_ASSIGNCONV (octave_float_complex, octave_float_complex,
                      octave_float_complex_matrix);

  INSTALL_ASSIGNCONV (octave_complex, octave_float_complex,
                      octave_complex_matrix);

  INSTALL_ASSIGNCONV (octave_float_complex, octave_null_matrix,
                      octave_float_complex_matrix);
  INSTALL_ASSIGNCONV (octave_float_complex, octave_null_str,
                      octave_float_complex_matrix);
  INSTALL_ASSIGNCONV (octave_float_complex, octave_null_sq_str,
                      octave_float_complex_matrix);

  INSTALL_CONVOP (octave_float_complex, octave_complex_matrix,
                  float_complex_to_complex);
}
