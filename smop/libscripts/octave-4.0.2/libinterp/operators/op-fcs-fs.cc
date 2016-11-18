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
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-float.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// complex scalar by scalar ops.

DEFBINOP_OP (add, float_complex, float_scalar, +)
DEFBINOP_OP (sub, float_complex, float_scalar, -)
DEFBINOP_OP (mul, float_complex, float_scalar, *)

DEFBINOP (div, float_complex, float)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_scalar&);

  float d = v2.float_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.float_complex_value () / d);
}

DEFBINOP_FN (pow, float_complex, float_scalar, xpow)

DEFBINOP (ldiv, float_complex, float)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_scalar&);

  FloatComplex d = v1.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v2.float_value () / d);
}

DEFCMPLXCMPOP_OP (lt, float_complex, float_scalar, <)
DEFCMPLXCMPOP_OP (le, float_complex, float_scalar, <=)
DEFCMPLXCMPOP_OP (eq, float_complex, float_scalar, ==)
DEFCMPLXCMPOP_OP (ge, float_complex, float_scalar, >=)
DEFCMPLXCMPOP_OP (gt, float_complex, float_scalar, >)
DEFCMPLXCMPOP_OP (ne, float_complex, float_scalar, !=)

DEFBINOP_OP (el_mul, float_complex, float_scalar, *)

DEFBINOP (el_div, float_complex, float)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_scalar&);

  float d = v2.float_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.float_complex_value () / d);
}

DEFBINOP_FN (el_pow, float_complex, float_scalar, xpow)

DEFBINOP (el_ldiv, float_complex, float)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_scalar&);

  FloatComplex d = v1.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v2.float_value () / d);
}

DEFBINOP (el_and, float_complex, float)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_scalar&);

  return (v1.float_complex_value () != 0.0f && v2.float_value ());
}

DEFBINOP (el_or, float_complex, float)
{
  CAST_BINOP_ARGS (const octave_float_complex&, const octave_float_scalar&);

  return (v1.float_complex_value () != 0.0f || v2.float_value ());
}

DEFNDCATOP_FN (fcs_fs, float_complex, float_scalar, float_complex_array,
               float_array, concat)

DEFNDCATOP_FN (cs_fs, complex, float_scalar, float_complex_array,
               float_array, concat)

DEFNDCATOP_FN (fcs_s, float_complex, scalar, float_complex_array,
               float_array, concat)

void
install_fcs_fs_ops (void)
{
  INSTALL_BINOP (op_add, octave_float_complex, octave_float_scalar, add);
  INSTALL_BINOP (op_sub, octave_float_complex, octave_float_scalar, sub);
  INSTALL_BINOP (op_mul, octave_float_complex, octave_float_scalar, mul);
  INSTALL_BINOP (op_div, octave_float_complex, octave_float_scalar, div);
  INSTALL_BINOP (op_pow, octave_float_complex, octave_float_scalar, pow);
  INSTALL_BINOP (op_ldiv, octave_float_complex, octave_float_scalar, ldiv);
  INSTALL_BINOP (op_lt, octave_float_complex, octave_float_scalar, lt);
  INSTALL_BINOP (op_le, octave_float_complex, octave_float_scalar, le);
  INSTALL_BINOP (op_eq, octave_float_complex, octave_float_scalar, eq);
  INSTALL_BINOP (op_ge, octave_float_complex, octave_float_scalar, ge);
  INSTALL_BINOP (op_gt, octave_float_complex, octave_float_scalar, gt);
  INSTALL_BINOP (op_ne, octave_float_complex, octave_float_scalar, ne);
  INSTALL_BINOP (op_el_mul, octave_float_complex, octave_float_scalar, el_mul);
  INSTALL_BINOP (op_el_div, octave_float_complex, octave_float_scalar, el_div);
  INSTALL_BINOP (op_el_pow, octave_float_complex, octave_float_scalar, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_float_complex, octave_float_scalar,
                 el_ldiv);
  INSTALL_BINOP (op_el_and, octave_float_complex, octave_float_scalar, el_and);
  INSTALL_BINOP (op_el_or, octave_float_complex, octave_float_scalar, el_or);

  INSTALL_CATOP (octave_float_complex, octave_float_scalar, fcs_fs);
  INSTALL_CATOP (octave_complex, octave_float_scalar, cs_fs);
  INSTALL_CATOP (octave_float_complex, octave_scalar, fcs_s);

  INSTALL_ASSIGNCONV (octave_float_complex, octave_float_scalar,
                      octave_float_complex_matrix);
  INSTALL_ASSIGNCONV (octave_complex, octave_float_scalar,
                      octave_complex_matrix);
}
