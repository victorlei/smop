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
#include "ov-flt-complex.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// scalar by complex scalar ops.

DEFBINOP_OP (add, float_scalar, float_complex, +)
DEFBINOP_OP (sub, float_scalar, float_complex, -)
DEFBINOP_OP (mul, float_scalar, float_complex, *)

DEFBINOP (div, float_scalar, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_scalar&, const octave_float_complex&);

  FloatComplex d = v2.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v1.float_value () / d);
}

DEFBINOP_FN (pow, float_scalar, float_complex, xpow)

DEFBINOP (ldiv, float_scalar, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_scalar&, const octave_float_complex&);

  float d = v1.float_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.float_complex_value () / d);
}

DEFCMPLXCMPOP_OP (lt, float_scalar, float_complex, <)
DEFCMPLXCMPOP_OP (le, float_scalar, float_complex, <=)
DEFCMPLXCMPOP_OP (eq, float_scalar, float_complex, ==)
DEFCMPLXCMPOP_OP (ge, float_scalar, float_complex, >=)
DEFCMPLXCMPOP_OP (gt, float_scalar, float_complex, >)
DEFCMPLXCMPOP_OP (ne, float_scalar, float_complex, !=)

DEFBINOP_OP (el_mul, float_scalar, float_complex, *)

DEFBINOP (el_div, float_scalar, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_scalar&, const octave_float_complex&);

  FloatComplex d = v2.float_complex_value ();

  if (d == 0.0f)
    gripe_divide_by_zero ();

  return octave_value (v1.float_value () / d);
}

DEFBINOP_FN (el_pow, float_scalar, float_complex, xpow)

DEFBINOP (el_ldiv, float_scalar, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_scalar&, const octave_float_complex&);

  float d = v1.float_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.float_complex_value () / d);
}

DEFBINOP (el_and, float_scalar, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_scalar&, const octave_float_complex&);

  return octave_value (v1.float_scalar_value ()
                       && (v2.float_complex_value () != 0.0f));
}

DEFBINOP (el_or, float_scalar, float_complex)
{
  CAST_BINOP_ARGS (const octave_float_scalar&, const octave_float_complex&);

  return octave_value (v1.float_scalar_value ()
                       || (v2.float_complex_value () != 0.0f));
}

DEFNDCATOP_FN (fs_fcs, float_scalar, float_complex, float_array,
               float_complex_array, concat)

DEFNDCATOP_FN (s_fcs, scalar, float_complex, float_array,
               float_complex_array, concat)

DEFNDCATOP_FN (fs_cs, float_scalar, complex, float_array,
               float_complex_array, concat)

void
install_fs_fcs_ops (void)
{
  INSTALL_BINOP (op_add, octave_float_scalar, octave_float_complex, add);
  INSTALL_BINOP (op_sub, octave_float_scalar, octave_float_complex, sub);
  INSTALL_BINOP (op_mul, octave_float_scalar, octave_float_complex, mul);
  INSTALL_BINOP (op_div, octave_float_scalar, octave_float_complex, div);
  INSTALL_BINOP (op_pow, octave_float_scalar, octave_float_complex, pow);
  INSTALL_BINOP (op_ldiv, octave_float_scalar, octave_float_complex, ldiv);
  INSTALL_BINOP (op_lt, octave_float_scalar, octave_float_complex, lt);
  INSTALL_BINOP (op_le, octave_float_scalar, octave_float_complex, le);
  INSTALL_BINOP (op_eq, octave_float_scalar, octave_float_complex, eq);
  INSTALL_BINOP (op_ge, octave_float_scalar, octave_float_complex, ge);
  INSTALL_BINOP (op_gt, octave_float_scalar, octave_float_complex, gt);
  INSTALL_BINOP (op_ne, octave_float_scalar, octave_float_complex, ne);
  INSTALL_BINOP (op_el_mul, octave_float_scalar, octave_float_complex, el_mul);
  INSTALL_BINOP (op_el_div, octave_float_scalar, octave_float_complex, el_div);
  INSTALL_BINOP (op_el_pow, octave_float_scalar, octave_float_complex, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_float_scalar, octave_float_complex,
                 el_ldiv);
  INSTALL_BINOP (op_el_and, octave_float_scalar, octave_float_complex, el_and);
  INSTALL_BINOP (op_el_or, octave_float_scalar, octave_float_complex, el_or);

  INSTALL_CATOP (octave_float_scalar, octave_float_complex, fs_fcs);
  INSTALL_CATOP (octave_scalar, octave_float_complex, s_fcs);
  INSTALL_CATOP (octave_float_scalar, octave_complex, fs_cs);

  INSTALL_ASSIGNCONV (octave_float_scalar, octave_float_complex,
                      octave_float_complex_matrix);
  INSTALL_ASSIGNCONV (octave_scalar, octave_float_complex,
                      octave_complex_matrix);
}
