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

#include "mx-m-cs.h"
#include "mx-cs-m.h"
#include "mx-nda-cs.h"
#include "mx-cs-nda.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-complex.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// matrix by complex scalar ops.

DEFNDBINOP_OP (add, matrix, complex, array, complex, +)
DEFNDBINOP_OP (sub, matrix, complex, array, complex, -)
DEFNDBINOP_OP (mul, matrix, complex, array, complex, *)

DEFBINOP (div, matrix, complex)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.array_value () / d);
}

DEFBINOP_FN (pow, matrix, complex, xpow)

DEFBINOP (ldiv, matrix, complex)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex&);

  Matrix m1 = v1.matrix_value ();
  ComplexMatrix m2 = v2.complex_matrix_value ();
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (m1, m2, typ);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, matrix, complex, array, complex, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, matrix, complex, array, complex, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, matrix, complex, array, complex, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, matrix, complex, array, complex, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, matrix, complex, array, complex, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, matrix, complex, array, complex, mx_el_ne)

DEFNDBINOP_OP (el_mul, matrix, complex, array, complex, *)

DEFBINOP (el_div, matrix, complex)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.array_value () / d);
}

DEFNDBINOP_FN (el_pow, matrix, complex, array, complex, elem_xpow)

DEFBINOP (el_ldiv, matrix, complex)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex&);

  return x_el_div (v2.complex_value (), v1.array_value ());
}

DEFNDBINOP_FN (el_and, matrix, complex, array, complex, mx_el_and)
DEFNDBINOP_FN (el_or, matrix, complex, array, complex, mx_el_or)

DEFNDCATOP_FN (m_cs, matrix, complex, array, complex_array, concat)

void
install_m_cs_ops (void)
{
  INSTALL_BINOP (op_add, octave_matrix, octave_complex, add);
  INSTALL_BINOP (op_sub, octave_matrix, octave_complex, sub);
  INSTALL_BINOP (op_mul, octave_matrix, octave_complex, mul);
  INSTALL_BINOP (op_div, octave_matrix, octave_complex, div);
  INSTALL_BINOP (op_pow, octave_matrix, octave_complex, pow);
  INSTALL_BINOP (op_ldiv, octave_matrix, octave_complex, ldiv);
  INSTALL_BINOP (op_lt, octave_matrix, octave_complex, lt);
  INSTALL_BINOP (op_le, octave_matrix, octave_complex, le);
  INSTALL_BINOP (op_eq, octave_matrix, octave_complex, eq);
  INSTALL_BINOP (op_ge, octave_matrix, octave_complex, ge);
  INSTALL_BINOP (op_gt, octave_matrix, octave_complex, gt);
  INSTALL_BINOP (op_ne, octave_matrix, octave_complex, ne);
  INSTALL_BINOP (op_el_mul, octave_matrix, octave_complex, el_mul);
  INSTALL_BINOP (op_el_div, octave_matrix, octave_complex, el_div);
  INSTALL_BINOP (op_el_pow, octave_matrix, octave_complex, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_matrix, octave_complex, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_matrix, octave_complex, el_and);
  INSTALL_BINOP (op_el_or, octave_matrix, octave_complex, el_or);

  INSTALL_CATOP (octave_matrix, octave_complex, m_cs);

  INSTALL_ASSIGNCONV (octave_matrix, octave_complex, octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_float_matrix, octave_complex,
                      octave_float_complex_matrix);
}
