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

#include "mx-cs-nda.h"
#include "mx-nda-cs.h"
#include "mx-cs-nda.h"
#include "mx-nda-cs.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// complex scalar by matrix ops.

DEFNDBINOP_OP (add, complex, matrix, complex, array, +)
DEFNDBINOP_OP (sub, complex, matrix, complex, array, -)
DEFNDBINOP_OP (mul, complex, matrix, complex, array, *)

DEFBINOP (div, complex, matrix)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_matrix&);

  ComplexMatrix m1 = v1.complex_matrix_value ();
  Matrix m2 = v2.matrix_value ();
  MatrixType typ = v2.matrix_type ();

  ComplexMatrix ret = xdiv (m1, m2, typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOP_FN (pow, complex, matrix, xpow)

DEFBINOP (ldiv, complex, matrix)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_matrix&);

  Complex d = v1.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.array_value () / d);
}

DEFNDCMPLXCMPOP_FN (lt, complex, matrix, complex, array, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, complex, matrix, complex, array, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, complex, matrix, complex, array, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, complex, matrix, complex, array, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, complex, matrix, complex, array, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, complex, matrix, complex, array, mx_el_ne)

DEFNDBINOP_OP (el_mul, complex, matrix, complex, array, *)
DEFNDBINOP_FN (el_div, complex, matrix, complex, array, x_el_div)
DEFNDBINOP_FN (el_pow, complex, matrix, complex, array, elem_xpow)

DEFBINOP (el_ldiv, complex, matrix)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_matrix&);

  Complex d = v1.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.array_value () / d);
}

DEFNDBINOP_FN (el_and, complex, matrix, complex, array, mx_el_and)
DEFNDBINOP_FN (el_or,  complex, matrix, complex, array, mx_el_or)

DEFNDCATOP_FN (cs_m, complex, matrix, complex_array, array, concat)

void
install_cs_m_ops (void)
{
  INSTALL_BINOP (op_add, octave_complex, octave_matrix, add);
  INSTALL_BINOP (op_sub, octave_complex, octave_matrix, sub);
  INSTALL_BINOP (op_mul, octave_complex, octave_matrix, mul);
  INSTALL_BINOP (op_div, octave_complex, octave_matrix, div);
  INSTALL_BINOP (op_pow, octave_complex, octave_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_complex, octave_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_complex, octave_matrix, lt);
  INSTALL_BINOP (op_le, octave_complex, octave_matrix, le);
  INSTALL_BINOP (op_eq, octave_complex, octave_matrix, eq);
  INSTALL_BINOP (op_ge, octave_complex, octave_matrix, ge);
  INSTALL_BINOP (op_gt, octave_complex, octave_matrix, gt);
  INSTALL_BINOP (op_ne, octave_complex, octave_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_complex, octave_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_complex, octave_matrix, el_div);
  INSTALL_BINOP (op_el_pow, octave_complex, octave_matrix, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_complex, octave_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_complex, octave_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_complex, octave_matrix, el_or);

  INSTALL_CATOP (octave_complex, octave_matrix, cs_m);

  INSTALL_ASSIGNCONV (octave_complex, octave_matrix, octave_complex_matrix);
}
