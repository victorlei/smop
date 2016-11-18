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

#include "mx-m-cm.h"
#include "mx-cm-m.h"
#include "mx-nda-cnda.h"
#include "mx-cnda-nda.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// matrix by complex matrix ops.

DEFNDBINOP_OP (add, matrix, complex_matrix, array, complex_array, +)
DEFNDBINOP_OP (sub, matrix, complex_matrix, array, complex_array, -)

DEFBINOP_OP (mul, matrix, complex_matrix, *)

DEFBINOP (trans_mul, matrix, complex_matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex_matrix&);

  Matrix m1 = v1.matrix_value ();
  ComplexMatrix m2 = v2.complex_matrix_value ();

  return ComplexMatrix (xgemm (m1, real (m2), blas_trans, blas_no_trans),
                        xgemm (m1, imag (m2), blas_trans, blas_no_trans));
}

DEFBINOP (div, matrix, complex_matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex_matrix&);
  MatrixType typ = v2.matrix_type ();

  ComplexMatrix ret = xdiv (v1.matrix_value (),
                            v2.complex_matrix_value (), typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOPX (pow, matrix, complex_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
  return octave_value ();
}

DEFBINOP (ldiv, matrix, complex_matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex_matrix&);
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (v1.matrix_value (),
                                v2.complex_matrix_value (), typ);

  v1.matrix_type (typ);
  return ret;
}

DEFBINOP (trans_ldiv, matrix, complex_matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex_matrix&);
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (v1.matrix_value (),
                                v2.complex_matrix_value (), typ, blas_trans);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, matrix, complex_matrix, array, complex_array, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, matrix, complex_matrix, array, complex_array, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, matrix, complex_matrix, array, complex_array, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, matrix, complex_matrix, array, complex_array, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, matrix, complex_matrix, array, complex_array, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, matrix, complex_matrix, array, complex_array, mx_el_ne)

DEFNDBINOP_FN (el_mul, matrix, complex_matrix, array, complex_array, product)
DEFNDBINOP_FN (el_div, matrix, complex_matrix, array, complex_array, quotient)
DEFNDBINOP_FN (el_pow, matrix, complex_matrix, array, complex_array, elem_xpow)

DEFBINOP (el_ldiv, matrix, complex_matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex_matrix&);

  return quotient (v2.complex_array_value (), v1.array_value ());
}

DEFNDBINOP_FN (el_and, matrix, complex_matrix, array, complex_array, mx_el_and)
DEFNDBINOP_FN (el_or,  matrix, complex_matrix, array, complex_array, mx_el_or)

DEFNDCATOP_FN (m_cm, matrix, complex_matrix, array, complex_array, concat)

DEFCONV (complex_matrix_conv, matrix, complex_matrix)
{
  CAST_CONV_ARG (const octave_matrix&);

  return new octave_complex_matrix (ComplexNDArray (v.array_value ()));
}

void
install_m_cm_ops (void)
{
  INSTALL_BINOP (op_add, octave_matrix, octave_complex_matrix, add);
  INSTALL_BINOP (op_sub, octave_matrix, octave_complex_matrix, sub);
  INSTALL_BINOP (op_mul, octave_matrix, octave_complex_matrix, mul);
  INSTALL_BINOP (op_div, octave_matrix, octave_complex_matrix, div);
  INSTALL_BINOP (op_pow, octave_matrix, octave_complex_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_matrix, octave_complex_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_matrix, octave_complex_matrix, lt);
  INSTALL_BINOP (op_le, octave_matrix, octave_complex_matrix, le);
  INSTALL_BINOP (op_eq, octave_matrix, octave_complex_matrix, eq);
  INSTALL_BINOP (op_ge, octave_matrix, octave_complex_matrix, ge);
  INSTALL_BINOP (op_gt, octave_matrix, octave_complex_matrix, gt);
  INSTALL_BINOP (op_ne, octave_matrix, octave_complex_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_matrix, octave_complex_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_matrix, octave_complex_matrix, el_div);
  INSTALL_BINOP (op_el_pow, octave_matrix, octave_complex_matrix, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_matrix, octave_complex_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_matrix, octave_complex_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_matrix, octave_complex_matrix, el_or);
  INSTALL_BINOP (op_trans_mul, octave_matrix, octave_complex_matrix, trans_mul);
  INSTALL_BINOP (op_herm_mul, octave_matrix, octave_complex_matrix, trans_mul);
  INSTALL_BINOP (op_trans_ldiv, octave_matrix, octave_complex_matrix,
                 trans_ldiv);
  INSTALL_BINOP (op_herm_ldiv, octave_matrix, octave_complex_matrix,
                 trans_ldiv);

  INSTALL_CATOP (octave_matrix, octave_complex_matrix, m_cm);

  INSTALL_ASSIGNCONV (octave_matrix, octave_complex_matrix,
                      octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_float_matrix, octave_complex_matrix,
                      octave_float_complex_matrix);

  INSTALL_WIDENOP (octave_matrix, octave_complex_matrix, complex_matrix_conv);
}
