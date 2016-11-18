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
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// matrix unary ops.

DEFNDUNOP_OP (not, matrix, array, !)
DEFNDUNOP_OP (uplus, matrix, array, /* no-op */)
DEFNDUNOP_OP (uminus, matrix, array, -)

DEFUNOP (transpose, matrix)
{
  CAST_UNOP_ARG (const octave_matrix&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.matrix_value ().transpose ());
}

DEFNCUNOP_METHOD (incr, matrix, increment)
DEFNCUNOP_METHOD (decr, matrix, decrement)
DEFNCUNOP_METHOD (changesign, matrix, changesign)

// matrix by matrix ops.

DEFNDBINOP_OP (add, matrix, matrix, array, array, +)
DEFNDBINOP_OP (sub, matrix, matrix, array, array, -)

DEFBINOP_OP (mul, matrix, matrix, *)

DEFBINOP (div, matrix, matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_matrix&);
  MatrixType typ = v2.matrix_type ();

  Matrix ret = xdiv (v1.matrix_value (), v2.matrix_value (), typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOPX (pow, matrix, matrix)
{
  error ("can't do A ^ B for A and B both matrices");
  return octave_value ();
}

DEFBINOP (ldiv, matrix, matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_matrix&);
  MatrixType typ = v1.matrix_type ();

  Matrix ret = xleftdiv (v1.matrix_value (), v2.matrix_value (), typ);

  v1.matrix_type (typ);
  return ret;
}

DEFBINOP (trans_mul, matrix, matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_matrix&);
  return octave_value(xgemm (v1.matrix_value (), v2.matrix_value (),
                             blas_trans, blas_no_trans));
}

DEFBINOP (mul_trans, matrix, matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_matrix&);
  return octave_value(xgemm (v1.matrix_value (), v2.matrix_value (),
                             blas_no_trans, blas_trans));
}

DEFBINOP (trans_ldiv, matrix, matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_matrix&);
  MatrixType typ = v1.matrix_type ();

  Matrix ret = xleftdiv (v1.matrix_value (), v2.matrix_value (),
                         typ, blas_trans);

  v1.matrix_type (typ);
  return ret;
}

DEFNDBINOP_FN (lt, matrix, matrix, array, array, mx_el_lt)
DEFNDBINOP_FN (le, matrix, matrix, array, array, mx_el_le)
DEFNDBINOP_FN (eq, matrix, matrix, array, array, mx_el_eq)
DEFNDBINOP_FN (ge, matrix, matrix, array, array, mx_el_ge)
DEFNDBINOP_FN (gt, matrix, matrix, array, array, mx_el_gt)
DEFNDBINOP_FN (ne, matrix, matrix, array, array, mx_el_ne)

DEFNDBINOP_FN (el_mul, matrix, matrix, array, array, product)
DEFNDBINOP_FN (el_div, matrix, matrix, array, array, quotient)
DEFNDBINOP_FN (el_pow, matrix, matrix, array, array, elem_xpow)

DEFBINOP (el_ldiv, matrix, matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_matrix&);

  return octave_value (quotient (v2.array_value (), v1.array_value ()));
}

DEFNDBINOP_FN (el_and, matrix, matrix, array, array, mx_el_and)
DEFNDBINOP_FN (el_or,  matrix, matrix, array, array, mx_el_or)
DEFNDBINOP_FN (el_not_and, matrix, matrix, array, array, mx_el_not_and)
DEFNDBINOP_FN (el_not_or,  matrix, matrix, array, array, mx_el_not_or)
DEFNDBINOP_FN (el_and_not, matrix, matrix, array, array, mx_el_and_not)
DEFNDBINOP_FN (el_or_not,  matrix, matrix, array, array, mx_el_or_not)


DEFNDCATOP_FN (m_m, matrix, matrix, array, array, concat)

DEFNDASSIGNOP_FN (assign, matrix, matrix, array, assign)
DEFNDASSIGNOP_FN (sgl_assign, float_matrix, matrix, float_array, assign)

DEFNULLASSIGNOP_FN (null_assign, matrix, delete_elements)

DEFNDASSIGNOP_OP (assign_add, matrix, matrix, array, +=)
DEFNDASSIGNOP_OP (assign_sub, matrix, matrix, array, -=)
DEFNDASSIGNOP_FNOP (assign_el_mul, matrix, matrix, array, product_eq)
DEFNDASSIGNOP_FNOP (assign_el_div, matrix, matrix, array, quotient_eq)

CONVDECL (matrix_to_float_matrix)
{
  CAST_CONV_ARG (const octave_matrix&);

  return new octave_float_matrix (FloatNDArray (v.array_value ()));
}

void
install_m_m_ops (void)
{
  INSTALL_UNOP (op_not, octave_matrix, not);
  INSTALL_UNOP (op_uplus, octave_matrix, uplus);
  INSTALL_UNOP (op_uminus, octave_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_matrix, transpose);

  INSTALL_NCUNOP (op_incr, octave_matrix, incr);
  INSTALL_NCUNOP (op_decr, octave_matrix, decr);
  INSTALL_NCUNOP (op_uminus, octave_matrix, changesign);

  INSTALL_BINOP (op_add, octave_matrix, octave_matrix, add);
  INSTALL_BINOP (op_sub, octave_matrix, octave_matrix, sub);
  INSTALL_BINOP (op_mul, octave_matrix, octave_matrix, mul);
  INSTALL_BINOP (op_div, octave_matrix, octave_matrix, div);
  INSTALL_BINOP (op_pow, octave_matrix, octave_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_matrix, octave_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_matrix, octave_matrix, lt);
  INSTALL_BINOP (op_le, octave_matrix, octave_matrix, le);
  INSTALL_BINOP (op_eq, octave_matrix, octave_matrix, eq);
  INSTALL_BINOP (op_ge, octave_matrix, octave_matrix, ge);
  INSTALL_BINOP (op_gt, octave_matrix, octave_matrix, gt);
  INSTALL_BINOP (op_ne, octave_matrix, octave_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_matrix, octave_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_matrix, octave_matrix, el_div);
  INSTALL_BINOP (op_el_pow, octave_matrix, octave_matrix, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_matrix, octave_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_matrix, octave_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_matrix, octave_matrix, el_or);
  INSTALL_BINOP (op_el_and_not, octave_matrix, octave_matrix, el_and_not);
  INSTALL_BINOP (op_el_or_not, octave_matrix, octave_matrix, el_or_not);
  INSTALL_BINOP (op_el_not_and, octave_matrix, octave_matrix, el_not_and);
  INSTALL_BINOP (op_el_not_or, octave_matrix, octave_matrix, el_not_or);
  INSTALL_BINOP (op_trans_mul, octave_matrix, octave_matrix, trans_mul);
  INSTALL_BINOP (op_mul_trans, octave_matrix, octave_matrix, mul_trans);
  INSTALL_BINOP (op_herm_mul, octave_matrix, octave_matrix, trans_mul);
  INSTALL_BINOP (op_mul_herm, octave_matrix, octave_matrix, mul_trans);
  INSTALL_BINOP (op_trans_ldiv, octave_matrix, octave_matrix, trans_ldiv);
  INSTALL_BINOP (op_herm_ldiv, octave_matrix, octave_matrix, trans_ldiv);

  INSTALL_CATOP (octave_matrix, octave_matrix, m_m);

  INSTALL_ASSIGNOP (op_asn_eq, octave_matrix, octave_matrix, assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_float_matrix, octave_matrix, sgl_assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_matrix, octave_null_matrix, null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_matrix, octave_null_str, null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_matrix, octave_null_sq_str, null_assign);

  INSTALL_ASSIGNOP (op_add_eq, octave_matrix, octave_matrix, assign_add);
  INSTALL_ASSIGNOP (op_sub_eq, octave_matrix, octave_matrix, assign_sub);
  INSTALL_ASSIGNOP (op_el_mul_eq, octave_matrix, octave_matrix, assign_el_mul);
  INSTALL_ASSIGNOP (op_el_div_eq, octave_matrix, octave_matrix, assign_el_div);

  INSTALL_CONVOP (octave_matrix, octave_float_matrix, matrix_to_float_matrix);
}
