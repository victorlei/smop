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
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// unary complex matrix ops.

DEFNDUNOP_OP (not, float_complex_matrix, float_complex_array, !)
DEFNDUNOP_OP (uplus, float_complex_matrix, float_complex_array, /* no-op */)
DEFNDUNOP_OP (uminus, float_complex_matrix, float_complex_array, -)

DEFUNOP (transpose, float_complex_matrix)
{
  CAST_UNOP_ARG (const octave_float_complex_matrix&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.float_complex_matrix_value ().transpose ());
}

DEFUNOP (hermitian, float_complex_matrix)
{
  CAST_UNOP_ARG (const octave_float_complex_matrix&);

  if (v.ndims () > 2)
    {
      error ("complex-conjugate transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.float_complex_matrix_value ().hermitian ());
}

DEFNCUNOP_METHOD (incr, float_complex_matrix, increment)
DEFNCUNOP_METHOD (decr, float_complex_matrix, decrement)
DEFNCUNOP_METHOD (changesign, float_complex_matrix, changesign)

// complex matrix by complex matrix ops.

DEFNDBINOP_OP (add, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, +)
DEFNDBINOP_OP (sub, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, -)

DEFBINOP_OP (mul, float_complex_matrix, float_complex_matrix, *)

DEFBINOP (div, float_complex_matrix, float_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex_matrix&);
  MatrixType typ = v2.matrix_type ();

  FloatComplexMatrix ret = xdiv (v1.float_complex_matrix_value (),
                                 v2.float_complex_matrix_value (), typ);

  v2.matrix_type (typ);
  return ret;
}

DEFBINOPX (pow, float_complex_matrix, float_complex_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
  return octave_value ();
}

DEFBINOP (ldiv, float_complex_matrix, float_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex_matrix&);
  MatrixType typ = v1.matrix_type ();

  FloatComplexMatrix ret = xleftdiv (v1.float_complex_matrix_value (),
                                     v2.float_complex_matrix_value (), typ);

  v1.matrix_type (typ);
  return ret;
}

DEFBINOP (trans_mul, float_complex_matrix, float_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex_matrix&);
  return octave_value(xgemm (v1.float_complex_matrix_value (),
                             v2.float_complex_matrix_value (),
                             blas_trans, blas_no_trans));
}

DEFBINOP (mul_trans, float_complex_matrix, float_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex_matrix&);
  return octave_value(xgemm (v1.float_complex_matrix_value (),
                             v2.float_complex_matrix_value (),
                             blas_no_trans, blas_trans));
}

DEFBINOP (herm_mul, float_complex_matrix, float_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex_matrix&);
  return octave_value(xgemm (v1.float_complex_matrix_value (),
                             v2.float_complex_matrix_value (),
                             blas_conj_trans, blas_no_trans));
}

DEFBINOP (mul_herm, float_complex_matrix, float_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex_matrix&);
  return octave_value(xgemm (v1.float_complex_matrix_value (),
                             v2.float_complex_matrix_value (),
                             blas_no_trans, blas_conj_trans));
}

DEFBINOP (trans_ldiv, float_complex_matrix, float_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex_matrix&);
  MatrixType typ = v1.matrix_type ();

  FloatComplexMatrix ret = xleftdiv (v1.float_complex_matrix_value (),
                                     v2.float_complex_matrix_value (),
                                     typ, blas_trans);

  v1.matrix_type (typ);
  return ret;
}

DEFBINOP (herm_ldiv, float_complex_matrix, float_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex_matrix&);
  MatrixType typ = v1.matrix_type ();

  FloatComplexMatrix ret = xleftdiv (v1.float_complex_matrix_value (),
                                     v2.float_complex_matrix_value (),
                                     typ, blas_conj_trans);

  v1.matrix_type (typ);
  return ret;
}

DEFNDCMPLXCMPOP_FN (lt, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_lt)
DEFNDCMPLXCMPOP_FN (le, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_le)
DEFNDCMPLXCMPOP_FN (eq, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_eq)
DEFNDCMPLXCMPOP_FN (ge, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_ge)
DEFNDCMPLXCMPOP_FN (gt, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_gt)
DEFNDCMPLXCMPOP_FN (ne, float_complex_matrix, float_complex_matrix,
                    float_complex_array, float_complex_array, mx_el_ne)

DEFNDBINOP_FN (el_mul, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, product)
DEFNDBINOP_FN (el_div, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, quotient)
DEFNDBINOP_FN (el_pow, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, elem_xpow)

DEFBINOP (el_ldiv, float_complex_matrix, float_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_float_complex_matrix&,
                   const octave_float_complex_matrix&);

  return octave_value (quotient (v2.float_complex_array_value (),
                                 v1.float_complex_array_value ()));
}

DEFNDBINOP_FN (el_and, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, mx_el_and)
DEFNDBINOP_FN (el_or,  float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, mx_el_or)

DEFNDCATOP_FN (fcm_fcm, float_complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, concat)

DEFNDCATOP_FN (cm_fcm, complex_matrix, float_complex_matrix,
               float_complex_array, float_complex_array, concat)

DEFNDCATOP_FN (fcm_cm, float_complex_matrix, complex_matrix,
               float_complex_array, float_complex_array, concat)

DEFNDASSIGNOP_FN (assign, float_complex_matrix, float_complex_matrix,
                  float_complex_array, assign)
DEFNDASSIGNOP_FN (dbl_clx_assign, float_complex_matrix, complex_matrix,
                  float_complex_array, assign)
DEFNDASSIGNOP_FN (dbl_assign, float_complex_matrix, matrix,
                  float_complex_array, assign)

DEFNULLASSIGNOP_FN (null_assign, float_complex_matrix, delete_elements)

DEFNDASSIGNOP_OP (assign_add, float_complex_matrix,
                  float_complex_matrix, float_complex_array, +=)
DEFNDASSIGNOP_OP (assign_sub, float_complex_matrix,
                  float_complex_matrix, float_complex_array, -=)
DEFNDASSIGNOP_FNOP (assign_el_mul, float_complex_matrix, float_complex_matrix,
                    float_complex_array, product_eq)
DEFNDASSIGNOP_FNOP (assign_el_div, float_complex_matrix, float_complex_matrix,
                    float_complex_array, quotient_eq)

CONVDECL (float_complex_matrix_to_complex_matrix)
{
  CAST_CONV_ARG (const octave_float_complex_matrix&);

  return
    new octave_complex_matrix (ComplexNDArray (v.float_complex_array_value ()));
}

void
install_fcm_fcm_ops (void)
{
  INSTALL_UNOP (op_not, octave_float_complex_matrix, not);
  INSTALL_UNOP (op_uplus, octave_float_complex_matrix, uplus);
  INSTALL_UNOP (op_uminus, octave_float_complex_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_float_complex_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_float_complex_matrix, hermitian);

  INSTALL_NCUNOP (op_incr, octave_float_complex_matrix, incr);
  INSTALL_NCUNOP (op_decr, octave_float_complex_matrix, decr);
  INSTALL_NCUNOP (op_uminus, octave_float_complex_matrix, changesign);

  INSTALL_BINOP (op_add, octave_float_complex_matrix,
                 octave_float_complex_matrix, add);
  INSTALL_BINOP (op_sub, octave_float_complex_matrix,
                 octave_float_complex_matrix, sub);
  INSTALL_BINOP (op_mul, octave_float_complex_matrix,
                 octave_float_complex_matrix, mul);
  INSTALL_BINOP (op_div, octave_float_complex_matrix,
                 octave_float_complex_matrix, div);
  INSTALL_BINOP (op_pow, octave_float_complex_matrix,
                 octave_float_complex_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_float_complex_matrix,
                 octave_float_complex_matrix, ldiv);
  INSTALL_BINOP (op_trans_mul, octave_float_complex_matrix,
                 octave_float_complex_matrix, trans_mul);
  INSTALL_BINOP (op_mul_trans, octave_float_complex_matrix,
                 octave_float_complex_matrix, mul_trans);
  INSTALL_BINOP (op_herm_mul, octave_float_complex_matrix,
                 octave_float_complex_matrix, herm_mul);
  INSTALL_BINOP (op_mul_herm, octave_float_complex_matrix,
                 octave_float_complex_matrix, mul_herm);
  INSTALL_BINOP (op_trans_ldiv, octave_float_complex_matrix,
                 octave_float_complex_matrix, trans_ldiv);
  INSTALL_BINOP (op_herm_ldiv, octave_float_complex_matrix,
                 octave_float_complex_matrix, herm_ldiv);

  INSTALL_BINOP (op_lt, octave_float_complex_matrix,
                 octave_float_complex_matrix, lt);
  INSTALL_BINOP (op_le, octave_float_complex_matrix,
                 octave_float_complex_matrix, le);
  INSTALL_BINOP (op_eq, octave_float_complex_matrix,
                 octave_float_complex_matrix, eq);
  INSTALL_BINOP (op_ge, octave_float_complex_matrix,
                 octave_float_complex_matrix, ge);
  INSTALL_BINOP (op_gt, octave_float_complex_matrix,
                 octave_float_complex_matrix, gt);
  INSTALL_BINOP (op_ne, octave_float_complex_matrix,
                 octave_float_complex_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_float_complex_matrix,
                 octave_float_complex_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_float_complex_matrix,
                 octave_float_complex_matrix, el_div);
  INSTALL_BINOP (op_el_pow, octave_float_complex_matrix,
                 octave_float_complex_matrix, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_float_complex_matrix,
                 octave_float_complex_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_float_complex_matrix,
                 octave_float_complex_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_float_complex_matrix,
                 octave_float_complex_matrix, el_or);

  INSTALL_CATOP (octave_float_complex_matrix,
                 octave_float_complex_matrix, fcm_fcm);
  INSTALL_CATOP (octave_complex_matrix,
                 octave_float_complex_matrix, cm_fcm);
  INSTALL_CATOP (octave_float_complex_matrix,
                 octave_complex_matrix, fcm_cm);

  INSTALL_ASSIGNOP (op_asn_eq, octave_float_complex_matrix,
                    octave_float_complex_matrix, assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_float_complex_matrix,
                    octave_complex_matrix, dbl_clx_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_float_complex_matrix,
                    octave_matrix, dbl_assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_float_complex_matrix,
                    octave_null_matrix, null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_float_complex_matrix,
                    octave_null_str, null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_float_complex_matrix,
                    octave_null_sq_str, null_assign);

  INSTALL_ASSIGNOP (op_add_eq, octave_float_complex_matrix,
                    octave_float_complex_matrix, assign_add);
  INSTALL_ASSIGNOP (op_sub_eq, octave_float_complex_matrix,
                    octave_float_complex_matrix, assign_sub);
  INSTALL_ASSIGNOP (op_el_mul_eq, octave_float_complex_matrix,
                    octave_float_complex_matrix, assign_el_mul);
  INSTALL_ASSIGNOP (op_el_div_eq, octave_float_complex_matrix,
                    octave_float_complex_matrix, assign_el_div);

  INSTALL_CONVOP (octave_float_complex_matrix, octave_complex_matrix,
                  float_complex_matrix_to_complex_matrix);
}
