/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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
#include "ov-typeinfo.h"
#include "ov-re-mat.h"
#include "ov-null-mat.h"
#include "ops.h"

#include "sparse-xpow.h"
#include "sparse-xdiv.h"
#include "ov-re-sparse.h"

// sparse matrix unary ops.

DEFUNOP_OP (not, sparse_matrix, !)
DEFUNOP_OP (uplus, sparse_matrix, /* no-op */)
DEFUNOP_OP (uminus, sparse_matrix, -)

DEFUNOP (transpose, sparse_matrix)
{
  CAST_UNOP_ARG (const octave_sparse_matrix&);
  return octave_value (v.sparse_matrix_value ().transpose (),
                       v.matrix_type ().transpose ());
}

// sparse matrix by sparse matrix ops.

DEFBINOP_OP (add, sparse_matrix, sparse_matrix, +)

// DEFBINOP_OP (sub, sparse_matrix, sparse_matrix, -)

static octave_value
oct_binop_sub (const octave_base_value& a1, const octave_base_value& a2)
{
  const octave_sparse_matrix& v1 =
    dynamic_cast<const octave_sparse_matrix&> (a1);
  const octave_sparse_matrix& v2 =
    dynamic_cast<const octave_sparse_matrix&> (a2);
  SparseMatrix m = v1.sparse_matrix_value () - v2.sparse_matrix_value ();

  return octave_value (m);
}

DEFBINOP_OP (mul, sparse_matrix, sparse_matrix, *)

DEFBINOP (div, sparse_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&, const octave_sparse_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    {
      double d = v2.scalar_value ();

      if (d == 0.0)
        gripe_divide_by_zero ();

      return octave_value (v1.sparse_matrix_value () / d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();
      SparseMatrix ret = xdiv (v1.sparse_matrix_value (),
                               v2.sparse_matrix_value (), typ);

      v2.matrix_type (typ);
      return ret;
    }
}

DEFBINOPX (pow, sparse_matrix, sparse_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
  return octave_value ();
}

DEFBINOP (ldiv, sparse_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&, const octave_sparse_matrix&);

  if (v1.rows () == 1 && v1.columns () == 1)
    {
      double d = v1.double_value ();

      if (d == 0.0)
        gripe_divide_by_zero ();

      return octave_value (v2.sparse_matrix_value () / d);
    }
  else
    {
      MatrixType typ = v1.matrix_type ();

      SparseMatrix ret = xleftdiv (v1.sparse_matrix_value (),
                                   v2.sparse_matrix_value (), typ);

      v1.matrix_type (typ);
      return ret;
    }
}

DEFBINOP_FN (lt, sparse_matrix, sparse_matrix, mx_el_lt)
DEFBINOP_FN (le, sparse_matrix, sparse_matrix, mx_el_le)
DEFBINOP_FN (eq, sparse_matrix, sparse_matrix, mx_el_eq)
DEFBINOP_FN (ge, sparse_matrix, sparse_matrix, mx_el_ge)
DEFBINOP_FN (gt, sparse_matrix, sparse_matrix, mx_el_gt)
DEFBINOP_FN (ne, sparse_matrix, sparse_matrix, mx_el_ne)

DEFBINOP_FN (el_mul, sparse_matrix, sparse_matrix, product)
DEFBINOP_FN (el_div, sparse_matrix, sparse_matrix, quotient)

DEFBINOP_FN (el_pow, sparse_matrix, sparse_matrix, elem_xpow)

DEFBINOP (el_ldiv, sparse_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&, const octave_sparse_matrix&);
  return octave_value
         (quotient (v2.sparse_matrix_value (), v1.sparse_matrix_value ()));
}

DEFBINOP_FN (el_and, sparse_matrix, sparse_matrix, mx_el_and)
DEFBINOP_FN (el_or,  sparse_matrix, sparse_matrix, mx_el_or)

DEFCATOP_FN (sm_sm, sparse_matrix, sparse_matrix, concat)

DEFASSIGNOP_FN (assign, sparse_matrix, sparse_matrix, assign)

DEFNULLASSIGNOP_FN (null_assign, sparse_matrix, delete_elements)

void
install_sm_sm_ops (void)
{
  INSTALL_UNOP (op_not, octave_sparse_matrix, not);
  INSTALL_UNOP (op_uplus, octave_sparse_matrix, uplus);
  INSTALL_UNOP (op_uminus, octave_sparse_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_sparse_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_sparse_matrix, transpose);

  INSTALL_BINOP (op_add, octave_sparse_matrix, octave_sparse_matrix, add);
  INSTALL_BINOP (op_sub, octave_sparse_matrix, octave_sparse_matrix, sub);
  INSTALL_BINOP (op_mul, octave_sparse_matrix, octave_sparse_matrix, mul);
  INSTALL_BINOP (op_div, octave_sparse_matrix, octave_sparse_matrix, div);
  INSTALL_BINOP (op_pow, octave_sparse_matrix, octave_sparse_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_sparse_matrix, octave_sparse_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_sparse_matrix, octave_sparse_matrix, lt);
  INSTALL_BINOP (op_le, octave_sparse_matrix, octave_sparse_matrix, le);
  INSTALL_BINOP (op_eq, octave_sparse_matrix, octave_sparse_matrix, eq);
  INSTALL_BINOP (op_ge, octave_sparse_matrix, octave_sparse_matrix, ge);
  INSTALL_BINOP (op_gt, octave_sparse_matrix, octave_sparse_matrix, gt);
  INSTALL_BINOP (op_ne, octave_sparse_matrix, octave_sparse_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_sparse_matrix, octave_sparse_matrix,
                 el_mul);
  INSTALL_BINOP (op_el_div, octave_sparse_matrix, octave_sparse_matrix,
                 el_div);
  INSTALL_BINOP (op_el_pow, octave_sparse_matrix, octave_sparse_matrix,
                 el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_sparse_matrix, octave_sparse_matrix,
                 el_ldiv);
  INSTALL_BINOP (op_el_and, octave_sparse_matrix, octave_sparse_matrix,
                 el_and);
  INSTALL_BINOP (op_el_or, octave_sparse_matrix, octave_sparse_matrix,
                 el_or);

  INSTALL_CATOP (octave_sparse_matrix, octave_sparse_matrix, sm_sm);

  INSTALL_ASSIGNOP (op_asn_eq, octave_sparse_matrix, octave_sparse_matrix,
                    assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_sparse_matrix, octave_null_matrix,
                    null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_sparse_matrix, octave_null_str,
                    null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_sparse_matrix, octave_null_sq_str,
                    null_assign);
}
