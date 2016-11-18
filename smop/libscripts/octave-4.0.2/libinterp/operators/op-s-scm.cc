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
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "ops.h"
#include "xpow.h"

#include "sparse-xpow.h"
#include "sparse-xdiv.h"
#include "smx-s-scm.h"
#include "smx-scm-s.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

// scalar by sparse complex matrix ops.

DEFBINOP_OP (add, scalar, sparse_complex_matrix, +)
DEFBINOP_OP (sub, scalar, sparse_complex_matrix, -)
DEFBINOP_OP (mul, scalar, sparse_complex_matrix, *)

DEFBINOP (div, scalar, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_sparse_complex_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    {
      Complex d = v2.complex_value ();

      if (d == 0.0)
        gripe_divide_by_zero ();

      return octave_value (SparseComplexMatrix (1, 1, v1.scalar_value () / d));
    }
  else
    {
      MatrixType typ = v2.matrix_type ();
      Matrix m1 = Matrix (1, 1, v1.scalar_value ());
      SparseComplexMatrix m2 = v2.sparse_complex_matrix_value ();
      ComplexMatrix ret = xdiv (m1, m2, typ);
      v2.matrix_type (typ);
      return ret;
    }
}

DEFBINOP (pow, scalar, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_scalar&,
                   const octave_sparse_complex_matrix&);
  return xpow (v1.scalar_value (), v2.complex_matrix_value ());
}

DEFBINOP (ldiv, scalar, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_scalar&,
                   const octave_sparse_complex_matrix&);

  double d = v1.double_value ();
  octave_value retval;

  if (d == 0.0)
    gripe_divide_by_zero ();

  retval = octave_value (v2.sparse_complex_matrix_value () / d);

  return retval;
}

DEFBINOP_FN (lt, scalar, sparse_complex_matrix, mx_el_lt)
DEFBINOP_FN (le, scalar, sparse_complex_matrix, mx_el_le)
DEFBINOP_FN (eq, scalar, sparse_complex_matrix, mx_el_eq)
DEFBINOP_FN (ge, scalar, sparse_complex_matrix, mx_el_ge)
DEFBINOP_FN (gt, scalar, sparse_complex_matrix, mx_el_gt)
DEFBINOP_FN (ne, scalar, sparse_complex_matrix, mx_el_ne)

DEFBINOP_OP (el_mul, scalar, sparse_complex_matrix, *)
DEFBINOP_FN (el_div, scalar, sparse_complex_matrix, x_el_div)
DEFBINOP_FN (el_pow, scalar, sparse_complex_matrix, elem_xpow)

DEFBINOP (el_ldiv, scalar, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_scalar&,
                   const octave_sparse_complex_matrix&);

  double d = v1.double_value ();
  octave_value retval;

  if (d == 0.0)
    gripe_divide_by_zero ();

  retval = octave_value (v2.sparse_complex_matrix_value () / d);

  return retval;
}

DEFBINOP_FN (el_and, scalar, sparse_complex_matrix, mx_el_and)
DEFBINOP_FN (el_or, scalar, sparse_complex_matrix, mx_el_or)

DEFCATOP (s_scm, scalar, sparse_compelx_matrix)
{
  CAST_BINOP_ARGS (octave_scalar&, const octave_sparse_complex_matrix&);
  SparseMatrix tmp (1, 1, v1.scalar_value ());
  return octave_value
         (tmp.concat (v2.sparse_complex_matrix_value (), ra_idx));
}

DEFCONV (sparse_complex_matrix_conv, scalar, sparse_complex_matrix)
{
  CAST_CONV_ARG (const octave_scalar&);

  return new octave_sparse_complex_matrix
         (SparseComplexMatrix (v.complex_matrix_value ()));
}

void
install_s_scm_ops (void)
{
  INSTALL_BINOP (op_add, octave_scalar, octave_sparse_complex_matrix, add);
  INSTALL_BINOP (op_sub, octave_scalar, octave_sparse_complex_matrix, sub);
  INSTALL_BINOP (op_mul, octave_scalar, octave_sparse_complex_matrix, mul);
  INSTALL_BINOP (op_div, octave_scalar, octave_sparse_complex_matrix, div);
  INSTALL_BINOP (op_pow, octave_scalar, octave_sparse_complex_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_scalar, octave_sparse_complex_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_scalar, octave_sparse_complex_matrix, lt);
  INSTALL_BINOP (op_le, octave_scalar, octave_sparse_complex_matrix, le);
  INSTALL_BINOP (op_eq, octave_scalar, octave_sparse_complex_matrix, eq);
  INSTALL_BINOP (op_ge, octave_scalar, octave_sparse_complex_matrix, ge);
  INSTALL_BINOP (op_gt, octave_scalar, octave_sparse_complex_matrix, gt);
  INSTALL_BINOP (op_ne, octave_scalar, octave_sparse_complex_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_scalar, octave_sparse_complex_matrix,
                 el_mul);
  INSTALL_BINOP (op_el_div, octave_scalar, octave_sparse_complex_matrix,
                 el_div);
  INSTALL_BINOP (op_el_pow, octave_scalar, octave_sparse_complex_matrix,
                 el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_scalar, octave_sparse_complex_matrix,
                 el_ldiv);
  INSTALL_BINOP (op_el_and, octave_scalar, octave_sparse_complex_matrix,
                 el_and);
  INSTALL_BINOP (op_el_or, octave_scalar, octave_sparse_complex_matrix,
                 el_or);

  INSTALL_CATOP (octave_scalar, octave_sparse_complex_matrix, s_scm);

  INSTALL_ASSIGNCONV (octave_scalar, octave_sparse_complex_matrix,
                      octave_complex_matrix);

  INSTALL_WIDENOP (octave_scalar, octave_sparse_complex_matrix,
                   sparse_complex_matrix_conv);
}
