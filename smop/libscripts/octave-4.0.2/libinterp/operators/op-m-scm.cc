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
#include "ov-cx-mat.h"
#include "ops.h"
#include "xdiv.h"

#include "sparse-xpow.h"
#include "sparse-xdiv.h"
#include "smx-scm-m.h"
#include "smx-m-scm.h"
#include "ov-cx-sparse.h"

// matrix by sparse complex matrix ops.

DEFBINOP_OP (add, matrix, sparse_complex_matrix, +)
DEFBINOP_OP (sub, matrix, sparse_complex_matrix, -)

DEFBINOP_OP (mul, matrix, sparse_complex_matrix, *)

DEFBINOP (div, matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_sparse_complex_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    {
      Complex d = v2.complex_value ();

      if (d == 0.0)
        gripe_divide_by_zero ();

      return octave_value (v1.array_value () / d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();

      ComplexMatrix ret = xdiv (v1.matrix_value (),
                                v2.sparse_complex_matrix_value (), typ);

      v2.matrix_type (typ);
      return ret;
    }
}

DEFBINOPX (pow, matrix, sparse_complex_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
  return octave_value ();
}

DEFBINOP (ldiv, matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&,
                   const octave_sparse_complex_matrix&);
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (v1.matrix_value (),
                                v2.complex_matrix_value (), typ);

  v1.matrix_type (typ);
  return ret;
}

DEFBINOP_FN (lt, matrix, sparse_complex_matrix, mx_el_lt)
DEFBINOP_FN (le, matrix, sparse_complex_matrix, mx_el_le)
DEFBINOP_FN (eq, matrix, sparse_complex_matrix, mx_el_eq)
DEFBINOP_FN (ge, matrix, sparse_complex_matrix, mx_el_ge)
DEFBINOP_FN (gt, matrix, sparse_complex_matrix, mx_el_gt)
DEFBINOP_FN (ne, matrix, sparse_complex_matrix, mx_el_ne)

DEFBINOP_FN (el_mul, matrix, sparse_complex_matrix, product)
DEFBINOP_FN (el_div, matrix, sparse_complex_matrix, quotient)

DEFBINOP (el_pow, matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&,
                   const octave_sparse_complex_matrix&);

  return octave_value
         (elem_xpow (SparseMatrix (v1.matrix_value ()),
                     v2.sparse_complex_matrix_value ()));
}

DEFBINOP (el_ldiv, matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_matrix&,
                   const octave_sparse_complex_matrix&);
  return octave_value
         (quotient (v2.sparse_complex_matrix_value (), v1.matrix_value ()));
}

DEFBINOP_FN (el_and, matrix, sparse_complex_matrix, mx_el_and)
DEFBINOP_FN (el_or,  matrix, sparse_complex_matrix, mx_el_or)

DEFCATOP (m_scm, matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (octave_matrix&, const octave_sparse_complex_matrix&);
  SparseMatrix tmp (v1.matrix_value ());
  return octave_value (tmp. concat (v2.sparse_complex_matrix_value (),
                                    ra_idx));
}

DEFCONV (sparse_complex_matrix_conv, matrix, sparse_complex_matrix)
{
  CAST_CONV_ARG (const octave_matrix&);
  return new octave_sparse_complex_matrix
         (SparseComplexMatrix (v.complex_matrix_value ()));
}

void
install_m_scm_ops (void)
{
  INSTALL_BINOP (op_add, octave_matrix, octave_sparse_complex_matrix, add);
  INSTALL_BINOP (op_sub, octave_matrix, octave_sparse_complex_matrix, sub);
  INSTALL_BINOP (op_mul, octave_matrix, octave_sparse_complex_matrix, mul);
  INSTALL_BINOP (op_div, octave_matrix, octave_sparse_complex_matrix, div);
  INSTALL_BINOP (op_pow, octave_matrix, octave_sparse_complex_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_matrix, octave_sparse_complex_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_matrix, octave_sparse_complex_matrix, lt);
  INSTALL_BINOP (op_le, octave_matrix, octave_sparse_complex_matrix, le);
  INSTALL_BINOP (op_eq, octave_matrix, octave_sparse_complex_matrix, eq);
  INSTALL_BINOP (op_ge, octave_matrix, octave_sparse_complex_matrix, ge);
  INSTALL_BINOP (op_gt, octave_matrix, octave_sparse_complex_matrix, gt);
  INSTALL_BINOP (op_ne, octave_matrix, octave_sparse_complex_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_matrix, octave_sparse_complex_matrix,
                 el_mul);
  INSTALL_BINOP (op_el_div, octave_matrix, octave_sparse_complex_matrix,
                 el_div);
  INSTALL_BINOP (op_el_pow, octave_matrix, octave_sparse_complex_matrix,
                 el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_matrix, octave_sparse_complex_matrix,
                 el_ldiv);
  INSTALL_BINOP (op_el_and, octave_matrix, octave_sparse_complex_matrix,
                 el_and);
  INSTALL_BINOP (op_el_or, octave_matrix, octave_sparse_complex_matrix,
                 el_or);

  INSTALL_CATOP (octave_matrix, octave_sparse_complex_matrix, m_scm);

  INSTALL_ASSIGNCONV (octave_matrix, octave_sparse_complex_matrix,
                      octave_complex_matrix);

  INSTALL_WIDENOP (octave_matrix, octave_sparse_complex_matrix,
                   sparse_complex_matrix_conv);
}
