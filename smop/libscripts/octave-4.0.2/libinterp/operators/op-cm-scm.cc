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
#include "ops.h"
#include "xdiv.h"

#include "sparse-xpow.h"
#include "sparse-xdiv.h"
#include "smx-scm-cm.h"
#include "smx-cm-scm.h"
#include "ov-cx-sparse.h"

// complex matrix by sparse complex matrix ops.

DEFBINOP_OP (add, complex_matrix, sparse_complex_matrix, +)
DEFBINOP_OP (sub, complex_matrix, sparse_complex_matrix, -)

DEFBINOP_OP (mul, complex_matrix, sparse_complex_matrix, *)

DEFBINOP (div, complex_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&,
                   const octave_sparse_complex_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    {
      Complex d = v2.complex_value ();

      if (d == 0.0)
        gripe_divide_by_zero ();

      return octave_value (v1.complex_array_value () / d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();

      ComplexMatrix ret = xdiv (v1.complex_matrix_value (),
                                v2.sparse_complex_matrix_value (), typ);

      v2.matrix_type (typ);
      return ret;
    }
}

DEFBINOPX (pow, complex_matrix, sparse_complex_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
  return octave_value ();
}

DEFBINOP (ldiv, complex_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&,
                   const octave_sparse_complex_matrix&);
  MatrixType typ = v1.matrix_type ();

  ComplexMatrix ret = xleftdiv (v1.complex_matrix_value (),
                                v2.complex_matrix_value (), typ);

  v1.matrix_type (typ);
  return ret;
}

DEFBINOP_FN (mul_trans, complex_matrix, sparse_complex_matrix, mul_trans);
DEFBINOP_FN (mul_herm, complex_matrix, sparse_complex_matrix, mul_herm);

DEFBINOP_FN (lt, complex_matrix, sparse_complex_matrix, mx_el_lt)
DEFBINOP_FN (le, complex_matrix, sparse_complex_matrix, mx_el_le)
DEFBINOP_FN (eq, complex_matrix, sparse_complex_matrix, mx_el_eq)
DEFBINOP_FN (ge, complex_matrix, sparse_complex_matrix, mx_el_ge)
DEFBINOP_FN (gt, complex_matrix, sparse_complex_matrix, mx_el_gt)
DEFBINOP_FN (ne, complex_matrix, sparse_complex_matrix, mx_el_ne)

DEFBINOP_FN (el_mul, complex_matrix, sparse_complex_matrix, product)
DEFBINOP_FN (el_div, complex_matrix, sparse_complex_matrix, quotient)

DEFBINOP (el_pow, complex_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&,
                   const octave_sparse_complex_matrix&);

  return octave_value
         (elem_xpow (SparseComplexMatrix (v1.complex_matrix_value ()),
                     v2.sparse_complex_matrix_value ()));
}

DEFBINOP (el_ldiv, sparse_complex_matrix, matrix)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&,
                   const octave_sparse_complex_matrix&);

  return octave_value (quotient (v2.sparse_complex_matrix_value (),
                                 v1.complex_matrix_value ()));
}

DEFBINOP_FN (el_and, complex_matrix, sparse_complex_matrix, mx_el_and)
DEFBINOP_FN (el_or,  complex_matrix, sparse_complex_matrix, mx_el_or)

DEFCATOP (cm_scm, complex_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (octave_complex_matrix&,
                   const octave_sparse_complex_matrix&);
  SparseComplexMatrix tmp (v1.complex_matrix_value ());
  return octave_value (tmp. concat (v2.sparse_complex_matrix_value (),
                                    ra_idx));
}

DEFCONV (sparse_complex_matrix_conv, complex_matrix,
         sparse_complex_matrix)
{
  CAST_CONV_ARG (const octave_complex_matrix&);
  return new octave_sparse_complex_matrix
         (SparseComplexMatrix (v.complex_matrix_value ()));
}

DEFNDASSIGNOP_FN (assign, complex_matrix, sparse_complex_matrix,
                  complex_array, assign)

void
install_cm_scm_ops (void)
{
  INSTALL_BINOP (op_add, octave_complex_matrix,
                 octave_sparse_complex_matrix, add);
  INSTALL_BINOP (op_sub, octave_complex_matrix,
                 octave_sparse_complex_matrix, sub);
  INSTALL_BINOP (op_mul, octave_complex_matrix,
                 octave_sparse_complex_matrix, mul);
  INSTALL_BINOP (op_div, octave_complex_matrix,
                 octave_sparse_complex_matrix, div);
  INSTALL_BINOP (op_pow, octave_complex_matrix,
                 octave_sparse_complex_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_complex_matrix,
                 octave_sparse_complex_matrix, ldiv);
  INSTALL_BINOP (op_mul_trans, octave_complex_matrix,
                 octave_sparse_complex_matrix, mul_trans);
  INSTALL_BINOP (op_mul_herm, octave_complex_matrix,
                 octave_sparse_complex_matrix, mul_herm);
  INSTALL_BINOP (op_lt, octave_complex_matrix,
                 octave_sparse_complex_matrix, lt);
  INSTALL_BINOP (op_le, octave_complex_matrix,
                 octave_sparse_complex_matrix, le);
  INSTALL_BINOP (op_eq, octave_complex_matrix,
                 octave_sparse_complex_matrix, eq);
  INSTALL_BINOP (op_ge, octave_complex_matrix,
                 octave_sparse_complex_matrix, ge);
  INSTALL_BINOP (op_gt, octave_complex_matrix,
                 octave_sparse_complex_matrix, gt);
  INSTALL_BINOP (op_ne, octave_complex_matrix,
                 octave_sparse_complex_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_complex_matrix,
                 octave_sparse_complex_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_complex_matrix,
                 octave_sparse_complex_matrix, el_div);
  INSTALL_BINOP (op_el_pow, octave_complex_matrix,
                 octave_sparse_complex_matrix, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_complex_matrix,
                 octave_sparse_complex_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_complex_matrix,
                 octave_sparse_complex_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_complex_matrix,
                 octave_sparse_complex_matrix, el_or);

  INSTALL_CATOP (octave_complex_matrix,
                 octave_sparse_complex_matrix, cm_scm);

  INSTALL_ASSIGNOP (op_asn_eq, octave_complex_matrix,
                    octave_sparse_complex_matrix, assign)
  INSTALL_ASSIGNCONV (octave_complex_matrix, octave_sparse_complex_matrix,
                      octave_complex_matrix);

  INSTALL_WIDENOP (octave_complex_matrix, octave_sparse_complex_matrix,
                   sparse_complex_matrix_conv);
}
