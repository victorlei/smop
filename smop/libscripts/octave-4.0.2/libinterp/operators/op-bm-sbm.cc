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
#include "ov-bool-mat.h"
#include "boolMatrix.h"
#include "ov-scalar.h"
#include "ops.h"

#include "ov-re-sparse.h"
#include "ov-bool-sparse.h"
#include "smx-bm-sbm.h"
#include "smx-sbm-bm.h"

// bool matrix by sparse bool matrix ops.

DEFBINOP_FN (eq, bool_matrix, sparse_bool_matrix, mx_el_eq)
DEFBINOP_FN (ne, bool_matrix, sparse_bool_matrix, mx_el_ne)

DEFBINOP_FN (el_and, bool_matrix, sparse_bool_matrix, mx_el_and)
DEFBINOP_FN (el_or,  bool_matrix, sparse_bool_matrix, mx_el_or)

DEFCATOP (bm_sbm, bool_matrix, sparse_bool_matrix)
{
  CAST_BINOP_ARGS (octave_bool_matrix&, const octave_sparse_bool_matrix&);
  SparseBoolMatrix tmp (v1.bool_matrix_value ());
  return octave_value (tmp. concat (v2.sparse_bool_matrix_value (),
                                    ra_idx));
}

DEFCATOP (m_sbm, matrix, sparse_bool_matrix)
{
  CAST_BINOP_ARGS (octave_matrix&, const octave_sparse_bool_matrix&);
  SparseMatrix tmp (v1.matrix_value ());
  return octave_value (tmp. concat (v2.sparse_matrix_value (), ra_idx));
}

DEFCATOP (bm_sm, bool_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (octave_bool_matrix&, const octave_sparse_matrix&);
  SparseMatrix tmp (v1.matrix_value ());
  return octave_value (tmp. concat (v2.sparse_matrix_value (), ra_idx));
}

DEFCONV (sparse_bool_matrix_conv, bool_matrix, sparse_bool_matrix)
{
  CAST_CONV_ARG (const octave_bool_matrix&);
  return new octave_sparse_bool_matrix
    (SparseBoolMatrix (v.bool_matrix_value ()));
}

DEFNDASSIGNOP_FN (assign, bool_matrix, sparse_bool_matrix, bool_array, assign)

void
install_bm_sbm_ops (void)
{
  INSTALL_BINOP (op_eq, octave_bool_matrix, octave_sparse_bool_matrix, eq);
  INSTALL_BINOP (op_ne, octave_bool_matrix, octave_sparse_bool_matrix, ne);

  INSTALL_BINOP (op_el_and, octave_bool_matrix, octave_sparse_bool_matrix,
                 el_and);
  INSTALL_BINOP (op_el_or, octave_bool_matrix, octave_sparse_bool_matrix,
                 el_or);

  INSTALL_CATOP (octave_bool_matrix, octave_sparse_bool_matrix, bm_sbm);
  INSTALL_CATOP (octave_bool_matrix, octave_sparse_matrix, bm_sm);
  INSTALL_CATOP (octave_matrix, octave_sparse_bool_matrix, m_sbm);

  INSTALL_ASSIGNOP (op_asn_eq, octave_bool_matrix, octave_sparse_bool_matrix,
                    assign)
  INSTALL_ASSIGNCONV (octave_bool_matrix, octave_sparse_bool_matrix,
                      octave_bool_matrix);

  INSTALL_WIDENOP (octave_bool_matrix, octave_sparse_bool_matrix,
                   sparse_bool_matrix_conv);
}
