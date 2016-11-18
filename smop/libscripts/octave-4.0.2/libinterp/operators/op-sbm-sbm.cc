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
#include "ov-scalar.h"
#include "ops.h"

#include "ov-re-sparse.h"
#include "ov-bool-sparse.h"

// unary sparse bool matrix ops.

DEFUNOP_OP (not, sparse_bool_matrix, !)

DEFUNOP (uplus, sparse_bool_matrix)
{
  CAST_UNOP_ARG (const octave_sparse_bool_matrix&);
  return octave_value (v.sparse_matrix_value ());
}

DEFUNOP (uminus, sparse_bool_matrix)
{
  CAST_UNOP_ARG (const octave_sparse_bool_matrix&);
  return octave_value (- v.sparse_matrix_value ());
}

DEFUNOP (transpose, sparse_bool_matrix)
{
  CAST_UNOP_ARG (const octave_sparse_bool_matrix&);
  return octave_value (v.sparse_bool_matrix_value ().transpose ());
}

// sparse bool matrix by sparse bool matrix ops.

DEFBINOP_FN (eq, sparse_bool_matrix, sparse_bool_matrix, mx_el_eq)
DEFBINOP_FN (ne, sparse_bool_matrix, sparse_bool_matrix, mx_el_ne)
DEFBINOP_FN (el_and, sparse_bool_matrix, sparse_bool_matrix, mx_el_and)
DEFBINOP_FN (el_or,  sparse_bool_matrix, sparse_bool_matrix, mx_el_or)

DEFNDCATOP_FN (sbm_sbm, sparse_bool_matrix, sparse_bool_matrix,
               sparse_bool_matrix, sparse_bool_matrix, concat)
DEFNDCATOP_FN (sbm_sm, sparse_bool_matrix, sparse_matrix, sparse_matrix,
               sparse_matrix, concat)
DEFNDCATOP_FN (sm_sbm, sparse_matrix, sparse_bool_matrix, sparse_matrix,
               sparse_matrix, concat)

DEFASSIGNOP_FN (assign, sparse_bool_matrix, sparse_bool_matrix,
                assign)

CONVDECL (bool_matrix_to_double_matrix)
{
  CAST_CONV_ARG (const octave_sparse_bool_matrix&);

  return new octave_sparse_matrix (SparseMatrix
                                    (v.sparse_bool_matrix_value ()));
}

void
install_sbm_sbm_ops (void)
{
  INSTALL_UNOP (op_not, octave_sparse_bool_matrix, not);
  INSTALL_UNOP (op_uplus, octave_sparse_bool_matrix, uplus);
  INSTALL_UNOP (op_uminus, octave_sparse_bool_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_sparse_bool_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_sparse_bool_matrix, transpose);

  INSTALL_BINOP (op_eq, octave_sparse_bool_matrix,
                 octave_sparse_bool_matrix, eq);
  INSTALL_BINOP (op_ne, octave_sparse_bool_matrix,
                 octave_sparse_bool_matrix, ne);

  INSTALL_BINOP (op_el_and, octave_sparse_bool_matrix,
                 octave_sparse_bool_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_sparse_bool_matrix,
                 octave_sparse_bool_matrix, el_or);

  INSTALL_CATOP (octave_sparse_bool_matrix, octave_sparse_bool_matrix,
                 sbm_sbm);
  INSTALL_CATOP (octave_sparse_bool_matrix, octave_sparse_matrix, sbm_sm);
  INSTALL_CATOP (octave_sparse_matrix, octave_sparse_bool_matrix, sm_sbm);

  INSTALL_ASSIGNOP (op_asn_eq, octave_sparse_bool_matrix,
                    octave_sparse_bool_matrix, assign);

  INSTALL_CONVOP (octave_sparse_bool_matrix, octave_sparse_matrix,
                  bool_matrix_to_double_matrix);
}
