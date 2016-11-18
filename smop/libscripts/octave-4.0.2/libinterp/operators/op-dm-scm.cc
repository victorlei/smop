/*

Copyright (C) 2009-2015 Jason Riedy, Jaroslav Hajek

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

#include "mx-cm-s.h"
#include "mx-s-cm.h"

#include "mx-dm-cs.h"
#include "mx-cs-dm.h"

#include "mx-m-cs.h"
#include "mx-cs-m.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-typeinfo.h"
#include "ops.h"

#include "ov-re-diag.h"
#include "ov-cx-diag.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

#include "sparse-xdiv.h"

// diagonal matrix by sparse matrix ops

DEFBINOP (mul_dm_scm, diag_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_diag_matrix&,
                   const octave_sparse_complex_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.diag_matrix_value () * d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();
      SparseComplexMatrix ret = v1.diag_matrix_value () *
                                v2.sparse_complex_matrix_value ();
      octave_value out = octave_value (ret);
      typ.mark_as_unsymmetric ();
      out.matrix_type (typ);
      return out;
    }
}

DEFBINOP (mul_cdm_sm, complex_diag_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_sparse_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.scalar_value ();

      return octave_value (v1.complex_diag_matrix_value () * d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();
      SparseComplexMatrix ret = v1.complex_diag_matrix_value () *
                                v2.sparse_matrix_value ();
      octave_value out = octave_value (ret);
      typ.mark_as_unsymmetric ();
      out.matrix_type (typ);
      return out;
    }
}

DEFBINOP (mul_cdm_scm, complex_diag_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_sparse_complex_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.complex_diag_matrix_value () * d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();
      SparseComplexMatrix ret = v1.complex_diag_matrix_value () *
                                v2.sparse_complex_matrix_value ();
      octave_value out = octave_value (ret);
      typ.mark_as_unsymmetric ();
      out.matrix_type (typ);
      return out;
    }
}

DEFBINOP (ldiv_dm_scm, diag_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_diag_matrix&,
                   const octave_sparse_complex_matrix&);

  MatrixType typ = v2.matrix_type ();
  return xleftdiv (v1.diag_matrix_value (), v2.sparse_complex_matrix_value (),
                   typ);
}

DEFBINOP (ldiv_cdm_sm, complex_diag_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_sparse_matrix&);

  MatrixType typ = v2.matrix_type ();
  return xleftdiv (v1.complex_diag_matrix_value (), v2.sparse_matrix_value (),
                   typ);
}

DEFBINOP (ldiv_cdm_scm, complex_diag_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_sparse_complex_matrix&);

  MatrixType typ = v2.matrix_type ();
  return xleftdiv (v1.complex_diag_matrix_value (),
                   v2.sparse_complex_matrix_value (),
                   typ);
}

DEFBINOP (add_dm_scm, diag_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_diag_matrix&,
                   const octave_sparse_complex_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.matrix_value () + d);
    }
  else
    return v1.diag_matrix_value () + v2.sparse_complex_matrix_value ();
}

DEFBINOP (add_cdm_sm, complex_diag_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_sparse_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v2.scalar_value ();

      return octave_value (v1.complex_matrix_value () + d);
    }
  else
    return v1.complex_diag_matrix_value () + v2.sparse_matrix_value ();
}

DEFBINOP (add_cdm_scm, complex_diag_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_sparse_complex_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.complex_matrix_value () + d);
    }
  else
    return v1.complex_diag_matrix_value () + v2.sparse_complex_matrix_value ();
}

DEFBINOP (sub_dm_scm, diag_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_diag_matrix&,
                   const octave_sparse_complex_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.matrix_value () + (-d));
    }
  else
    return v1.diag_matrix_value () - v2.sparse_complex_matrix_value ();
}

DEFBINOP (sub_cdm_sm, complex_diag_matrix, sparse_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_sparse_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v2.scalar_value ();

      return octave_value (v1.complex_matrix_value () + (-d));
    }
  else
    return v1.complex_diag_matrix_value () - v2.sparse_matrix_value ();
}

DEFBINOP (sub_cdm_scm, complex_diag_matrix, sparse_complex_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_sparse_complex_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.complex_matrix_value () + (-d));
    }
  else
    return v1.complex_diag_matrix_value () - v2.sparse_complex_matrix_value ();
}

// sparse matrix by diagonal matrix ops

DEFBINOP (mul_scm_dm, sparse_complex_matrix, diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_complex_matrix&,
                   const octave_diag_matrix&);

  if (v1.rows () == 1 && v1.columns () == 1)
    // If v1 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v1.complex_value ();

      return octave_value (d * v2.diag_matrix_value ());
    }
  else
    {
      MatrixType typ = v1.matrix_type ();
      SparseComplexMatrix ret = v1.sparse_complex_matrix_value () *
                                v2.diag_matrix_value ();
      octave_value out = octave_value (ret);
      typ.mark_as_unsymmetric ();
      out.matrix_type (typ);
      return out;
    }
}

DEFBINOP (mul_sm_cdm, sparse_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&,
                   const octave_complex_diag_matrix&);

  if (v1.rows () == 1 && v1.columns () == 1)
    // If v1 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v1.complex_value ();

      return octave_value (d * v2.complex_diag_matrix_value ());
    }
  else
    {
      MatrixType typ = v1.matrix_type ();
      SparseComplexMatrix ret = v1.sparse_matrix_value () *
                                v2.complex_diag_matrix_value ();
      octave_value out = octave_value (ret);
      typ.mark_as_unsymmetric ();
      out.matrix_type (typ);
      return out;
    }
}

DEFBINOP (mul_scm_cdm, sparse_complex_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_complex_matrix&,
                   const octave_complex_diag_matrix&);

  if (v1.rows () == 1 && v1.columns () == 1)
    // If v1 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v1.complex_value ();

      return octave_value (d * v2.complex_diag_matrix_value ());
    }
  else if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, don't bother with further dispatching.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.sparse_complex_matrix_value () * d);
    }
  else
    {
      MatrixType typ = v1.matrix_type ();
      SparseComplexMatrix ret = v1.sparse_complex_matrix_value () *
                                v2.complex_diag_matrix_value ();
      octave_value out = octave_value (ret);
      typ.mark_as_unsymmetric ();
      out.matrix_type (typ);
      return out;
    }
}

DEFBINOP (div_scm_dm, sparse_complex_matrix, diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_complex_matrix&,
                   const octave_diag_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    {
      double d = v2.scalar_value ();

      if (d == 0.0)
        gripe_divide_by_zero ();

      return octave_value (v1.sparse_complex_matrix_value () / d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();
      return xdiv (v1.sparse_complex_matrix_value (),
                   v2.diag_matrix_value (), typ);
    }
}

DEFBINOP (div_sm_cdm, sparse_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&,
                   const octave_complex_diag_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    {
      std::complex<double> d = v2.complex_value ();

      if (d == 0.0)
        gripe_divide_by_zero ();

      return octave_value (v1.sparse_matrix_value () / d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();
      return xdiv (v1.sparse_matrix_value (),
                   v2.complex_diag_matrix_value (), typ);
    }
}

DEFBINOP (div_scm_cdm, sparse_complex_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_complex_matrix&,
                   const octave_complex_diag_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    {
      std::complex<double> d = v2.complex_value ();

      if (d == 0.0)
        gripe_divide_by_zero ();

      return octave_value (v1.sparse_complex_matrix_value () / d);
    }
  else
    {
      MatrixType typ = v2.matrix_type ();
      return xdiv (v1.sparse_complex_matrix_value (),
                   v2.complex_diag_matrix_value (), typ);
    }
}

DEFBINOP (add_sm_cdm, sparse_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&,
                   const octave_complex_diag_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.sparse_matrix_value () + d);
    }
  else
    return v1.sparse_matrix_value () + v2.complex_diag_matrix_value ();
}

DEFBINOP (add_scm_dm, sparse_complex_matrix, diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_complex_matrix&,
                   const octave_diag_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v2.scalar_value ();

      return octave_value (v1.sparse_complex_matrix_value () + d);
    }
  else
    return v1.sparse_complex_matrix_value () + v2.diag_matrix_value ();
}

DEFBINOP (add_scm_cdm, sparse_complex_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_complex_matrix&,
                   const octave_complex_diag_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.sparse_complex_matrix_value () + d);
    }
  else
    return v1.sparse_complex_matrix_value () + v2.complex_diag_matrix_value ();
}

DEFBINOP (sub_sm_cdm, sparse_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_matrix&,
                   const octave_complex_diag_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.sparse_matrix_value () + (-d));
    }
  else
    return v1.sparse_matrix_value () - v2.complex_diag_matrix_value ();
}

DEFBINOP (sub_scm_dm, sparse_complex_matrix, diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_complex_matrix&,
                   const octave_diag_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      double d = v2.scalar_value ();

      return octave_value (v1.sparse_complex_matrix_value () + (-d));
    }
  else
    return v1.sparse_complex_matrix_value () - v2.diag_matrix_value ();
}

DEFBINOP (sub_scm_cdm, sparse_complex_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_sparse_complex_matrix&,
                   const octave_complex_diag_matrix&);

  if (v2.rows () == 1 && v2.columns () == 1)
    // If v2 is a scalar in disguise, return a diagonal matrix rather than
    // a sparse matrix.
    {
      std::complex<double> d = v2.complex_value ();

      return octave_value (v1.sparse_complex_matrix_value () + (-d));
    }
  else
    return v1.sparse_complex_matrix_value () - v2.complex_diag_matrix_value ();
}

void
install_dm_scm_ops (void)
{
  INSTALL_BINOP (op_mul, octave_diag_matrix, octave_sparse_complex_matrix,
                 mul_dm_scm);
  INSTALL_BINOP (op_mul, octave_complex_diag_matrix, octave_sparse_matrix,
                 mul_cdm_sm);
  INSTALL_BINOP (op_mul, octave_complex_diag_matrix,
                 octave_sparse_complex_matrix, mul_cdm_scm);
  INSTALL_BINOP (op_ldiv, octave_diag_matrix, octave_sparse_complex_matrix,
                 ldiv_dm_scm);
  INSTALL_BINOP (op_ldiv, octave_complex_diag_matrix, octave_sparse_matrix,
                 ldiv_cdm_sm);
  INSTALL_BINOP (op_ldiv, octave_complex_diag_matrix,
                 octave_sparse_complex_matrix, ldiv_cdm_scm);

  INSTALL_BINOP (op_add, octave_diag_matrix, octave_sparse_complex_matrix,
                 add_dm_scm);
  INSTALL_BINOP (op_add, octave_complex_diag_matrix, octave_sparse_matrix,
                 add_cdm_sm);
  INSTALL_BINOP (op_add, octave_complex_diag_matrix,
                 octave_sparse_complex_matrix, add_cdm_scm);
  INSTALL_BINOP (op_sub, octave_diag_matrix, octave_sparse_complex_matrix,
                 sub_dm_scm);
  INSTALL_BINOP (op_sub, octave_complex_diag_matrix, octave_sparse_matrix,
                 sub_cdm_sm);
  INSTALL_BINOP (op_sub, octave_complex_diag_matrix,
                 octave_sparse_complex_matrix, sub_cdm_scm);

  INSTALL_BINOP (op_mul, octave_sparse_complex_matrix, octave_diag_matrix,
                 mul_scm_dm);
  INSTALL_BINOP (op_mul, octave_sparse_matrix, octave_complex_diag_matrix,
                 mul_sm_cdm);
  INSTALL_BINOP (op_mul, octave_sparse_complex_matrix,
                 octave_complex_diag_matrix, mul_scm_cdm);

  INSTALL_BINOP (op_div, octave_sparse_complex_matrix, octave_diag_matrix,
                 div_scm_dm);
  INSTALL_BINOP (op_div, octave_sparse_matrix, octave_complex_diag_matrix,
                 div_sm_cdm);
  INSTALL_BINOP (op_div, octave_sparse_complex_matrix,
                 octave_complex_diag_matrix, div_scm_cdm);

  INSTALL_BINOP (op_add, octave_sparse_complex_matrix, octave_diag_matrix,
                 add_scm_dm);
  INSTALL_BINOP (op_add, octave_sparse_matrix, octave_complex_diag_matrix,
                 add_sm_cdm);
  INSTALL_BINOP (op_add, octave_sparse_complex_matrix,
                 octave_complex_diag_matrix, add_scm_cdm);
  INSTALL_BINOP (op_sub, octave_sparse_complex_matrix, octave_diag_matrix,
                 sub_scm_dm);
  INSTALL_BINOP (op_sub, octave_sparse_matrix, octave_complex_diag_matrix,
                 sub_sm_cdm);
  INSTALL_BINOP (op_sub, octave_sparse_complex_matrix,
                 octave_complex_diag_matrix, sub_scm_cdm);
}
