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

#include <cassert>

#include "Array-util.h"
#include "lo-array-gripes.h"
#include "oct-cmplx.h"
#include "quit.h"
#include "error.h"
#include "lo-ieee.h"

#include "dSparse.h"
#include "dDiagMatrix.h"
#include "CSparse.h"
#include "CDiagMatrix.h"
#include "oct-spparms.h"
#include "sparse-xdiv.h"

static void
solve_singularity_warning (double rcond)
{
  gripe_singular_matrix (rcond);
}

template <class T1, class T2>
bool
mx_leftdiv_conform (const T1& a, const T2& b)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type b_nr = b.rows ();

  if (a_nr != b_nr)
    {
      octave_idx_type a_nc = a.cols ();
      octave_idx_type b_nc = b.cols ();

      gripe_nonconformant ("operator \\", a_nr, a_nc, b_nr, b_nc);
      return false;
    }

  return true;
}

#define INSTANTIATE_MX_LEFTDIV_CONFORM(T1, T2) \
  template bool mx_leftdiv_conform (const T1&, const T2&)

INSTANTIATE_MX_LEFTDIV_CONFORM (SparseMatrix, SparseMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseMatrix, SparseComplexMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseComplexMatrix, SparseMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseComplexMatrix, SparseComplexMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseMatrix, Matrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseMatrix, ComplexMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseComplexMatrix, Matrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (SparseComplexMatrix, ComplexMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (DiagMatrix, SparseMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (DiagMatrix, SparseComplexMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (ComplexDiagMatrix, SparseMatrix);
INSTANTIATE_MX_LEFTDIV_CONFORM (ComplexDiagMatrix, SparseComplexMatrix);

template <class T1, class T2>
bool
mx_div_conform (const T1& a, const T2& b)
{
  octave_idx_type a_nc = a.cols ();
  octave_idx_type b_nc = b.cols ();

  if (a_nc != b_nc)
    {
      octave_idx_type a_nr = a.rows ();
      octave_idx_type b_nr = b.rows ();

      gripe_nonconformant ("operator /", a_nr, a_nc, b_nr, b_nc);
      return false;
    }

  return true;
}

#define INSTANTIATE_MX_DIV_CONFORM(T1, T2) \
  template bool mx_div_conform (const T1&, const T2&)

INSTANTIATE_MX_DIV_CONFORM (SparseMatrix, SparseMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseMatrix, SparseComplexMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseComplexMatrix, SparseMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseComplexMatrix, SparseComplexMatrix);
INSTANTIATE_MX_DIV_CONFORM (Matrix, SparseMatrix);
INSTANTIATE_MX_DIV_CONFORM (Matrix, SparseComplexMatrix);
INSTANTIATE_MX_DIV_CONFORM (ComplexMatrix, SparseMatrix);
INSTANTIATE_MX_DIV_CONFORM (ComplexMatrix, SparseComplexMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseMatrix, DiagMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseMatrix, ComplexDiagMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseComplexMatrix, DiagMatrix);
INSTANTIATE_MX_DIV_CONFORM (SparseComplexMatrix, ComplexDiagMatrix);

// Right division functions.  X / Y = X * inv (Y) = (inv (Y') * X')'
//
//                  Y / X:   m   cm   sm  scm
//                   +--   +---+----+----+----+
//   sparse matrix         | 1 |  3 |  5 |  7 |
//                         +---+----+----+----+
//   sparse complex_matrix | 2 |  4 |  6 |  8 |
//                         +---+----+----+----+
//   diagonal matrix                |  9 | 11 |
//                                  +----+----+
//   complex diag. matrix           | 10 | 12 |
//                                  +----+----+

// -*- 1 -*-
Matrix
xdiv (const Matrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return Matrix ();

  Matrix atmp = a.transpose ();
  SparseMatrix btmp = b.transpose ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  Matrix result = btmp.solve (btyp, atmp, info, rcond,
                              solve_singularity_warning);

  typ = btyp.transpose ();
  return result.transpose ();
}

// -*- 2 -*-
ComplexMatrix
xdiv (const Matrix& a, const SparseComplexMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  Matrix atmp = a.transpose ();
  SparseComplexMatrix btmp = b.hermitian ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  ComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 3 -*-
ComplexMatrix
xdiv (const ComplexMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  ComplexMatrix atmp = a.hermitian ();
  SparseMatrix btmp = b.transpose ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  ComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 4 -*-
ComplexMatrix
xdiv (const ComplexMatrix& a, const SparseComplexMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return ComplexMatrix ();

  ComplexMatrix atmp = a.hermitian ();
  SparseComplexMatrix btmp = b.hermitian ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  ComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 5 -*-
SparseMatrix
xdiv (const SparseMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return SparseMatrix ();

  SparseMatrix atmp = a.transpose ();
  SparseMatrix btmp = b.transpose ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  SparseMatrix result = btmp.solve (btyp, atmp, info, rcond,
                                    solve_singularity_warning);

  typ = btyp.transpose ();
  return result.transpose ();
}

// -*- 6 -*-
SparseComplexMatrix
xdiv (const SparseMatrix& a, const SparseComplexMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return SparseComplexMatrix ();

  SparseMatrix atmp = a.transpose ();
  SparseComplexMatrix btmp = b.hermitian ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  SparseComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 7 -*-
SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return SparseComplexMatrix ();

  SparseComplexMatrix atmp = a.hermitian ();
  SparseMatrix btmp = b.transpose ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  SparseComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

// -*- 8 -*-
SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const SparseComplexMatrix& b,
      MatrixType &typ)
{
  if (! mx_div_conform (a, b))
    return SparseComplexMatrix ();

  SparseComplexMatrix atmp = a.hermitian ();
  SparseComplexMatrix btmp = b.hermitian ();
  MatrixType btyp = typ.transpose ();

  octave_idx_type info;
  double rcond = 0.0;
  SparseComplexMatrix result
    = btmp.solve (btyp, atmp, info, rcond, solve_singularity_warning);

  typ = btyp.transpose ();
  return result.hermitian ();
}

template <typename RT, typename SM, typename DM>
RT do_rightdiv_sm_dm (const SM& a, const DM& d)
{
  const octave_idx_type d_nr = d.rows ();

  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  using std::min;
  const octave_idx_type nc = min (d_nr, a_nc);

  if (! mx_div_conform (a, d))
    return RT ();

  const octave_idx_type nz = a.nnz ();
  RT r (a_nr, nc, nz);

  typedef typename DM::element_type DM_elt_type;
  const DM_elt_type zero = DM_elt_type ();

  octave_idx_type k_result = 0;
  for (octave_idx_type j = 0; j < nc; ++j)
    {
      octave_quit ();
      const DM_elt_type s = d.dgelem (j);
      const octave_idx_type colend = a.cidx (j+1);
      r.xcidx (j) = k_result;
      if (s != zero)
        for (octave_idx_type k = a.cidx (j); k < colend; ++k)
          {
            r.xdata (k_result) = a.data (k) / s;
            r.xridx (k_result) = a.ridx (k);
            ++k_result;
          }
    }
  r.xcidx (nc) = k_result;

  r.maybe_compress (true);
  return r;
}

// -*- 9 -*-
SparseMatrix
xdiv (const SparseMatrix& a, const DiagMatrix& b, MatrixType &)
{
  return do_rightdiv_sm_dm<SparseMatrix> (a, b);
}

// -*- 10 -*-
SparseComplexMatrix
xdiv (const SparseMatrix& a, const ComplexDiagMatrix& b, MatrixType &)
{
  return do_rightdiv_sm_dm<SparseComplexMatrix> (a, b);
}

// -*- 11 -*-
SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const DiagMatrix& b, MatrixType &)
{
  return do_rightdiv_sm_dm<SparseComplexMatrix> (a, b);
}

// -*- 12 -*-
SparseComplexMatrix
xdiv (const SparseComplexMatrix& a, const ComplexDiagMatrix& b, MatrixType &)
{
  return do_rightdiv_sm_dm<SparseComplexMatrix> (a, b);
}

// Funny element by element division operations.
//
//       op2 \ op1:   s   cs
//            +--   +---+----+
//   matrix         | 1 |  3 |
//                  +---+----+
//   complex_matrix | 2 |  4 |
//                  +---+----+

Matrix
x_el_div (double a, const SparseMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  Matrix result;
  if (a == 0.)
    result = Matrix (nr, nc, octave_NaN);
  else if (a > 0.)
    result = Matrix (nr, nc, octave_Inf);
  else
    result = Matrix (nr, nc, -octave_Inf);


  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = b.cidx (j); i < b.cidx (j+1); i++)
      {
        octave_quit ();
        result.elem (b.ridx (i), j) = a / b.data (i);
      }

  return result;
}

ComplexMatrix
x_el_div (double a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix  result (nr, nc, Complex (octave_NaN, octave_NaN));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = b.cidx (j); i < b.cidx (j+1); i++)
      {
        octave_quit ();
        result.elem (b.ridx (i), j) = a / b.data (i);
      }

  return result;
}

ComplexMatrix
x_el_div (const Complex a, const SparseMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc, (a / 0.0));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = b.cidx (j); i < b.cidx (j+1); i++)
      {
        octave_quit ();
        result.elem (b.ridx (i), j) = a / b.data (i);
      }

  return result;
}

ComplexMatrix
x_el_div (const Complex a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc, (a / 0.0));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = b.cidx (j); i < b.cidx (j+1); i++)
      {
        octave_quit ();
        result.elem (b.ridx (i), j) = a / b.data (i);
      }

  return result;
}

// Left division functions.  X \ Y = inv (X) * Y
//
//               Y  \  X :   sm  scm  dm  dcm
//                   +--   +---+----+
//   matrix                | 1 |  5 |
//                         +---+----+
//   complex_matrix        | 2 |  6 |
//                         +---+----+----+----+
//   sparse matrix         | 3 |  7 |  9 | 11 |
//                         +---+----+----+----+
//   sparse complex_matrix | 4 |  8 | 10 | 12 |
//                         +---+----+----+----+

// -*- 1 -*-
Matrix
xleftdiv (const SparseMatrix& a, const Matrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return Matrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 2 -*-
ComplexMatrix
xleftdiv (const SparseMatrix& a, const ComplexMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 3 -*-
SparseMatrix
xleftdiv (const SparseMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 4 -*-
SparseComplexMatrix
xleftdiv (const SparseMatrix& a, const SparseComplexMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 5 -*-
ComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const Matrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 6 -*-
ComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const ComplexMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return ComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 7 -*-
SparseComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const SparseMatrix& b, MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

// -*- 8 -*-
SparseComplexMatrix
xleftdiv (const SparseComplexMatrix& a, const SparseComplexMatrix& b,
          MatrixType &typ)
{
  if (! mx_leftdiv_conform (a, b))
    return SparseComplexMatrix ();

  octave_idx_type info;
  double rcond = 0.0;
  return a.solve (typ, b, info, rcond, solve_singularity_warning);
}

template <typename RT, typename DM, typename SM>
RT do_leftdiv_dm_sm (const DM& d, const SM& a)
{
  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  const octave_idx_type d_nc = d.cols ();

  using std::min;
  const octave_idx_type nr = min (d_nc, a_nr);

  if (! mx_leftdiv_conform (d, a))
    return RT ();

  const octave_idx_type nz = a.nnz ();
  RT r (nr, a_nc, nz);

  typedef typename DM::element_type DM_elt_type;
  const DM_elt_type zero = DM_elt_type ();

  octave_idx_type k_result = 0;
  for (octave_idx_type j = 0; j < a_nc; ++j)
    {
      octave_quit ();
      const octave_idx_type colend = a.cidx (j+1);
      r.xcidx (j) = k_result;
      for (octave_idx_type k = a.cidx (j); k < colend; ++k)
        {
          const octave_idx_type i = a.ridx (k);
          if (i < nr)
            {
              const DM_elt_type s = d.dgelem (i);
              if (s != zero)
                {
                  r.xdata (k_result) = a.data (k) / s;
                  r.xridx (k_result) = i;
                  ++k_result;
                }
            }
        }
    }
  r.xcidx (a_nc) = k_result;

  r.maybe_compress (true);
  return r;
}

// -*- 9 -*-
SparseMatrix
xleftdiv (const DiagMatrix& d, const SparseMatrix& a,  MatrixType&)
{
  return do_leftdiv_dm_sm<SparseMatrix> (d, a);
}

// -*- 10 -*-
SparseComplexMatrix
xleftdiv (const DiagMatrix& d, const SparseComplexMatrix& a,  MatrixType&)
{
  return do_leftdiv_dm_sm<SparseComplexMatrix> (d, a);
}

// -*- 11 -*-
SparseComplexMatrix
xleftdiv (const ComplexDiagMatrix& d, const SparseMatrix& a,  MatrixType&)
{
  return do_leftdiv_dm_sm<SparseComplexMatrix> (d, a);
}

// -*- 12 -*-
SparseComplexMatrix
xleftdiv (const ComplexDiagMatrix& d, const SparseComplexMatrix& a,
          MatrixType&)
{
  return do_leftdiv_dm_sm<SparseComplexMatrix> (d, a);
}
