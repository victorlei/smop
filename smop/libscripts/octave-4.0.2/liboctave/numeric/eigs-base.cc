/*

Copyright (C) 2005-2015 David Bateman

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

#include <cfloat>
#include <cmath>
#include <vector>
#include <iostream>

#include "f77-fcn.h"
#include "oct-locbuf.h"
#include "quit.h"
#include "SparsedbleLU.h"
#include "SparseCmplxLU.h"
#include "dSparse.h"
#include "CSparse.h"
#include "MatrixType.h"
#include "SparsedbleCHOL.h"
#include "SparseCmplxCHOL.h"
#include "oct-rand.h"
#include "dbleCHOL.h"
#include "CmplxCHOL.h"
#include "dbleLU.h"
#include "CmplxLU.h"

#ifdef HAVE_ARPACK
typedef ColumnVector (*EigsFunc) (const ColumnVector &x, int &eigs_error);
typedef ComplexColumnVector (*EigsComplexFunc)
  (const ComplexColumnVector &x, int &eigs_error);

// Arpack and blas fortran functions we call.
extern "C"
{
  F77_RET_T
  F77_FUNC (dsaupd, DSAUPD) (octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const double&,
                             double*, const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             octave_idx_type*, double*, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dseupd, DSEUPD) (const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type*, double*, double*,
                             const octave_idx_type&, const double&,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const double&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             octave_idx_type*, double*, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dnaupd, DNAUPD) (octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type&, const double&,
                             double*, const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             octave_idx_type*, double*, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dneupd, DNEUPD) (const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type*, double*, double*,
                             double*, const octave_idx_type&, const double&,
                             const double&, double*,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type&, const double&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             octave_idx_type*, double*, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (znaupd, ZNAUPD) (octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const double&,
                             Complex*, const octave_idx_type&, Complex*,
                             const octave_idx_type&, octave_idx_type*,
                             octave_idx_type*, Complex*, Complex*,
                             const octave_idx_type&, double *, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zneupd, ZNEUPD) (const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type*, Complex*, Complex*,
                             const octave_idx_type&, const Complex&, Complex*,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const double&,
                             Complex*, const octave_idx_type&, Complex*,
                             const octave_idx_type&, octave_idx_type*,
                             octave_idx_type*, Complex*, Complex*,
                             const octave_idx_type&, double *, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgemv, DGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const double&, const double*,
                           const octave_idx_type&, const double*,
                           const octave_idx_type&, const double&, double*,
                           const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL);


  F77_RET_T
  F77_FUNC (zgemv, ZGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const Complex&, const Complex*,
                           const octave_idx_type&, const Complex*,
                           const octave_idx_type&, const Complex&, Complex*,
                           const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL);

}


#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
static octave_idx_type
lusolve (const SparseMatrix&, const SparseMatrix&, Matrix&);

static octave_idx_type
lusolve (const SparseComplexMatrix&, const SparseComplexMatrix&,
         ComplexMatrix&);

static octave_idx_type
lusolve (const Matrix&, const Matrix&, Matrix&);

static octave_idx_type
lusolve (const ComplexMatrix&, const ComplexMatrix&, ComplexMatrix&);

static ComplexMatrix
ltsolve (const SparseComplexMatrix&, const ColumnVector&,
         const ComplexMatrix&);

static Matrix
ltsolve (const SparseMatrix&, const ColumnVector&, const Matrix&,);

static ComplexMatrix
ltsolve (const ComplexMatrix&, const ColumnVector&, const ComplexMatrix&);

static Matrix
ltsolve (const Matrix&, const ColumnVector&, const Matrix&,);

static ComplexMatrix
utsolve (const SparseComplexMatrix&, const ColumnVector&, const ComplexMatrix&);

static Matrix
utsolve (const SparseMatrix&, const ColumnVector&, const Matrix&);

static ComplexMatrix
utsolve (const ComplexMatrix&, const ColumnVector&, const ComplexMatrix&);

static Matrix
utsolve (const Matrix&, const ColumnVector&, const Matrix&);

#endif

static void
warn_convergence (void)
{
  (*current_liboctave_warning_with_id_handler)
    ("Octave:convergence",
     "eigs: 'A - sigma*B' is singular, indicating sigma is exactly "
     "an eigenvalue so convergence is not guaranteed");
}

template <class M, class SM>
static octave_idx_type
lusolve (const SM& L, const SM& U, M& m)
{
  octave_idx_type err = 0;
  double rcond;
  MatrixType utyp (MatrixType::Upper);

  // Sparse L is lower triangular, Dense L is permuted lower triangular!!!
  m = L.solve (m, err, rcond, 0);
  if (err)
    return err;

  m = U.solve (utyp, m, err, rcond, 0);

  return err;
}

template <class SM, class M>
static M
ltsolve (const SM& L, const ColumnVector& Q, const M& m)
{
  octave_idx_type n = L.cols ();
  octave_idx_type b_nc = m.cols ();
  octave_idx_type err = 0;
  double rcond;
  MatrixType ltyp (MatrixType::Lower);
  M tmp = L.solve (ltyp, m, err, rcond, 0);
  M retval;
  const double* qv = Q.fortran_vec ();

  if (!err)
    {
      retval.resize (n, b_nc);
      for (octave_idx_type j = 0; j < b_nc; j++)
        {
          for (octave_idx_type i = 0; i < n; i++)
            retval.elem (static_cast<octave_idx_type>(qv[i]), j) =
              tmp.elem (i,j);
        }
    }

  return retval;
}

template <class SM, class M>
static M
utsolve (const SM& U, const ColumnVector& Q, const M& m)
{
  octave_idx_type n = U.cols ();
  octave_idx_type b_nc = m.cols ();
  octave_idx_type err = 0;
  double rcond;
  MatrixType utyp (MatrixType::Upper);

  M retval (n, b_nc);
  const double* qv = Q.fortran_vec ();
  for (octave_idx_type j = 0; j < b_nc; j++)
    {
      for (octave_idx_type i = 0; i < n; i++)
        retval.elem (i,j) = m.elem (static_cast<octave_idx_type>(qv[i]), j);
    }
  return U.solve (utyp, retval, err, rcond, 0);
}

static bool
vector_product (const SparseMatrix& m, const double* x, double* y)
{
  octave_idx_type nc = m.cols ();

  for (octave_idx_type j = 0; j < nc; j++)
    y[j] = 0.;

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
      y[m.ridx (i)] += m.data (i) * x[j];

  return true;
}

static bool
vector_product (const Matrix& m, const double *x, double *y)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.cols ();

  F77_XFCN (dgemv, DGEMV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           nr, nc, 1.0,  m.data (), nr,
                           x, 1, 0.0, y, 1
                           F77_CHAR_ARG_LEN (1)));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable error in dgemv");
      return false;
    }
  else
    return true;
}

static bool
vector_product (const SparseComplexMatrix& m, const Complex* x,
                Complex* y)
{
  octave_idx_type nc = m.cols ();

  for (octave_idx_type j = 0; j < nc; j++)
    y[j] = 0.;

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
      y[m.ridx (i)] += m.data (i) * x[j];

  return true;
}

static bool
vector_product (const ComplexMatrix& m, const Complex *x, Complex *y)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.cols ();

  F77_XFCN (zgemv, ZGEMV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           nr, nc, 1.0,  m.data (), nr,
                           x, 1, 0.0, y, 1
                           F77_CHAR_ARG_LEN (1)));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable error in zgemv");
      return false;
    }
  else
    return true;
}

static bool
make_cholb (Matrix& b, Matrix& bt, ColumnVector& permB)
{
  octave_idx_type info;
  CHOL fact (b, info);
  octave_idx_type n = b.cols ();

  if (info != 0)
    return false;
  else
    {
      bt = fact.chol_matrix ();
      b = bt.transpose ();
      permB = ColumnVector (n);
      for (octave_idx_type i = 0; i < n; i++)
        permB(i) = i;
      return true;
    }
}

static bool
make_cholb (SparseMatrix& b, SparseMatrix& bt, ColumnVector& permB)
{
  octave_idx_type info;
  SparseCHOL fact (b, info, false);

  if (fact.P () != 0)
    return false;
  else
    {
      b = fact.L ();
      bt = b.transpose ();
      permB = fact.perm () - 1.0;
      return true;
    }
}

static bool
make_cholb (ComplexMatrix& b, ComplexMatrix& bt, ColumnVector& permB)
{
  octave_idx_type info;
  ComplexCHOL fact (b, info);
  octave_idx_type n = b.cols ();

  if (info != 0)
    return false;
  else
    {
      bt = fact.chol_matrix ();
      b = bt.hermitian ();
      permB = ColumnVector (n);
      for (octave_idx_type i = 0; i < n; i++)
        permB(i) = i;
      return true;
    }
}

static bool
make_cholb (SparseComplexMatrix& b, SparseComplexMatrix& bt,
            ColumnVector& permB)
{
  octave_idx_type info;
  SparseComplexCHOL fact (b, info, false);

  if (fact.P () != 0)
    return false;
  else
    {
      b = fact.L ();
      bt = b.hermitian ();
      permB = fact.perm () - 1.0;
      return true;
    }
}

static bool
LuAminusSigmaB (const SparseMatrix &m, const SparseMatrix &b,
                bool cholB, const ColumnVector& permB, double sigma,
                SparseMatrix &L, SparseMatrix &U, octave_idx_type *P,
                octave_idx_type *Q)
{
  bool have_b = ! b.is_empty ();
  octave_idx_type n = m.rows ();

  // Caclulate LU decomposition of 'A - sigma * B'
  SparseMatrix AminusSigmaB (m);

  if (have_b)
    {
      if (cholB)
        {
          if (permB.length ())
            {
              SparseMatrix tmp(n,n,n);
              for (octave_idx_type i = 0; i < n; i++)
                {
                  tmp.xcidx (i) = i;
                  tmp.xridx (i) =
                    static_cast<octave_idx_type>(permB(i));
                  tmp.xdata (i) = 1;
                }
              tmp.xcidx (n) = n;

              AminusSigmaB = AminusSigmaB - sigma * tmp *
                             b.transpose () * b * tmp.transpose ();
            }
          else
            AminusSigmaB = AminusSigmaB - sigma *
                           b.transpose () * b;
        }
      else
        AminusSigmaB = AminusSigmaB - sigma * b;
    }
  else
    {
      SparseMatrix sigmat (n, n, n);

      // Create sigma * speye (n,n)
      sigmat.xcidx (0) = 0;
      for (octave_idx_type i = 0; i < n; i++)
        {
          sigmat.xdata (i) = sigma;
          sigmat.xridx (i) = i;
          sigmat.xcidx (i+1) = i + 1;
        }

      AminusSigmaB = AminusSigmaB - sigmat;
    }

  SparseLU fact (AminusSigmaB);

  L = fact.L ();
  U = fact.U ();
  const octave_idx_type *P2 = fact.row_perm ();
  const octave_idx_type *Q2 = fact.col_perm ();

  for (octave_idx_type j = 0; j < n; j++)
    {
      P[j] = P2[j];
      Q[j] = Q2[j];
    }

  // Test condition number of LU decomposition
  double minU = octave_NaN;
  double maxU = octave_NaN;
  for (octave_idx_type j = 0; j < n; j++)
    {
      double d = 0.;
      if (U.xcidx (j+1) > U.xcidx (j)
          && U.xridx (U.xcidx (j+1)-1) == j)
        d = std::abs (U.xdata (U.xcidx (j+1)-1));

      if (xisnan (minU) || d < minU)
        minU = d;

      if (xisnan (maxU) || d > maxU)
        maxU = d;
    }

  double rcond = (minU / maxU);
  volatile double rcond_plus_one = rcond + 1.0;

  if (rcond_plus_one == 1.0 || xisnan (rcond))
    warn_convergence ();

  return true;
}

static bool
LuAminusSigmaB (const Matrix &m, const Matrix &b,
                bool cholB, const ColumnVector& permB, double sigma,
                Matrix &L, Matrix &U, octave_idx_type *P,
                octave_idx_type *Q)
{
  bool have_b = ! b.is_empty ();
  octave_idx_type n = m.cols ();

  // Caclulate LU decomposition of 'A - sigma * B'
  Matrix AminusSigmaB (m);

  if (have_b)
    {
      if (cholB)
        {
          Matrix tmp = sigma * b.transpose () * b;
          const double *pB = permB.fortran_vec ();
          double *p = AminusSigmaB.fortran_vec ();

          if (permB.length ())
            {
              for (octave_idx_type j = 0;
                   j < b.cols (); j++)
                for (octave_idx_type i = 0;
                     i < b.rows (); i++)
                  *p++ -= tmp.xelem (static_cast<octave_idx_type>(pB[i]),
                                     static_cast<octave_idx_type>(pB[j]));
            }
          else
            AminusSigmaB = AminusSigmaB - tmp;
        }
      else
        AminusSigmaB = AminusSigmaB - sigma * b;
    }
  else
    {
      double *p = AminusSigmaB.fortran_vec ();

      for (octave_idx_type i = 0; i < n; i++)
        p[i*(n+1)] -= sigma;
    }

  LU fact (AminusSigmaB);

  L = fact.P ().transpose () * fact.L ();
  U = fact.U ();
  for (octave_idx_type j = 0; j < n; j++)
    P[j] = Q[j] = j;

  // Test condition number of LU decomposition
  double minU = octave_NaN;
  double maxU = octave_NaN;
  for (octave_idx_type j = 0; j < n; j++)
    {
      double d = std::abs (U.xelem (j,j));
      if (xisnan (minU) || d < minU)
        minU = d;

      if (xisnan (maxU) || d > maxU)
        maxU = d;
    }

  double rcond = (minU / maxU);
  volatile double rcond_plus_one = rcond + 1.0;

  if (rcond_plus_one == 1.0 || xisnan (rcond))
    warn_convergence ();

  return true;
}

static bool
LuAminusSigmaB (const SparseComplexMatrix &m, const SparseComplexMatrix &b,
                bool cholB, const ColumnVector& permB, Complex sigma,
                SparseComplexMatrix &L, SparseComplexMatrix &U,
                octave_idx_type *P, octave_idx_type *Q)
{
  bool have_b = ! b.is_empty ();
  octave_idx_type n = m.rows ();

  // Caclulate LU decomposition of 'A - sigma * B'
  SparseComplexMatrix AminusSigmaB (m);

  if (have_b)
    {
      if (cholB)
        {
          if (permB.length ())
            {
              SparseMatrix tmp(n,n,n);
              for (octave_idx_type i = 0; i < n; i++)
                {
                  tmp.xcidx (i) = i;
                  tmp.xridx (i) =
                    static_cast<octave_idx_type>(permB(i));
                  tmp.xdata (i) = 1;
                }
              tmp.xcidx (n) = n;

              AminusSigmaB = AminusSigmaB - tmp * b.hermitian () * b *
                             tmp.transpose () * sigma;
            }
          else
            AminusSigmaB = AminusSigmaB - sigma * b.hermitian () * b;
        }
      else
        AminusSigmaB = AminusSigmaB - sigma * b;
    }
  else
    {
      SparseComplexMatrix sigmat (n, n, n);

      // Create sigma * speye (n,n)
      sigmat.xcidx (0) = 0;
      for (octave_idx_type i = 0; i < n; i++)
        {
          sigmat.xdata (i) = sigma;
          sigmat.xridx (i) = i;
          sigmat.xcidx (i+1) = i + 1;
        }

      AminusSigmaB = AminusSigmaB - sigmat;
    }

  SparseComplexLU fact (AminusSigmaB);

  L = fact.L ();
  U = fact.U ();
  const octave_idx_type *P2 = fact.row_perm ();
  const octave_idx_type *Q2 = fact.col_perm ();

  for (octave_idx_type j = 0; j < n; j++)
    {
      P[j] = P2[j];
      Q[j] = Q2[j];
    }

  // Test condition number of LU decomposition
  double minU = octave_NaN;
  double maxU = octave_NaN;
  for (octave_idx_type j = 0; j < n; j++)
    {
      double d = 0.;
      if (U.xcidx (j+1) > U.xcidx (j)
          && U.xridx (U.xcidx (j+1)-1) == j)
        d = std::abs (U.xdata (U.xcidx (j+1)-1));

      if (xisnan (minU) || d < minU)
        minU = d;

      if (xisnan (maxU) || d > maxU)
        maxU = d;
    }

  double rcond = (minU / maxU);
  volatile double rcond_plus_one = rcond + 1.0;

  if (rcond_plus_one == 1.0 || xisnan (rcond))
    warn_convergence ();

  return true;
}

static bool
LuAminusSigmaB (const ComplexMatrix &m, const ComplexMatrix &b,
                bool cholB, const ColumnVector& permB, Complex sigma,
                ComplexMatrix &L, ComplexMatrix &U, octave_idx_type *P,
                octave_idx_type *Q)
{
  bool have_b = ! b.is_empty ();
  octave_idx_type n = m.cols ();

  // Caclulate LU decomposition of 'A - sigma * B'
  ComplexMatrix AminusSigmaB (m);

  if (have_b)
    {
      if (cholB)
        {
          ComplexMatrix tmp = sigma * b.hermitian () * b;
          const double *pB = permB.fortran_vec ();
          Complex *p = AminusSigmaB.fortran_vec ();

          if (permB.length ())
            {
              for (octave_idx_type j = 0;
                   j < b.cols (); j++)
                for (octave_idx_type i = 0;
                     i < b.rows (); i++)
                  *p++ -= tmp.xelem (static_cast<octave_idx_type>(pB[i]),
                                     static_cast<octave_idx_type>(pB[j]));
            }
          else
            AminusSigmaB = AminusSigmaB - tmp;
        }
      else
        AminusSigmaB = AminusSigmaB - sigma * b;
    }
  else
    {
      Complex *p = AminusSigmaB.fortran_vec ();

      for (octave_idx_type i = 0; i < n; i++)
        p[i*(n+1)] -= sigma;
    }

  ComplexLU fact (AminusSigmaB);

  L = fact.P ().transpose () * fact.L ();
  U = fact.U ();
  for (octave_idx_type j = 0; j < n; j++)
    P[j] = Q[j] = j;

  // Test condition number of LU decomposition
  double minU = octave_NaN;
  double maxU = octave_NaN;
  for (octave_idx_type j = 0; j < n; j++)
    {
      double d = std::abs (U.xelem (j,j));
      if (xisnan (minU) || d < minU)
        minU = d;

      if (xisnan (maxU) || d > maxU)
        maxU = d;
    }

  double rcond = (minU / maxU);
  volatile double rcond_plus_one = rcond + 1.0;

  if (rcond_plus_one == 1.0 || xisnan (rcond))
    warn_convergence ();

  return true;
}

template <class M>
octave_idx_type
EigsRealSymmetricMatrix (const M& m, const std::string typ,
                         octave_idx_type k, octave_idx_type p,
                         octave_idx_type &info, Matrix &eig_vec,
                         ColumnVector &eig_val, const M& _b,
                         ColumnVector &permB, ColumnVector &resid,
                         std::ostream& os, double tol, bool rvec,
                         bool cholB, int disp, int maxit)
{
  M b(_b);
  octave_idx_type n = m.cols ();
  octave_idx_type mode = 1;
  bool have_b = ! b.is_empty ();
  bool note3 = false;
  char bmat = 'I';
  double sigma = 0.;
  M bt;

  if (m.rows () != m.cols ())
    {
      (*current_liboctave_error_handler) ("eigs: A must be square");
      return -1;
    }
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    {
      (*current_liboctave_error_handler)
        ("eigs: B must be square and the same size as A");
      return -1;
    }

  if (resid.is_empty ())
    {
      std::string rand_dist = octave_rand::distribution ();
      octave_rand::distribution ("uniform");
      resid = ColumnVector (octave_rand::vector (n));
      octave_rand::distribution (rand_dist);
    }

  if (n < 3)
    {
      (*current_liboctave_error_handler)
        ("eigs: n must be at least 3");
      return -1;
    }

  if (p < 0)
    {
      p = k * 2;

      if (p < 20)
        p = 20;

      if (p > n - 1)
        p = n - 1 ;
    }

  if (k < 1 || k > n - 2)
    {
      (*current_liboctave_error_handler)
        ("eigs: Invalid number of eigenvalues to extract (must be 0 < k < n-1-1).\n"
         "      Use 'eig (full (A))' instead");
      return -1;
    }

  if (p <= k || p >= n)
    {
      (*current_liboctave_error_handler)
        ("eigs: opts.p must be greater than k and less than n");
      return -1;
    }

  if (have_b && cholB && permB.length () != 0)
    {
      // Check the we really have a permutation vector
      if (permB.length () != n)
        {
          (*current_liboctave_error_handler)
            ("eigs: permB vector invalid");
          return -1;
        }
      else
        {
          Array<bool> checked (dim_vector (n, 1), false);
          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_idx_type bidx =
                static_cast<octave_idx_type> (permB(i));
              if (checked(bidx) || bidx < 0 || bidx >= n
                  || D_NINT (bidx) != bidx)
                {
                  (*current_liboctave_error_handler)
                    ("eigs: permB vector invalid");
                  return -1;
                }
            }
        }
    }

  if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
      && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
      && typ != "SI")
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecognized sigma value");
      return -1;
    }

  if (typ == "LI" || typ == "SI" || typ == "LR" || typ == "SR")
    {
      (*current_liboctave_error_handler)
        ("eigs: invalid sigma value for real symmetric problem");
      return -1;
    }

  if (have_b)
    {
      // See Note 3 dsaupd
      note3 = true;
      if (cholB)
        {
          bt = b;
          b = b.transpose ();
          if (permB.length () == 0)
            {
              permB = ColumnVector (n);
              for (octave_idx_type i = 0; i < n; i++)
                permB(i) = i;
            }
        }
      else
        {
          if (! make_cholb (b, bt, permB))
            {
              (*current_liboctave_error_handler)
                ("eigs: The matrix B is not positive definite");
              return -1;
            }
        }
    }

  Array<octave_idx_type> ip (dim_vector (11, 1));
  octave_idx_type *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<octave_idx_type> iptr (dim_vector (14, 1));
  octave_idx_type *ipntr = iptr.fortran_vec ();

  octave_idx_type ido = 0;
  int iter = 0;
  octave_idx_type lwork = p * (p + 8);

  OCTAVE_LOCAL_BUFFER (double, v, n * p);
  OCTAVE_LOCAL_BUFFER (double, workl, lwork);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_FUNC (dsaupd, DSAUPD)
        (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
         F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
         k, tol, presid, p, v, n, iparam,
         ipntr, workd, workl, lwork, info
         F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      if (f77_exception_encountered)
        {
          (*current_liboctave_error_handler)
            ("eigs: unrecoverable exception encountered in dsaupd");
          return -1;
        }

      if (disp > 0 && !xisnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              for (int i = 0 ; i < k; i++)
                os << "    " << workl[iptr(5)+i-1] << "\n";
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer. But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave_NaN;
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              Matrix mtmp (n,1);
              for (octave_idx_type i = 0; i < n; i++)
                mtmp(i,0) = workd[i + iptr(0) - 1];

              mtmp = utsolve (bt, permB, m * ltsolve (b, permB, mtmp));

              for (octave_idx_type i = 0; i < n; i++)
                workd[i+iptr(1)-1] = mtmp(i,0);
            }
          else if (!vector_product (m, workd + iptr(0) - 1,
                                    workd + iptr(1) - 1))
            break;
        }
      else
        {
          if (info < 0)
            {
              (*current_liboctave_error_handler)
                ("eigs: error %d in dsaupd", info);
              return -1;
            }
          break;
        }
    }
  while (1);

  octave_idx_type info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type. It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent. As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<octave_idx_type> s (dim_vector (p, 1));
  octave_idx_type *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  double *z = eig_vec.fortran_vec ();

  eig_val.resize (k);
  double *d = eig_val.fortran_vec ();

  F77_FUNC (dseupd, DSEUPD)
    (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, d, z, n, sigma,
     F77_CONST_CHAR_ARG2 (&bmat, 1), n,
     F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2), k, tol, presid, p, v, n, iparam,
     ipntr, workd, workl, lwork, info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1)
     F77_CHAR_ARG_LEN(2));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable exception encountered in dseupd");
      return -1;
    }
  else
    {
      if (info2 == 0)
        {
          octave_idx_type k2 = k / 2;
          if (typ != "SM" && typ != "BE")
            {
              for (octave_idx_type i = 0; i < k2; i++)
                {
                  double dtmp = d[i];
                  d[i] = d[k - i - 1];
                  d[k - i - 1] = dtmp;
                }
            }

          if (rvec)
            {
              if (typ != "SM" && typ != "BE")
                {
                  OCTAVE_LOCAL_BUFFER (double, dtmp, n);

                  for (octave_idx_type i = 0; i < k2; i++)
                    {
                      octave_idx_type off1 = i * n;
                      octave_idx_type off2 = (k - i - 1) * n;

                      if (off1 == off2)
                        continue;

                      for (octave_idx_type j = 0; j < n; j++)
                        dtmp[j] = z[off1 + j];

                      for (octave_idx_type j = 0; j < n; j++)
                        z[off1 + j] = z[off2 + j];

                      for (octave_idx_type j = 0; j < n; j++)
                        z[off2 + j] = dtmp[j];
                    }
                }

              if (note3)
                eig_vec = ltsolve (b, permB, eig_vec);
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("eigs: error %d in dseupd", info2);
          return -1;
        }
    }

  return ip(4);
}

template <class M>
octave_idx_type
EigsRealSymmetricMatrixShift (const M& m, double sigma,
                              octave_idx_type k, octave_idx_type p,
                              octave_idx_type &info, Matrix &eig_vec,
                              ColumnVector &eig_val, const M& _b,
                              ColumnVector &permB, ColumnVector &resid,
                              std::ostream& os, double tol, bool rvec,
                              bool cholB, int disp, int maxit)
{
  M b(_b);
  octave_idx_type n = m.cols ();
  octave_idx_type mode = 3;
  bool have_b = ! b.is_empty ();
  std::string typ = "LM";

  if (m.rows () != m.cols ())
    {
      (*current_liboctave_error_handler) ("eigs: A must be square");
      return -1;
    }
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    {
      (*current_liboctave_error_handler)
        ("eigs: B must be square and the same size as A");
      return -1;
    }

  // FIXME: The "SM" type for mode 1 seems unstable though faster!!
  //if (! std::abs (sigma))
  //  return EigsRealSymmetricMatrix (m, "SM", k, p, info, eig_vec, eig_val,
  //                                _b, permB, resid, os, tol, rvec, cholB,
  //                                disp, maxit);

  if (resid.is_empty ())
    {
      std::string rand_dist = octave_rand::distribution ();
      octave_rand::distribution ("uniform");
      resid = ColumnVector (octave_rand::vector (n));
      octave_rand::distribution (rand_dist);
    }

  if (n < 3)
    {
      (*current_liboctave_error_handler)
        ("eigs: n must be at least 3");
      return -1;
    }

  if (k <= 0 || k >= n - 1)
    {
      (*current_liboctave_error_handler)
        ("eigs: Invalid number of eigenvalues to extract (must be 0 < k < n-1-1).\n"
         "      Use 'eig (full (A))' instead");
      return -1;
    }

  if (p < 0)
    {
      p = k * 2;

      if (p < 20)
        p = 20;

      if (p > n - 1)
        p = n - 1 ;
    }

  if (p <= k || p >= n)
    {
      (*current_liboctave_error_handler)
        ("eigs: opts.p must be greater than k and less than n");
      return -1;
    }

  if (have_b && cholB && permB.length () != 0)
    {
      // Check the we really have a permutation vector
      if (permB.length () != n)
        {
          (*current_liboctave_error_handler) ("eigs: permB vector invalid");
          return -1;
        }
      else
        {
          Array<bool> checked (dim_vector (n, 1), false);
          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_idx_type bidx =
                static_cast<octave_idx_type> (permB(i));
              if (checked(bidx) || bidx < 0 || bidx >= n
                  || D_NINT (bidx) != bidx)
                {
                  (*current_liboctave_error_handler)
                    ("eigs: permB vector invalid");
                  return -1;
                }
            }
        }
    }

  char bmat = 'I';
  if (have_b)
    bmat = 'G';

  Array<octave_idx_type> ip (dim_vector (11, 1));
  octave_idx_type *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<octave_idx_type> iptr (dim_vector (14, 1));
  octave_idx_type *ipntr = iptr.fortran_vec ();

  octave_idx_type ido = 0;
  int iter = 0;
  M L, U;

  OCTAVE_LOCAL_BUFFER (octave_idx_type, P, (have_b ? b.rows () : m.rows ()));
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Q, (have_b ? b.cols () : m.cols ()));

  if (! LuAminusSigmaB (m, b, cholB, permB, sigma, L, U, P, Q))
    return -1;

  octave_idx_type lwork = p * (p + 8);

  OCTAVE_LOCAL_BUFFER (double, v, n * p);
  OCTAVE_LOCAL_BUFFER (double, workl, lwork);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_FUNC (dsaupd, DSAUPD)
        (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
         F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
         k, tol, presid, p, v, n, iparam,
         ipntr, workd, workl, lwork, info
         F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      if (f77_exception_encountered)
        {
          (*current_liboctave_error_handler)
            ("eigs: unrecoverable exception encountered in dsaupd");
          return -1;
        }

      if (disp > 0 && !xisnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              for (int i = 0 ; i < k; i++)
                os << "    " << workl[iptr(5)+i-1] << "\n";
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer. But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave_NaN;
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              if (ido == -1)
                {
                  OCTAVE_LOCAL_BUFFER (double, dtmp, n);

                  vector_product (m, workd+iptr(0)-1, dtmp);

                  Matrix tmp(n, 1);

                  for (octave_idx_type i = 0; i < n; i++)
                    tmp(i,0) = dtmp[P[i]];

                  lusolve (L, U, tmp);

                  double *ip2 = workd+iptr(1)-1;
                  for (octave_idx_type i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i,0);
                }
              else if (ido == 2)
                vector_product (b, workd+iptr(0)-1, workd+iptr(1)-1);
              else
                {
                  double *ip2 = workd+iptr(2)-1;
                  Matrix tmp(n, 1);

                  for (octave_idx_type i = 0; i < n; i++)
                    tmp(i,0) = ip2[P[i]];

                  lusolve (L, U, tmp);

                  ip2 = workd+iptr(1)-1;
                  for (octave_idx_type i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i,0);
                }
            }
          else
            {
              if (ido == 2)
                {
                  for (octave_idx_type i = 0; i < n; i++)
                    workd[iptr(0) + i - 1] = workd[iptr(1) + i - 1];
                }
              else
                {
                  double *ip2 = workd+iptr(0)-1;
                  Matrix tmp(n, 1);

                  for (octave_idx_type i = 0; i < n; i++)
                    tmp(i,0) = ip2[P[i]];

                  lusolve (L, U, tmp);

                  ip2 = workd+iptr(1)-1;
                  for (octave_idx_type i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i,0);
                }
            }
        }
      else
        {
          if (info < 0)
            {
              (*current_liboctave_error_handler)
                ("eigs: error %d in dsaupd", info);
              return -1;
            }
          break;
        }
    }
  while (1);

  octave_idx_type info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type. It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent. As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<octave_idx_type> s (dim_vector (p, 1));
  octave_idx_type *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  double *z = eig_vec.fortran_vec ();

  eig_val.resize (k);
  double *d = eig_val.fortran_vec ();

  F77_FUNC (dseupd, DSEUPD)
    (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, d, z, n, sigma,
     F77_CONST_CHAR_ARG2 (&bmat, 1), n,
     F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
     k, tol, presid, p, v, n, iparam, ipntr, workd, workl, lwork, info2
     F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable exception encountered in dseupd");
      return -1;
    }
  else
    {
      if (info2 == 0)
        {
          octave_idx_type k2 = k / 2;
          for (octave_idx_type i = 0; i < k2; i++)
            {
              double dtmp = d[i];
              d[i] = d[k - i - 1];
              d[k - i - 1] = dtmp;
            }

          if (rvec)
            {
              OCTAVE_LOCAL_BUFFER (double, dtmp, n);

              for (octave_idx_type i = 0; i < k2; i++)
                {
                  octave_idx_type off1 = i * n;
                  octave_idx_type off2 = (k - i - 1) * n;

                  if (off1 == off2)
                    continue;

                  for (octave_idx_type j = 0; j < n; j++)
                    dtmp[j] = z[off1 + j];

                  for (octave_idx_type j = 0; j < n; j++)
                    z[off1 + j] = z[off2 + j];

                  for (octave_idx_type j = 0; j < n; j++)
                    z[off2 + j] = dtmp[j];
                }
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("eigs: error %d in dseupd", info2);
          return -1;
        }
    }

  return ip(4);
}

octave_idx_type
EigsRealSymmetricFunc (EigsFunc fun, octave_idx_type n,
                       const std::string &_typ, double sigma,
                       octave_idx_type k, octave_idx_type p,
                       octave_idx_type &info, Matrix &eig_vec,
                       ColumnVector &eig_val, ColumnVector &resid,
                       std::ostream& os, double tol, bool rvec,
                       bool /* cholB */, int disp, int maxit)
{
  std::string typ (_typ);
  bool have_sigma = (sigma ? true : false);
  char bmat = 'I';
  octave_idx_type mode = 1;
  int err = 0;

  if (resid.is_empty ())
    {
      std::string rand_dist = octave_rand::distribution ();
      octave_rand::distribution ("uniform");
      resid = ColumnVector (octave_rand::vector (n));
      octave_rand::distribution (rand_dist);
    }

  if (n < 3)
    {
      (*current_liboctave_error_handler)
        ("eigs: n must be at least 3");
      return -1;
    }

  if (p < 0)
    {
      p = k * 2;

      if (p < 20)
        p = 20;

      if (p > n - 1)
        p = n - 1 ;
    }

  if (k <= 0 || k >= n - 1)
    {
      (*current_liboctave_error_handler)
        ("eigs: Invalid number of eigenvalues to extract (must be 0 < k < n-1).\n"
         "      Use 'eig (full (A))' instead");
      return -1;
    }

  if (p <= k || p >= n)
    {
      (*current_liboctave_error_handler)
        ("eigs: opts.p must be greater than k and less than n");
      return -1;
    }

  if (! have_sigma)
    {
      if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
          && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
          && typ != "SI")
        (*current_liboctave_error_handler)
          ("eigs: unrecognized sigma value");

      if (typ == "LI" || typ == "SI" || typ == "LR" || typ == "SR")
        {
          (*current_liboctave_error_handler)
            ("eigs: invalid sigma value for real symmetric problem");
          return -1;
        }

      if (typ == "SM")
        {
          typ = "LM";
          sigma = 0.;
          mode = 3;
        }
    }
  else if (! std::abs (sigma))
    typ = "SM";
  else
    {
      typ = "LM";
      mode = 3;
    }

  Array<octave_idx_type> ip (dim_vector (11, 1));
  octave_idx_type *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<octave_idx_type> iptr (dim_vector (14, 1));
  octave_idx_type *ipntr = iptr.fortran_vec ();

  octave_idx_type ido = 0;
  int iter = 0;
  octave_idx_type lwork = p * (p + 8);

  OCTAVE_LOCAL_BUFFER (double, v, n * p);
  OCTAVE_LOCAL_BUFFER (double, workl, lwork);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_FUNC (dsaupd, DSAUPD)
        (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
         F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
         k, tol, presid, p, v, n, iparam,
         ipntr, workd, workl, lwork, info
         F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      if (f77_exception_encountered)
        {
          (*current_liboctave_error_handler)
            ("eigs: unrecoverable exception encountered in dsaupd");
          return -1;
        }

      if (disp > 0 && !xisnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              for (int i = 0 ; i < k; i++)
                os << "    " << workl[iptr(5)+i-1] << "\n";
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer. But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave_NaN;
        }


      if (ido == -1 || ido == 1 || ido == 2)
        {
          double *ip2 = workd + iptr(0) - 1;
          ColumnVector x(n);

          for (octave_idx_type i = 0; i < n; i++)
            x(i) = *ip2++;

          ColumnVector y = fun (x, err);

          if (err)
            return false;

          ip2 = workd + iptr(1) - 1;
          for (octave_idx_type i = 0; i < n; i++)
            *ip2++ = y(i);
        }
      else
        {
          if (info < 0)
            {
              (*current_liboctave_error_handler)
                ("eigs: error %d in dsaupd", info);
              return -1;
            }
          break;
        }
    }
  while (1);

  octave_idx_type info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type. It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent. As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<octave_idx_type> s (dim_vector (p, 1));
  octave_idx_type *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  double *z = eig_vec.fortran_vec ();

  eig_val.resize (k);
  double *d = eig_val.fortran_vec ();

  F77_FUNC (dseupd, DSEUPD)
    (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, d, z, n, sigma,
     F77_CONST_CHAR_ARG2 (&bmat, 1), n,
     F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
     k, tol, presid, p, v, n, iparam, ipntr, workd, workl, lwork, info2
     F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable exception encountered in dseupd");
      return -1;
    }
  else
    {
      if (info2 == 0)
        {
          octave_idx_type k2 = k / 2;
          if (typ != "SM" && typ != "BE")
            {
              for (octave_idx_type i = 0; i < k2; i++)
                {
                  double dtmp = d[i];
                  d[i] = d[k - i - 1];
                  d[k - i - 1] = dtmp;
                }
            }

          if (rvec)
            {
              if (typ != "SM" && typ != "BE")
                {
                  OCTAVE_LOCAL_BUFFER (double, dtmp, n);

                  for (octave_idx_type i = 0; i < k2; i++)
                    {
                      octave_idx_type off1 = i * n;
                      octave_idx_type off2 = (k - i - 1) * n;

                      if (off1 == off2)
                        continue;

                      for (octave_idx_type j = 0; j < n; j++)
                        dtmp[j] = z[off1 + j];

                      for (octave_idx_type j = 0; j < n; j++)
                        z[off1 + j] = z[off2 + j];

                      for (octave_idx_type j = 0; j < n; j++)
                        z[off2 + j] = dtmp[j];
                    }
                }
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("eigs: error %d in dseupd", info2);
          return -1;
        }
    }

  return ip(4);
}

template <class M>
octave_idx_type
EigsRealNonSymmetricMatrix (const M& m, const std::string typ,
                            octave_idx_type k, octave_idx_type p,
                            octave_idx_type &info, ComplexMatrix &eig_vec,
                            ComplexColumnVector &eig_val, const M& _b,
                            ColumnVector &permB, ColumnVector &resid,
                            std::ostream& os, double tol, bool rvec,
                            bool cholB, int disp, int maxit)
{
  M b(_b);
  octave_idx_type n = m.cols ();
  octave_idx_type mode = 1;
  bool have_b = ! b.is_empty ();
  bool note3 = false;
  char bmat = 'I';
  double sigmar = 0.;
  double sigmai = 0.;
  M bt;

  if (m.rows () != m.cols ())
    {
      (*current_liboctave_error_handler) ("eigs: A must be square");
      return -1;
    }
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    {
      (*current_liboctave_error_handler)
        ("eigs: B must be square and the same size as A");
      return -1;
    }

  if (resid.is_empty ())
    {
      std::string rand_dist = octave_rand::distribution ();
      octave_rand::distribution ("uniform");
      resid = ColumnVector (octave_rand::vector (n));
      octave_rand::distribution (rand_dist);
    }

  if (n < 3)
    {
      (*current_liboctave_error_handler)
        ("eigs: n must be at least 3");
      return -1;
    }

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n - 1)
        p = n - 1 ;
    }

  if (k <= 0 || k >= n - 1)
    {
      (*current_liboctave_error_handler)
        ("eigs: Invalid number of eigenvalues to extract (must be 0 < k < n-1).\n"
         "      Use 'eig (full (A))' instead");
      return -1;
    }

  if (p <= k || p >= n)
    {
      (*current_liboctave_error_handler)
        ("eigs: opts.p must be greater than k and less than n");
      return -1;
    }

  if (have_b && cholB && permB.length () != 0)
    {
      // Check the we really have a permutation vector
      if (permB.length () != n)
        {
          (*current_liboctave_error_handler)
            ("eigs: permB vector invalid");
          return -1;
        }
      else
        {
          Array<bool> checked (dim_vector (n, 1), false);
          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_idx_type bidx =
                static_cast<octave_idx_type> (permB(i));
              if (checked(bidx) || bidx < 0 || bidx >= n
                  || D_NINT (bidx) != bidx)
                {
                  (*current_liboctave_error_handler)
                    ("eigs: permB vector invalid");
                  return -1;
                }
            }
        }
    }

  if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
      && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
      && typ != "SI")
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecognized sigma value");
      return -1;
    }

  if (typ == "LA" || typ == "SA" || typ == "BE")
    {
      (*current_liboctave_error_handler)
        ("eigs: invalid sigma value for unsymmetric problem");
      return -1;
    }

  if (have_b)
    {
      // See Note 3 dsaupd
      note3 = true;
      if (cholB)
        {
          bt = b;
          b = b.transpose ();
          if (permB.length () == 0)
            {
              permB = ColumnVector (n);
              for (octave_idx_type i = 0; i < n; i++)
                permB(i) = i;
            }
        }
      else
        {
          if (! make_cholb (b, bt, permB))
            {
              (*current_liboctave_error_handler)
                ("eigs: The matrix B is not positive definite");
              return -1;
            }
        }
    }

  Array<octave_idx_type> ip (dim_vector (11, 1));
  octave_idx_type *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<octave_idx_type> iptr (dim_vector (14, 1));
  octave_idx_type *ipntr = iptr.fortran_vec ();

  octave_idx_type ido = 0;
  int iter = 0;
  octave_idx_type lwork = 3 * p * (p + 2);

  OCTAVE_LOCAL_BUFFER (double, v, n * (p + 1));
  OCTAVE_LOCAL_BUFFER (double, workl, lwork + 1);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n + 1);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_FUNC (dnaupd, DNAUPD)
        (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
         F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
         k, tol, presid, p, v, n, iparam,
         ipntr, workd, workl, lwork, info
         F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      if (f77_exception_encountered)
        {
          (*current_liboctave_error_handler)
            ("eigs: unrecoverable exception encountered in dnaupd");
          return -1;
        }

      if (disp > 0 && !xisnan(workl[iptr(5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              for (int i = 0 ; i < k; i++)
                os << "    " << workl[iptr(5)+i-1] << "\n";
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer. But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave_NaN;
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              Matrix mtmp (n,1);
              for (octave_idx_type i = 0; i < n; i++)
                mtmp(i,0) = workd[i + iptr(0) - 1];

              mtmp = utsolve (bt, permB, m * ltsolve (b, permB, mtmp));

              for (octave_idx_type i = 0; i < n; i++)
                workd[i+iptr(1)-1] = mtmp(i,0);
            }
          else if (!vector_product (m, workd + iptr(0) - 1,
                                    workd + iptr(1) - 1))
            break;
        }
      else
        {
          if (info < 0)
            {
              (*current_liboctave_error_handler)
                ("eigs: error %d in dnaupd", info);
              return -1;
            }
          break;
        }
    }
  while (1);

  octave_idx_type info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type. It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent. As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<octave_idx_type> s (dim_vector (p, 1));
  octave_idx_type *sel = s.fortran_vec ();

  // FIXME: initialize eig_vec2 to zero; apparently dneupd can skip
  // the assignment to elements of Z that represent imaginary parts.
  // Found with valgrind and
  //
  //   A = [1,0,0,-1;0,1,0,0;0,0,1,0;0,0,2,1];
  //   [vecs, vals, f] = eigs (A, 1)

  Matrix eig_vec2 (n, k + 1, 0.0);
  double *z = eig_vec2.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, dr, k + 1);
  OCTAVE_LOCAL_BUFFER (double, di, k + 1);
  OCTAVE_LOCAL_BUFFER (double, workev, 3 * p);
  for (octave_idx_type i = 0; i < k+1; i++)
    dr[i] = di[i] = 0.;

  F77_FUNC (dneupd, DNEUPD)
    (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, dr, di, z, n, sigmar,
     sigmai, workev,  F77_CONST_CHAR_ARG2 (&bmat, 1), n,
     F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2), k, tol, presid, p, v, n, iparam,
     ipntr, workd, workl, lwork, info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1)
     F77_CHAR_ARG_LEN(2));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable exception encountered in dneupd");
      return -1;
    }
  else
    {
      eig_val.resize (k+1);
      Complex *d = eig_val.fortran_vec ();

      if (info2 == 0)
        {
          octave_idx_type jj = 0;
          for (octave_idx_type i = 0; i < k+1; i++)
            {
              if (dr[i] == 0.0 && di[i] == 0.0 && jj == 0)
                jj++;
              else
                d[i-jj] = Complex (dr[i], di[i]);
            }
          if (jj == 0 && !rvec)
            for (octave_idx_type i = 0; i < k; i++)
              d[i] = d[i+1];

          octave_idx_type k2 = k / 2;
          for (octave_idx_type i = 0; i < k2; i++)
            {
              Complex dtmp = d[i];
              d[i] = d[k - i - 1];
              d[k - i - 1] = dtmp;
            }
          eig_val.resize (k);

          if (rvec)
            {
              OCTAVE_LOCAL_BUFFER (double, dtmp, n);

              for (octave_idx_type i = 0; i < k2; i++)
                {
                  octave_idx_type off1 = i * n;
                  octave_idx_type off2 = (k - i - 1) * n;

                  if (off1 == off2)
                    continue;

                  for (octave_idx_type j = 0; j < n; j++)
                    dtmp[j] = z[off1 + j];

                  for (octave_idx_type j = 0; j < n; j++)
                    z[off1 + j] = z[off2 + j];

                  for (octave_idx_type j = 0; j < n; j++)
                    z[off2 + j] = dtmp[j];
                }

              eig_vec.resize (n, k);
              octave_idx_type i = 0;
              while (i < k)
                {
                  octave_idx_type off1 = i * n;
                  octave_idx_type off2 = (i+1) * n;
                  if (std::imag (eig_val(i)) == 0)
                    {
                      for (octave_idx_type j = 0; j < n; j++)
                        eig_vec(j,i) =
                          Complex (z[j+off1],0.);
                      i++;
                    }
                  else
                    {
                      for (octave_idx_type j = 0; j < n; j++)
                        {
                          eig_vec(j,i) =
                            Complex (z[j+off1],z[j+off2]);
                          if (i < k - 1)
                            eig_vec(j,i+1) =
                              Complex (z[j+off1],-z[j+off2]);
                        }
                      i+=2;
                    }
                }

              if (note3)
                eig_vec = ltsolve (M(b), permB, eig_vec);
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("eigs: error %d in dneupd", info2);
          return -1;
        }
    }

  return ip(4);
}

template <class M>
octave_idx_type
EigsRealNonSymmetricMatrixShift (const M& m, double sigmar,
                                 octave_idx_type k, octave_idx_type p,
                                 octave_idx_type &info,
                                 ComplexMatrix &eig_vec,
                                 ComplexColumnVector &eig_val, const M& _b,
                                 ColumnVector &permB, ColumnVector &resid,
                                 std::ostream& os, double tol, bool rvec,
                                 bool cholB, int disp, int maxit)
{
  M b(_b);
  octave_idx_type n = m.cols ();
  octave_idx_type mode = 3;
  bool have_b = ! b.is_empty ();
  std::string typ = "LM";
  double sigmai = 0.;

  if (m.rows () != m.cols ())
    {
      (*current_liboctave_error_handler) ("eigs: A must be square");
      return -1;
    }
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    {
      (*current_liboctave_error_handler)
        ("eigs: B must be square and the same size as A");
      return -1;
    }

  // FIXME: The "SM" type for mode 1 seems unstable though faster!!
  //if (! std::abs (sigmar))
  //  return EigsRealNonSymmetricMatrix (m, "SM", k, p, info, eig_vec, eig_val,
  //                                   _b, permB, resid, os, tol, rvec, cholB,
  //                                   disp, maxit);

  if (resid.is_empty ())
    {
      std::string rand_dist = octave_rand::distribution ();
      octave_rand::distribution ("uniform");
      resid = ColumnVector (octave_rand::vector (n));
      octave_rand::distribution (rand_dist);
    }

  if (n < 3)
    {
      (*current_liboctave_error_handler)
        ("eigs: n must be at least 3");
      return -1;
    }

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n - 1)
        p = n - 1 ;
    }

  if (k <= 0 || k >= n - 1)
    {
      (*current_liboctave_error_handler)
        ("eigs: Invalid number of eigenvalues to extract (must be 0 < k < n-1).\n"
         "      Use 'eig (full (A))' instead");
      return -1;
    }

  if (p <= k || p >= n)
    {
      (*current_liboctave_error_handler)
        ("eigs: opts.p must be greater than k and less than n");
      return -1;
    }

  if (have_b && cholB && permB.length () != 0)
    {
      // Check that we really have a permutation vector
      if (permB.length () != n)
        {
          (*current_liboctave_error_handler) ("eigs: permB vector invalid");
          return -1;
        }
      else
        {
          Array<bool> checked (dim_vector (n, 1), false);
          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_idx_type bidx =
                static_cast<octave_idx_type> (permB(i));
              if (checked(bidx) || bidx < 0 || bidx >= n
                  || D_NINT (bidx) != bidx)
                {
                  (*current_liboctave_error_handler)
                    ("eigs: permB vector invalid");
                  return -1;
                }
            }
        }
    }

  char bmat = 'I';
  if (have_b)
    bmat = 'G';

  Array<octave_idx_type> ip (dim_vector (11, 1));
  octave_idx_type *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<octave_idx_type> iptr (dim_vector (14, 1));
  octave_idx_type *ipntr = iptr.fortran_vec ();

  octave_idx_type ido = 0;
  int iter = 0;
  M L, U;

  OCTAVE_LOCAL_BUFFER (octave_idx_type, P, (have_b ? b.rows () : m.rows ()));
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Q, (have_b ? b.cols () : m.cols ()));

  if (! LuAminusSigmaB (m, b, cholB, permB, sigmar, L, U, P, Q))
    return -1;

  octave_idx_type lwork = 3 * p * (p + 2);

  OCTAVE_LOCAL_BUFFER (double, v, n * (p + 1));
  OCTAVE_LOCAL_BUFFER (double, workl, lwork + 1);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n + 1);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_FUNC (dnaupd, DNAUPD)
        (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
         F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
         k, tol, presid, p, v, n, iparam,
         ipntr, workd, workl, lwork, info
         F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      if (f77_exception_encountered)
        {
          (*current_liboctave_error_handler)
            ("eigs: unrecoverable exception encountered in dsaupd");
          return -1;
        }

      if (disp > 0 && !xisnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              for (int i = 0 ; i < k; i++)
                os << "    " << workl[iptr(5)+i-1] << "\n";
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer. But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave_NaN;
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              if (ido == -1)
                {
                  OCTAVE_LOCAL_BUFFER (double, dtmp, n);

                  vector_product (m, workd+iptr(0)-1, dtmp);

                  Matrix tmp(n, 1);

                  for (octave_idx_type i = 0; i < n; i++)
                    tmp(i,0) = dtmp[P[i]];

                  lusolve (L, U, tmp);

                  double *ip2 = workd+iptr(1)-1;
                  for (octave_idx_type i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i,0);
                }
              else if (ido == 2)
                vector_product (b, workd+iptr(0)-1, workd+iptr(1)-1);
              else
                {
                  double *ip2 = workd+iptr(2)-1;
                  Matrix tmp(n, 1);

                  for (octave_idx_type i = 0; i < n; i++)
                    tmp(i,0) = ip2[P[i]];

                  lusolve (L, U, tmp);

                  ip2 = workd+iptr(1)-1;
                  for (octave_idx_type i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i,0);
                }
            }
          else
            {
              if (ido == 2)
                {
                  for (octave_idx_type i = 0; i < n; i++)
                    workd[iptr(0) + i - 1] = workd[iptr(1) + i - 1];
                }
              else
                {
                  double *ip2 = workd+iptr(0)-1;
                  Matrix tmp(n, 1);

                  for (octave_idx_type i = 0; i < n; i++)
                    tmp(i,0) = ip2[P[i]];

                  lusolve (L, U, tmp);

                  ip2 = workd+iptr(1)-1;
                  for (octave_idx_type i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i,0);
                }
            }
        }
      else
        {
          if (info < 0)
            {
              (*current_liboctave_error_handler)
                ("eigs: error %d in dsaupd", info);
              return -1;
            }
          break;
        }
    }
  while (1);

  octave_idx_type info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type. It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent. As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<octave_idx_type> s (dim_vector (p, 1));
  octave_idx_type *sel = s.fortran_vec ();

  // FIXME: initialize eig_vec2 to zero; apparently dneupd can skip
  // the assignment to elements of Z that represent imaginary parts.
  // Found with valgrind and
  //
  //   A = [1,0,0,-1;0,1,0,0;0,0,1,0;0,0,2,1];
  //   [vecs, vals, f] = eigs (A, 1)

  Matrix eig_vec2 (n, k + 1, 0.0);
  double *z = eig_vec2.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, dr, k + 1);
  OCTAVE_LOCAL_BUFFER (double, di, k + 1);
  OCTAVE_LOCAL_BUFFER (double, workev, 3 * p);
  for (octave_idx_type i = 0; i < k+1; i++)
    dr[i] = di[i] = 0.;

  F77_FUNC (dneupd, DNEUPD)
    (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, dr, di, z, n, sigmar,
     sigmai, workev,  F77_CONST_CHAR_ARG2 (&bmat, 1), n,
     F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2), k, tol, presid, p, v, n, iparam,
     ipntr, workd, workl, lwork, info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1)
     F77_CHAR_ARG_LEN(2));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable exception encountered in dneupd");
      return -1;
    }
  else
    {
      eig_val.resize (k+1);
      Complex *d = eig_val.fortran_vec ();

      if (info2 == 0)
        {
          octave_idx_type jj = 0;
          for (octave_idx_type i = 0; i < k+1; i++)
            {
              if (dr[i] == 0.0 && di[i] == 0.0 && jj == 0)
                jj++;
              else
                d[i-jj] = Complex (dr[i], di[i]);
            }
          if (jj == 0 && !rvec)
            for (octave_idx_type i = 0; i < k; i++)
              d[i] = d[i+1];

          octave_idx_type k2 = k / 2;
          for (octave_idx_type i = 0; i < k2; i++)
            {
              Complex dtmp = d[i];
              d[i] = d[k - i - 1];
              d[k - i - 1] = dtmp;
            }
          eig_val.resize (k);

          if (rvec)
            {
              OCTAVE_LOCAL_BUFFER (double, dtmp, n);

              for (octave_idx_type i = 0; i < k2; i++)
                {
                  octave_idx_type off1 = i * n;
                  octave_idx_type off2 = (k - i - 1) * n;

                  if (off1 == off2)
                    continue;

                  for (octave_idx_type j = 0; j < n; j++)
                    dtmp[j] = z[off1 + j];

                  for (octave_idx_type j = 0; j < n; j++)
                    z[off1 + j] = z[off2 + j];

                  for (octave_idx_type j = 0; j < n; j++)
                    z[off2 + j] = dtmp[j];
                }

              eig_vec.resize (n, k);
              octave_idx_type i = 0;
              while (i < k)
                {
                  octave_idx_type off1 = i * n;
                  octave_idx_type off2 = (i+1) * n;
                  if (std::imag (eig_val(i)) == 0)
                    {
                      for (octave_idx_type j = 0; j < n; j++)
                        eig_vec(j,i) =
                          Complex (z[j+off1],0.);
                      i++;
                    }
                  else
                    {
                      for (octave_idx_type j = 0; j < n; j++)
                        {
                          eig_vec(j,i) =
                            Complex (z[j+off1],z[j+off2]);
                          if (i < k - 1)
                            eig_vec(j,i+1) =
                              Complex (z[j+off1],-z[j+off2]);
                        }
                      i+=2;
                    }
                }
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("eigs: error %d in dneupd", info2);
          return -1;
        }
    }

  return ip(4);
}

octave_idx_type
EigsRealNonSymmetricFunc (EigsFunc fun, octave_idx_type n,
                          const std::string &_typ, double sigmar,
                          octave_idx_type k, octave_idx_type p,
                          octave_idx_type &info, ComplexMatrix &eig_vec,
                          ComplexColumnVector &eig_val, ColumnVector &resid,
                          std::ostream& os, double tol, bool rvec,
                          bool /* cholB */, int disp, int maxit)
{
  std::string typ (_typ);
  bool have_sigma = (sigmar ? true : false);
  char bmat = 'I';
  double sigmai = 0.;
  octave_idx_type mode = 1;
  int err = 0;

  if (resid.is_empty ())
    {
      std::string rand_dist = octave_rand::distribution ();
      octave_rand::distribution ("uniform");
      resid = ColumnVector (octave_rand::vector (n));
      octave_rand::distribution (rand_dist);
    }

  if (n < 3)
    {
      (*current_liboctave_error_handler)
        ("eigs: n must be at least 3");
      return -1;
    }

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n - 1)
        p = n - 1 ;
    }

  if (k <= 0 || k >= n - 1)
    {
      (*current_liboctave_error_handler)
        ("eigs: Invalid number of eigenvalues to extract (must be 0 < k < n-1).\n"
         "      Use 'eig (full (A))' instead");
      return -1;
    }

  if (p <= k || p >= n)
    {
      (*current_liboctave_error_handler)
        ("eigs: opts.p must be greater than k and less than n");
      return -1;
    }


  if (! have_sigma)
    {
      if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
          && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
          && typ != "SI")
        (*current_liboctave_error_handler)
          ("eigs: unrecognized sigma value");

      if (typ == "LA" || typ == "SA" || typ == "BE")
        {
          (*current_liboctave_error_handler)
            ("eigs: invalid sigma value for unsymmetric problem");
          return -1;
        }

      if (typ == "SM")
        {
          typ = "LM";
          sigmar = 0.;
          mode = 3;
        }
    }
  else if (! std::abs (sigmar))
    typ = "SM";
  else
    {
      typ = "LM";
      mode = 3;
    }

  Array<octave_idx_type> ip (dim_vector (11, 1));
  octave_idx_type *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<octave_idx_type> iptr (dim_vector (14, 1));
  octave_idx_type *ipntr = iptr.fortran_vec ();

  octave_idx_type ido = 0;
  int iter = 0;
  octave_idx_type lwork = 3 * p * (p + 2);

  OCTAVE_LOCAL_BUFFER (double, v, n * (p + 1));
  OCTAVE_LOCAL_BUFFER (double, workl, lwork + 1);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n + 1);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_FUNC (dnaupd, DNAUPD)
        (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
         F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
         k, tol, presid, p, v, n, iparam,
         ipntr, workd, workl, lwork, info
         F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      if (f77_exception_encountered)
        {
          (*current_liboctave_error_handler)
            ("eigs: unrecoverable exception encountered in dnaupd");
          return -1;
        }

      if (disp > 0 && !xisnan(workl[iptr(5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              for (int i = 0 ; i < k; i++)
                os << "    " << workl[iptr(5)+i-1] << "\n";
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer. But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave_NaN;
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          double *ip2 = workd + iptr(0) - 1;
          ColumnVector x(n);

          for (octave_idx_type i = 0; i < n; i++)
            x(i) = *ip2++;

          ColumnVector y = fun (x, err);

          if (err)
            return false;

          ip2 = workd + iptr(1) - 1;
          for (octave_idx_type i = 0; i < n; i++)
            *ip2++ = y(i);
        }
      else
        {
          if (info < 0)
            {
              (*current_liboctave_error_handler)
                ("eigs: error %d in dsaupd", info);
              return -1;
            }
          break;
        }
    }
  while (1);

  octave_idx_type info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type. It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent. As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<octave_idx_type> s (dim_vector (p, 1));
  octave_idx_type *sel = s.fortran_vec ();

  // FIXME: initialize eig_vec2 to zero; apparently dneupd can skip
  // the assignment to elements of Z that represent imaginary parts.
  // Found with valgrind and
  //
  //   A = [1,0,0,-1;0,1,0,0;0,0,1,0;0,0,2,1];
  //   [vecs, vals, f] = eigs (A, 1)

  Matrix eig_vec2 (n, k + 1, 0.0);
  double *z = eig_vec2.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, dr, k + 1);
  OCTAVE_LOCAL_BUFFER (double, di, k + 1);
  OCTAVE_LOCAL_BUFFER (double, workev, 3 * p);
  for (octave_idx_type i = 0; i < k+1; i++)
    dr[i] = di[i] = 0.;

  F77_FUNC (dneupd, DNEUPD)
    (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, dr, di, z, n, sigmar,
     sigmai, workev,  F77_CONST_CHAR_ARG2 (&bmat, 1), n,
     F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2), k, tol, presid, p, v, n, iparam,
     ipntr, workd, workl, lwork, info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1)
     F77_CHAR_ARG_LEN(2));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable exception encountered in dneupd");
      return -1;
    }
  else
    {
      eig_val.resize (k+1);
      Complex *d = eig_val.fortran_vec ();

      if (info2 == 0)
        {
          octave_idx_type jj = 0;
          for (octave_idx_type i = 0; i < k+1; i++)
            {
              if (dr[i] == 0.0 && di[i] == 0.0 && jj == 0)
                jj++;
              else
                d[i-jj] = Complex (dr[i], di[i]);
            }
          if (jj == 0 && !rvec)
            for (octave_idx_type i = 0; i < k; i++)
              d[i] = d[i+1];

          octave_idx_type k2 = k / 2;
          for (octave_idx_type i = 0; i < k2; i++)
            {
              Complex dtmp = d[i];
              d[i] = d[k - i - 1];
              d[k - i - 1] = dtmp;
            }
          eig_val.resize (k);

          if (rvec)
            {
              OCTAVE_LOCAL_BUFFER (double, dtmp, n);

              for (octave_idx_type i = 0; i < k2; i++)
                {
                  octave_idx_type off1 = i * n;
                  octave_idx_type off2 = (k - i - 1) * n;

                  if (off1 == off2)
                    continue;

                  for (octave_idx_type j = 0; j < n; j++)
                    dtmp[j] = z[off1 + j];

                  for (octave_idx_type j = 0; j < n; j++)
                    z[off1 + j] = z[off2 + j];

                  for (octave_idx_type j = 0; j < n; j++)
                    z[off2 + j] = dtmp[j];
                }

              eig_vec.resize (n, k);
              octave_idx_type i = 0;
              while (i < k)
                {
                  octave_idx_type off1 = i * n;
                  octave_idx_type off2 = (i+1) * n;
                  if (std::imag (eig_val(i)) == 0)
                    {
                      for (octave_idx_type j = 0; j < n; j++)
                        eig_vec(j,i) =
                          Complex (z[j+off1],0.);
                      i++;
                    }
                  else
                    {
                      for (octave_idx_type j = 0; j < n; j++)
                        {
                          eig_vec(j,i) =
                            Complex (z[j+off1],z[j+off2]);
                          if (i < k - 1)
                            eig_vec(j,i+1) =
                              Complex (z[j+off1],-z[j+off2]);
                        }
                      i+=2;
                    }
                }
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("eigs: error %d in dneupd", info2);
          return -1;
        }
    }

  return ip(4);
}

template <class M>
octave_idx_type
EigsComplexNonSymmetricMatrix (const M& m, const std::string typ,
                               octave_idx_type k, octave_idx_type p,
                               octave_idx_type &info, ComplexMatrix &eig_vec,
                               ComplexColumnVector &eig_val, const M& _b,
                               ColumnVector &permB,
                               ComplexColumnVector &cresid,
                               std::ostream& os, double tol, bool rvec,
                               bool cholB, int disp, int maxit)
{
  M b(_b);
  octave_idx_type n = m.cols ();
  octave_idx_type mode = 1;
  bool have_b = ! b.is_empty ();
  bool note3 = false;
  char bmat = 'I';
  Complex sigma = 0.;
  M bt;

  if (m.rows () != m.cols ())
    {
      (*current_liboctave_error_handler) ("eigs: A must be square");
      return -1;
    }
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    {
      (*current_liboctave_error_handler)
        ("eigs: B must be square and the same size as A");
      return -1;
    }

  if (cresid.is_empty ())
    {
      std::string rand_dist = octave_rand::distribution ();
      octave_rand::distribution ("uniform");
      Array<double> rr (octave_rand::vector (n));
      Array<double> ri (octave_rand::vector (n));
      cresid = ComplexColumnVector (n);
      for (octave_idx_type i = 0; i < n; i++)
        cresid(i) = Complex (rr(i),ri(i));
      octave_rand::distribution (rand_dist);
    }

  if (n < 3)
    {
      (*current_liboctave_error_handler)
        ("eigs: n must be at least 3");
      return -1;
    }

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n - 1)
        p = n - 1 ;
    }

  if (k <= 0 || k >= n - 1)
    {
      (*current_liboctave_error_handler)
        ("eigs: Invalid number of eigenvalues to extract (must be 0 < k < n-1).\n"
         "      Use 'eig (full (A))' instead");
      return -1;
    }

  if (p <= k || p >= n)
    {
      (*current_liboctave_error_handler)
        ("eigs: opts.p must be greater than k and less than n");
      return -1;
    }

  if (have_b && cholB && permB.length () != 0)
    {
      // Check the we really have a permutation vector
      if (permB.length () != n)
        {
          (*current_liboctave_error_handler)
            ("eigs: permB vector invalid");
          return -1;
        }
      else
        {
          Array<bool> checked (dim_vector (n, 1), false);
          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_idx_type bidx =
                static_cast<octave_idx_type> (permB(i));
              if (checked(bidx) || bidx < 0 || bidx >= n
                  || D_NINT (bidx) != bidx)
                {
                  (*current_liboctave_error_handler)
                    ("eigs: permB vector invalid");
                  return -1;
                }
            }
        }
    }

  if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
      && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
      && typ != "SI")
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecognized sigma value");
      return -1;
    }

  if (typ == "LA" || typ == "SA" || typ == "BE")
    {
      (*current_liboctave_error_handler)
        ("eigs: invalid sigma value for complex problem");
      return -1;
    }

  if (have_b)
    {
      // See Note 3 dsaupd
      note3 = true;
      if (cholB)
        {
          bt = b;
          b = b.hermitian ();
          if (permB.length () == 0)
            {
              permB = ColumnVector (n);
              for (octave_idx_type i = 0; i < n; i++)
                permB(i) = i;
            }
        }
      else
        {
          if (! make_cholb (b, bt, permB))
            {
              (*current_liboctave_error_handler)
                ("eigs: The matrix B is not positive definite");
              return -1;
            }
        }
    }

  Array<octave_idx_type> ip (dim_vector (11, 1));
  octave_idx_type *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<octave_idx_type> iptr (dim_vector (14, 1));
  octave_idx_type *ipntr = iptr.fortran_vec ();

  octave_idx_type ido = 0;
  int iter = 0;
  octave_idx_type lwork = p * (3 * p + 5);

  OCTAVE_LOCAL_BUFFER (Complex, v, n * p);
  OCTAVE_LOCAL_BUFFER (Complex, workl, lwork);
  OCTAVE_LOCAL_BUFFER (Complex, workd, 3 * n);
  OCTAVE_LOCAL_BUFFER (double, rwork, p);
  Complex *presid = cresid.fortran_vec ();

  do
    {
      F77_FUNC (znaupd, ZNAUPD)
        (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
         F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
         k, tol, presid, p, v, n, iparam,
         ipntr, workd, workl, lwork, rwork, info
         F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      if (f77_exception_encountered)
        {
          (*current_liboctave_error_handler)
            ("eigs: unrecoverable exception encountered in znaupd");
          return -1;
        }

      if (disp > 0 && !xisnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              for (int i = 0 ; i < k; i++)
                os << "    " << workl[iptr(5)+i-1] << "\n";
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer. But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave_NaN;
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              ComplexMatrix mtmp (n,1);
              for (octave_idx_type i = 0; i < n; i++)
                mtmp(i,0) = workd[i + iptr(0) - 1];
              mtmp = utsolve (bt, permB, m * ltsolve (b, permB, mtmp));
              for (octave_idx_type i = 0; i < n; i++)
                workd[i+iptr(1)-1] = mtmp(i,0);

            }
          else if (!vector_product (m, workd + iptr(0) - 1,
                                    workd + iptr(1) - 1))
            break;
        }
      else
        {
          if (info < 0)
            {
              (*current_liboctave_error_handler)
                ("eigs: error %d in znaupd", info);
              return -1;
            }
          break;
        }
    }
  while (1);

  octave_idx_type info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type. It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent. As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<octave_idx_type> s (dim_vector (p, 1));
  octave_idx_type *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  Complex *z = eig_vec.fortran_vec ();

  eig_val.resize (k+1);
  Complex *d = eig_val.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (Complex, workev, 2 * p);

  F77_FUNC (zneupd, ZNEUPD)
    (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, d, z, n, sigma, workev,
     F77_CONST_CHAR_ARG2 (&bmat, 1), n,
     F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
     k, tol, presid, p, v, n, iparam, ipntr, workd, workl, lwork, rwork, info2
     F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable exception encountered in zneupd");
      return -1;
    }

  if (info2 == 0)
    {
      octave_idx_type k2 = k / 2;
      for (octave_idx_type i = 0; i < k2; i++)
        {
          Complex ctmp = d[i];
          d[i] = d[k - i - 1];
          d[k - i - 1] = ctmp;
        }
      eig_val.resize (k);

      if (rvec)
        {
          OCTAVE_LOCAL_BUFFER (Complex, ctmp, n);

          for (octave_idx_type i = 0; i < k2; i++)
            {
              octave_idx_type off1 = i * n;
              octave_idx_type off2 = (k - i - 1) * n;

              if (off1 == off2)
                continue;

              for (octave_idx_type j = 0; j < n; j++)
                ctmp[j] = z[off1 + j];

              for (octave_idx_type j = 0; j < n; j++)
                z[off1 + j] = z[off2 + j];

              for (octave_idx_type j = 0; j < n; j++)
                z[off2 + j] = ctmp[j];
            }

          if (note3)
            eig_vec = ltsolve (b, permB, eig_vec);
        }
    }
  else
    {
      (*current_liboctave_error_handler)
        ("eigs: error %d in zneupd", info2);
      return -1;
    }

  return ip(4);
}

template <class M>
octave_idx_type
EigsComplexNonSymmetricMatrixShift (const M& m, Complex sigma,
                                    octave_idx_type k, octave_idx_type p,
                                    octave_idx_type &info,
                                    ComplexMatrix &eig_vec,
                                    ComplexColumnVector &eig_val, const M& _b,
                                    ColumnVector &permB,
                                    ComplexColumnVector &cresid,
                                    std::ostream& os, double tol, bool rvec,
                                    bool cholB, int disp, int maxit)
{
  M b(_b);
  octave_idx_type n = m.cols ();
  octave_idx_type mode = 3;
  bool have_b = ! b.is_empty ();
  std::string typ = "LM";

  if (m.rows () != m.cols ())
    {
      (*current_liboctave_error_handler) ("eigs: A must be square");
      return -1;
    }
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    {
      (*current_liboctave_error_handler)
        ("eigs: B must be square and the same size as A");
      return -1;
    }

  // FIXME: The "SM" type for mode 1 seems unstable though faster!!
  //if (! std::abs (sigma))
  //  return EigsComplexNonSymmetricMatrix (m, "SM", k, p, info, eig_vec,
  //                                      eig_val, _b, permB, cresid, os, tol,
  //                                      rvec, cholB, disp, maxit);

  if (cresid.is_empty ())
    {
      std::string rand_dist = octave_rand::distribution ();
      octave_rand::distribution ("uniform");
      Array<double> rr (octave_rand::vector (n));
      Array<double> ri (octave_rand::vector (n));
      cresid = ComplexColumnVector (n);
      for (octave_idx_type i = 0; i < n; i++)
        cresid(i) = Complex (rr(i),ri(i));
      octave_rand::distribution (rand_dist);
    }

  if (n < 3)
    {
      (*current_liboctave_error_handler)
        ("eigs: n must be at least 3");
      return -1;
    }

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n - 1)
        p = n - 1 ;
    }

  if (k <= 0 || k >= n - 1)
    {
      (*current_liboctave_error_handler)
        ("eigs: Invalid number of eigenvalues to extract (must be 0 < k < n-1).\n"
         "      Use 'eig (full (A))' instead");
      return -1;
    }

  if (p <= k || p >= n)
    {
      (*current_liboctave_error_handler)
        ("eigs: opts.p must be greater than k and less than n");
      return -1;
    }

  if (have_b && cholB && permB.length () != 0)
    {
      // Check that we really have a permutation vector
      if (permB.length () != n)
        {
          (*current_liboctave_error_handler) ("eigs: permB vector invalid");
          return -1;
        }
      else
        {
          Array<bool> checked (dim_vector (n, 1), false);
          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_idx_type bidx =
                static_cast<octave_idx_type> (permB(i));
              if (checked(bidx) || bidx < 0 || bidx >= n
                  || D_NINT (bidx) != bidx)
                {
                  (*current_liboctave_error_handler)
                    ("eigs: permB vector invalid");
                  return -1;
                }
            }
        }
    }

  char bmat = 'I';
  if (have_b)
    bmat = 'G';

  Array<octave_idx_type> ip (dim_vector (11, 1));
  octave_idx_type *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<octave_idx_type> iptr (dim_vector (14, 1));
  octave_idx_type *ipntr = iptr.fortran_vec ();

  octave_idx_type ido = 0;
  int iter = 0;
  M L, U;

  OCTAVE_LOCAL_BUFFER (octave_idx_type, P, (have_b ? b.rows () : m.rows ()));
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Q, (have_b ? b.cols () : m.cols ()));

  if (! LuAminusSigmaB (m, b, cholB, permB, sigma, L, U, P, Q))
    return -1;

  octave_idx_type lwork = p * (3 * p + 5);

  OCTAVE_LOCAL_BUFFER (Complex, v, n * p);
  OCTAVE_LOCAL_BUFFER (Complex, workl, lwork);
  OCTAVE_LOCAL_BUFFER (Complex, workd, 3 * n);
  OCTAVE_LOCAL_BUFFER (double, rwork, p);
  Complex *presid = cresid.fortran_vec ();

  do
    {
      F77_FUNC (znaupd, ZNAUPD)
        (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
         F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
         k, tol, presid, p, v, n, iparam,
         ipntr, workd, workl, lwork, rwork, info
         F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      if (f77_exception_encountered)
        {
          (*current_liboctave_error_handler)
            ("eigs: unrecoverable exception encountered in znaupd");
          return -1;
        }

      if (disp > 0 && !xisnan(workl[iptr(5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              for (int i = 0 ; i < k; i++)
                os << "    " << workl[iptr(5)+i-1] << "\n";
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer. But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave_NaN;
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              if (ido == -1)
                {
                  OCTAVE_LOCAL_BUFFER (Complex, ctmp, n);

                  vector_product (m, workd+iptr(0)-1, ctmp);

                  ComplexMatrix tmp(n, 1);

                  for (octave_idx_type i = 0; i < n; i++)
                    tmp(i,0) = ctmp[P[i]];

                  lusolve (L, U, tmp);

                  Complex *ip2 = workd+iptr(1)-1;
                  for (octave_idx_type i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i,0);
                }
              else if (ido == 2)
                vector_product (b, workd + iptr(0) - 1, workd + iptr(1) - 1);
              else
                {
                  Complex *ip2 = workd+iptr(2)-1;
                  ComplexMatrix tmp(n, 1);

                  for (octave_idx_type i = 0; i < n; i++)
                    tmp(i,0) = ip2[P[i]];

                  lusolve (L, U, tmp);

                  ip2 = workd+iptr(1)-1;
                  for (octave_idx_type i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i,0);
                }
            }
          else
            {
              if (ido == 2)
                {
                  for (octave_idx_type i = 0; i < n; i++)
                    workd[iptr(0) + i - 1] =
                      workd[iptr(1) + i - 1];
                }
              else
                {
                  Complex *ip2 = workd+iptr(0)-1;
                  ComplexMatrix tmp(n, 1);

                  for (octave_idx_type i = 0; i < n; i++)
                    tmp(i,0) = ip2[P[i]];

                  lusolve (L, U, tmp);

                  ip2 = workd+iptr(1)-1;
                  for (octave_idx_type i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i,0);
                }
            }
        }
      else
        {
          if (info < 0)
            {
              (*current_liboctave_error_handler)
                ("eigs: error %d in dsaupd", info);
              return -1;
            }
          break;
        }
    }
  while (1);

  octave_idx_type info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type. It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent. As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<octave_idx_type> s (dim_vector (p, 1));
  octave_idx_type *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  Complex *z = eig_vec.fortran_vec ();

  eig_val.resize (k+1);
  Complex *d = eig_val.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (Complex, workev, 2 * p);

  F77_FUNC (zneupd, ZNEUPD)
    (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, d, z, n, sigma, workev,
     F77_CONST_CHAR_ARG2 (&bmat, 1), n,
     F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
     k, tol, presid, p, v, n, iparam, ipntr, workd, workl, lwork, rwork, info2
     F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable exception encountered in zneupd");
      return -1;
    }

  if (info2 == 0)
    {
      octave_idx_type k2 = k / 2;
      for (octave_idx_type i = 0; i < k2; i++)
        {
          Complex ctmp = d[i];
          d[i] = d[k - i - 1];
          d[k - i - 1] = ctmp;
        }
      eig_val.resize (k);

      if (rvec)
        {
          OCTAVE_LOCAL_BUFFER (Complex, ctmp, n);

          for (octave_idx_type i = 0; i < k2; i++)
            {
              octave_idx_type off1 = i * n;
              octave_idx_type off2 = (k - i - 1) * n;

              if (off1 == off2)
                continue;

              for (octave_idx_type j = 0; j < n; j++)
                ctmp[j] = z[off1 + j];

              for (octave_idx_type j = 0; j < n; j++)
                z[off1 + j] = z[off2 + j];

              for (octave_idx_type j = 0; j < n; j++)
                z[off2 + j] = ctmp[j];
            }
        }
    }
  else
    {
      (*current_liboctave_error_handler)
        ("eigs: error %d in zneupd", info2);
      return -1;
    }

  return ip(4);
}

octave_idx_type
EigsComplexNonSymmetricFunc (EigsComplexFunc fun, octave_idx_type n,
                             const std::string &_typ, Complex sigma,
                             octave_idx_type k, octave_idx_type p,
                             octave_idx_type &info, ComplexMatrix &eig_vec,
                             ComplexColumnVector &eig_val,
                             ComplexColumnVector &cresid, std::ostream& os,
                             double tol, bool rvec, bool /* cholB */,
                             int disp, int maxit)
{
  std::string typ (_typ);
  bool have_sigma = (std::abs (sigma) ? true : false);
  char bmat = 'I';
  octave_idx_type mode = 1;
  int err = 0;

  if (cresid.is_empty ())
    {
      std::string rand_dist = octave_rand::distribution ();
      octave_rand::distribution ("uniform");
      Array<double> rr (octave_rand::vector (n));
      Array<double> ri (octave_rand::vector (n));
      cresid = ComplexColumnVector (n);
      for (octave_idx_type i = 0; i < n; i++)
        cresid(i) = Complex (rr(i),ri(i));
      octave_rand::distribution (rand_dist);
    }

  if (n < 3)
    {
      (*current_liboctave_error_handler)
        ("eigs: n must be at least 3");
      return -1;
    }

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n - 1)
        p = n - 1 ;
    }

  if (k <= 0 || k >= n - 1)
    {
      (*current_liboctave_error_handler)
        ("eigs: Invalid number of eigenvalues to extract (must be 0 < k < n-1).\n"
         "      Use 'eig (full (A))' instead");
      return -1;
    }

  if (p <= k || p >= n)
    {
      (*current_liboctave_error_handler)
        ("eigs: opts.p must be greater than k and less than n");
      return -1;
    }

  if (! have_sigma)
    {
      if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
          && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
          && typ != "SI")
        (*current_liboctave_error_handler)
          ("eigs: unrecognized sigma value");

      if (typ == "LA" || typ == "SA" || typ == "BE")
        {
          (*current_liboctave_error_handler)
            ("eigs: invalid sigma value for complex problem");
          return -1;
        }

      if (typ == "SM")
        {
          typ = "LM";
          sigma = 0.;
          mode = 3;
        }
    }
  else if (! std::abs (sigma))
    typ = "SM";
  else
    {
      typ = "LM";
      mode = 3;
    }

  Array<octave_idx_type> ip (dim_vector (11, 1));
  octave_idx_type *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<octave_idx_type> iptr (dim_vector (14, 1));
  octave_idx_type *ipntr = iptr.fortran_vec ();

  octave_idx_type ido = 0;
  int iter = 0;
  octave_idx_type lwork = p * (3 * p + 5);

  OCTAVE_LOCAL_BUFFER (Complex, v, n * p);
  OCTAVE_LOCAL_BUFFER (Complex, workl, lwork);
  OCTAVE_LOCAL_BUFFER (Complex, workd, 3 * n);
  OCTAVE_LOCAL_BUFFER (double, rwork, p);
  Complex *presid = cresid.fortran_vec ();

  do
    {
      F77_FUNC (znaupd, ZNAUPD)
        (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
         F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
         k, tol, presid, p, v, n, iparam,
         ipntr, workd, workl, lwork, rwork, info
         F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      if (f77_exception_encountered)
        {
          (*current_liboctave_error_handler)
            ("eigs: unrecoverable exception encountered in znaupd");
          return -1;
        }

      if (disp > 0 && !xisnan(workl[iptr(5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              for (int i = 0 ; i < k; i++)
                os << "    " << workl[iptr(5)+i-1] << "\n";
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer. But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave_NaN;
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          Complex *ip2 = workd + iptr(0) - 1;
          ComplexColumnVector x(n);

          for (octave_idx_type i = 0; i < n; i++)
            x(i) = *ip2++;

          ComplexColumnVector y = fun (x, err);

          if (err)
            return false;

          ip2 = workd + iptr(1) - 1;
          for (octave_idx_type i = 0; i < n; i++)
            *ip2++ = y(i);
        }
      else
        {
          if (info < 0)
            {
              (*current_liboctave_error_handler)
                ("eigs: error %d in dsaupd", info);
              return -1;
            }
          break;
        }
    }
  while (1);

  octave_idx_type info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type. It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent. As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<octave_idx_type> s (dim_vector (p, 1));
  octave_idx_type *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  Complex *z = eig_vec.fortran_vec ();

  eig_val.resize (k+1);
  Complex *d = eig_val.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (Complex, workev, 2 * p);

  F77_FUNC (zneupd, ZNEUPD)
    (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, d, z, n, sigma, workev,
     F77_CONST_CHAR_ARG2 (&bmat, 1), n,
     F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
     k, tol, presid, p, v, n, iparam, ipntr, workd, workl, lwork, rwork, info2
     F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler)
        ("eigs: unrecoverable exception encountered in zneupd");
      return -1;
    }

  if (info2 == 0)
    {
      octave_idx_type k2 = k / 2;
      for (octave_idx_type i = 0; i < k2; i++)
        {
          Complex ctmp = d[i];
          d[i] = d[k - i - 1];
          d[k - i - 1] = ctmp;
        }
      eig_val.resize (k);

      if (rvec)
        {
          OCTAVE_LOCAL_BUFFER (Complex, ctmp, n);

          for (octave_idx_type i = 0; i < k2; i++)
            {
              octave_idx_type off1 = i * n;
              octave_idx_type off2 = (k - i - 1) * n;

              if (off1 == off2)
                continue;

              for (octave_idx_type j = 0; j < n; j++)
                ctmp[j] = z[off1 + j];

              for (octave_idx_type j = 0; j < n; j++)
                z[off1 + j] = z[off2 + j];

              for (octave_idx_type j = 0; j < n; j++)
                z[off2 + j] = ctmp[j];
            }
        }
    }
  else
    {
      (*current_liboctave_error_handler)
        ("eigs: error %d in zneupd", info2);
      return -1;
    }

  return ip(4);
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern octave_idx_type
EigsRealSymmetricMatrix (const Matrix& m, const std::string typ,
                         octave_idx_type k, octave_idx_type p,
                         octave_idx_type &info, Matrix &eig_vec,
                         ColumnVector &eig_val, const Matrix& b,
                         ColumnVector &permB, ColumnVector &resid,
                         std::ostream &os,
                         double tol = std::numeric_limits<double>::epsilon (),
                         bool rvec = false, bool cholB = 0, int disp = 0,
                         int maxit = 300);

extern octave_idx_type
EigsRealSymmetricMatrix (const SparseMatrix& m, const std::string typ,
                         octave_idx_type k, octave_idx_type p,
                         octave_idx_type &info, Matrix &eig_vec,
                         ColumnVector &eig_val, const SparseMatrix& b,
                         ColumnVector &permB, ColumnVector &resid,
                         std::ostream& os,
                         double tol = std::numeric_limits<double>::epsilon (),
                         bool rvec = false, bool cholB = 0, int disp = 0,
                         int maxit = 300);

extern octave_idx_type
EigsRealSymmetricMatrixShift (const Matrix& m, double sigma,
                              octave_idx_type k, octave_idx_type p,
                              octave_idx_type &info, Matrix &eig_vec,
                              ColumnVector &eig_val, const Matrix& b,
                              ColumnVector &permB, ColumnVector &resid,
                              std::ostream &os,
                              double tol = std::numeric_limits<double>::epsilon (),
                              bool rvec = false, bool cholB = 0, int disp = 0,
                              int maxit = 300);

extern octave_idx_type
EigsRealSymmetricMatrixShift (const SparseMatrix& m, double sigma,
                              octave_idx_type k, octave_idx_type p,
                              octave_idx_type &info, Matrix &eig_vec,
                              ColumnVector &eig_val, const SparseMatrix& b,
                              ColumnVector &permB, ColumnVector &resid,
                              std::ostream &os,
                              double tol = std::numeric_limits<double>::epsilon (),
                              bool rvec = false, bool cholB = 0, int disp = 0,
                              int maxit = 300);

extern octave_idx_type
EigsRealSymmetricFunc (EigsFunc fun, octave_idx_type n,
                       const std::string &typ, double sigma,
                       octave_idx_type k, octave_idx_type p,
                       octave_idx_type &info,
                       Matrix &eig_vec, ColumnVector &eig_val,
                       ColumnVector &resid, std::ostream &os,
                       double tol = std::numeric_limits<double>::epsilon (),
                       bool rvec = false, bool cholB = 0, int disp = 0,
                       int maxit = 300);

extern octave_idx_type
EigsRealNonSymmetricMatrix (const Matrix& m, const std::string typ,
                            octave_idx_type k, octave_idx_type p,
                            octave_idx_type &info, ComplexMatrix &eig_vec,
                            ComplexColumnVector &eig_val, const Matrix& b,
                            ColumnVector &permB, ColumnVector &resid,
                            std::ostream &os,
                            double tol = std::numeric_limits<double>::epsilon (),
                            bool rvec = false, bool cholB = 0, int disp = 0,
                            int maxit = 300);

extern octave_idx_type
EigsRealNonSymmetricMatrix (const SparseMatrix& m, const std::string typ,
                            octave_idx_type k, octave_idx_type p,
                            octave_idx_type &info, ComplexMatrix &eig_vec,
                            ComplexColumnVector &eig_val,
                            const SparseMatrix& b,
                            ColumnVector &permB, ColumnVector &resid,
                            std::ostream &os,
                            double tol = std::numeric_limits<double>::epsilon (),
                            bool rvec = false, bool cholB = 0, int disp = 0,
                            int maxit = 300);

extern octave_idx_type
EigsRealNonSymmetricMatrixShift (const Matrix& m, double sigma,
                                 octave_idx_type k, octave_idx_type p,
                                 octave_idx_type &info,
                                 ComplexMatrix &eig_vec,
                                 ComplexColumnVector &eig_val, const Matrix& b,
                                 ColumnVector &permB, ColumnVector &resid,
                                 std::ostream &os,
                                 double tol = std::numeric_limits<double>::epsilon (),
                                 bool rvec = false, bool cholB = 0,
                                 int disp = 0, int maxit = 300);

extern octave_idx_type
EigsRealNonSymmetricMatrixShift (const SparseMatrix& m, double sigma,
                                 octave_idx_type k, octave_idx_type p,
                                 octave_idx_type &info,
                                 ComplexMatrix &eig_vec,
                                 ComplexColumnVector &eig_val,
                                 const SparseMatrix& b,
                                 ColumnVector &permB, ColumnVector &resid,
                                 std::ostream &os,
                                 double tol = std::numeric_limits<double>::epsilon (),
                                 bool rvec = false, bool cholB = 0,
                                 int disp = 0, int maxit = 300);

extern octave_idx_type
EigsRealNonSymmetricFunc (EigsFunc fun, octave_idx_type n,
                          const std::string &_typ, double sigma,
                          octave_idx_type k, octave_idx_type p,
                          octave_idx_type &info, ComplexMatrix &eig_vec,
                          ComplexColumnVector &eig_val,
                          ColumnVector &resid, std::ostream& os,
                          double tol = std::numeric_limits<double>::epsilon (),
                          bool rvec = false, bool cholB = 0, int disp = 0,
                          int maxit = 300);

extern octave_idx_type
EigsComplexNonSymmetricMatrix (const ComplexMatrix& m, const std::string typ,
                               octave_idx_type k, octave_idx_type p,
                               octave_idx_type &info, ComplexMatrix &eig_vec,
                               ComplexColumnVector &eig_val,
                               const ComplexMatrix& b, ColumnVector &permB,
                               ComplexColumnVector &resid,
                               std::ostream &os,
                               double tol = std::numeric_limits<double>::epsilon (),
                               bool rvec = false, bool cholB = 0, int disp = 0,
                               int maxit = 300);

extern octave_idx_type
EigsComplexNonSymmetricMatrix (const SparseComplexMatrix& m,
                               const std::string typ, octave_idx_type k,
                               octave_idx_type p, octave_idx_type &info,
                               ComplexMatrix &eig_vec,
                               ComplexColumnVector &eig_val,
                               const SparseComplexMatrix& b,
                               ColumnVector &permB,
                               ComplexColumnVector &resid,
                               std::ostream &os,
                               double tol = std::numeric_limits<double>::epsilon (),
                               bool rvec = false, bool cholB = 0, int disp = 0,
                               int maxit = 300);

extern octave_idx_type
EigsComplexNonSymmetricMatrixShift (const ComplexMatrix& m, Complex sigma,
                                    octave_idx_type k, octave_idx_type p,
                                    octave_idx_type &info,
                                    ComplexMatrix &eig_vec,
                                    ComplexColumnVector &eig_val,
                                    const ComplexMatrix& b,
                                    ColumnVector &permB,
                                    ComplexColumnVector &resid,
                                    std::ostream &os,
                                    double tol = std::numeric_limits<double>::epsilon (),
                                    bool rvec = false, bool cholB = 0,
                                    int disp = 0, int maxit = 300);

extern octave_idx_type
EigsComplexNonSymmetricMatrixShift (const SparseComplexMatrix& m,
                                    Complex sigma,
                                    octave_idx_type k, octave_idx_type p,
                                    octave_idx_type &info,
                                    ComplexMatrix &eig_vec,
                                    ComplexColumnVector &eig_val,
                                    const SparseComplexMatrix& b,
                                    ColumnVector &permB,
                                    ComplexColumnVector &resid,
                                    std::ostream &os,
                                    double tol = std::numeric_limits<double>::epsilon (),
                                    bool rvec = false, bool cholB = 0,
                                    int disp = 0, int maxit = 300);

extern octave_idx_type
EigsComplexNonSymmetricFunc (EigsComplexFunc fun, octave_idx_type n,
                             const std::string &_typ, Complex sigma,
                             octave_idx_type k, octave_idx_type p,
                             octave_idx_type &info, ComplexMatrix &eig_vec,
                             ComplexColumnVector &eig_val,
                             ComplexColumnVector &resid, std::ostream& os,
                             double tol = std::numeric_limits<double>::epsilon (),
                             bool rvec = false, bool cholB = 0,
                             int disp = 0, int maxit = 300);
#endif

#ifndef _MSC_VER
template octave_idx_type
lusolve (const SparseMatrix&, const SparseMatrix&, Matrix&);

template octave_idx_type
lusolve (const SparseComplexMatrix&, const SparseComplexMatrix&,
         ComplexMatrix&);

template octave_idx_type
lusolve (const Matrix&, const Matrix&, Matrix&);

template octave_idx_type
lusolve (const ComplexMatrix&, const ComplexMatrix&, ComplexMatrix&);

template ComplexMatrix
ltsolve (const SparseComplexMatrix&, const ColumnVector&,
         const ComplexMatrix&);

template Matrix
ltsolve (const SparseMatrix&, const ColumnVector&, const Matrix&);

template ComplexMatrix
ltsolve (const ComplexMatrix&, const ColumnVector&, const ComplexMatrix&);

template Matrix
ltsolve (const Matrix&, const ColumnVector&, const Matrix&);

template ComplexMatrix
utsolve (const SparseComplexMatrix&, const ColumnVector&,
         const ComplexMatrix&);

template Matrix
utsolve (const SparseMatrix&, const ColumnVector&, const Matrix&);

template ComplexMatrix
utsolve (const ComplexMatrix&, const ColumnVector&, const ComplexMatrix&);

template Matrix
utsolve (const Matrix&, const ColumnVector&, const Matrix&);
#endif

#endif
