// Matrix manipulations.
/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008 Jaroslav Hajek
Copyright (C) 2009 VZLU Prague, a.s.

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

#include <iostream>
#include <vector>

#include "Array-util.h"
#include "byte-swap.h"
#include "boolMatrix.h"
#include "chMatrix.h"
#include "dMatrix.h"
#include "dDiagMatrix.h"
#include "CMatrix.h"
#include "dColVector.h"
#include "dRowVector.h"
#include "CColVector.h"
#include "PermMatrix.h"
#include "DET.h"
#include "dbleSCHUR.h"
#include "dbleSVD.h"
#include "dbleCHOL.h"
#include "f77-fcn.h"
#include "functor.h"
#include "lo-error.h"
#include "oct-locbuf.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-m-dm.h"
#include "mx-dm-m.h"
#include "mx-inlines.cc"
#include "mx-op-defs.h"
#include "oct-cmplx.h"
#include "oct-fftw.h"
#include "oct-norm.h"
#include "quit.h"

// Fortran functions we call.

extern "C"
{
  F77_RET_T
  F77_FUNC (xilaenv, XILAENV) (const octave_idx_type&,
                               F77_CONST_CHAR_ARG_DECL,
                               F77_CONST_CHAR_ARG_DECL,
                               const octave_idx_type&, const octave_idx_type&,
                               const octave_idx_type&, const octave_idx_type&,
                               octave_idx_type&
                               F77_CHAR_ARG_LEN_DECL
                               F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgebal, DGEBAL) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type&,
                             octave_idx_type&, double*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgebak, DGEBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);


  F77_RET_T
  F77_FUNC (dgemm, DGEMM) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const octave_idx_type&, const double&,
                           const double*, const octave_idx_type&,
                           const double*, const octave_idx_type&,
                           const double&, double*, const octave_idx_type&
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
  F77_FUNC (xddot, XDDOT) (const octave_idx_type&, const double*,
                           const octave_idx_type&, const double*,
                           const octave_idx_type&, double&);

  F77_RET_T
  F77_FUNC (dsyrk, DSYRK) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const double&, const double*, const octave_idx_type&,
                           const double&, double*, const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgetrf, DGETRF) (const octave_idx_type&, const octave_idx_type&,
                             double*, const octave_idx_type&,
                             octave_idx_type*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgetrs, DGETRS) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const double*, const octave_idx_type&,
                             const octave_idx_type*, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgetri, DGETRI) (const octave_idx_type&, double*,
                             const octave_idx_type&, const octave_idx_type*,
                             double*, const octave_idx_type&,
                             octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgecon, DGECON) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, const double&, double&,
                             double*, octave_idx_type*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgelsy, DGELSY) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             double&, octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgelsd, DGELSD) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*, double&,
                             octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             octave_idx_type&);

  F77_RET_T
  F77_FUNC (dpotrf, DPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, double *,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpocon, DPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, const double&,
                             double&, double*, octave_idx_type*,
                             octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (dpotrs, DPOTRS) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const double*, const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dtrtri, DTRTRI) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (dtrcon, DTRCON) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const double*,
                             const octave_idx_type&, double&,
                             double*, octave_idx_type*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (dtrtrs, DTRTRS) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const double*, const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dlartg, DLARTG) (const double&, const double&, double&,
                             double&, double&);

  F77_RET_T
  F77_FUNC (dtrsyl, DTRSYL) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, const double*,
                             const octave_idx_type&, const double*,
                             const octave_idx_type&, const double*,
                             const octave_idx_type&, double&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xdlange, XDLANGE) (F77_CONST_CHAR_ARG_DECL,
                               const octave_idx_type&, const octave_idx_type&,
                               const double*, const octave_idx_type&,
                               double*, double&
                               F77_CHAR_ARG_LEN_DECL);
}

// Matrix class.

Matrix::Matrix (const RowVector& rv)
  : NDArray (rv)
{
}

Matrix::Matrix (const ColumnVector& cv)
  : NDArray (cv)
{
}

Matrix::Matrix (const DiagMatrix& a)
  : NDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

Matrix::Matrix (const MDiagArray2<double>& a)
  : NDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

Matrix::Matrix (const DiagArray2<double>& a)
  : NDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

Matrix::Matrix (const PermMatrix& a)
  : NDArray (a.dims (), 0.0)
{
  const Array<octave_idx_type> ia (a.col_perm_vec ());
  octave_idx_type len = a.rows ();
  for (octave_idx_type i = 0; i < len; i++)
    elem (ia(i), i) = 1.0;
}

// FIXME: could we use a templated mixed-type copy function here?

Matrix::Matrix (const boolMatrix& a)
  : NDArray (a)
{
}

Matrix::Matrix (const charMatrix& a)
  : NDArray (a.dims ())
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    for (octave_idx_type j = 0; j < a.cols (); j++)
      elem (i, j) = static_cast<unsigned char> (a.elem (i, j));
}

bool
Matrix::operator == (const Matrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return false;

  return mx_inline_equal (length (), data (), a.data ());
}

bool
Matrix::operator != (const Matrix& a) const
{
  return !(*this == a);
}

bool
Matrix::is_symmetric (void) const
{
  if (is_square () && rows () > 0)
    {
      for (octave_idx_type i = 0; i < rows (); i++)
        for (octave_idx_type j = i+1; j < cols (); j++)
          if (elem (i, j) != elem (j, i))
            return false;

      return true;
    }

  return false;
}

Matrix&
Matrix::insert (const Matrix& a, octave_idx_type r, octave_idx_type c)
{
  Array<double>::insert (a, r, c);
  return *this;
}

Matrix&
Matrix::insert (const RowVector& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.length ();

  if (r < 0 || r >= rows () || c < 0 || c + a_len > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r, c+i) = a.elem (i);
    }

  return *this;
}

Matrix&
Matrix::insert (const ColumnVector& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.length ();

  if (r < 0 || r + a_len > rows () || c < 0 || c >= cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r+i, c) = a.elem (i);
    }

  return *this;
}

Matrix&
Matrix::insert (const DiagMatrix& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  fill (0.0, r, c, r + a_nr - 1, c + a_nc - 1);

  octave_idx_type a_len = a.length ();

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r+i, c+i) = a.elem (i, i);
    }

  return *this;
}

Matrix&
Matrix::fill (double val)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      make_unique ();

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          xelem (i, j) = val;
    }

  return *this;
}

Matrix&
Matrix::fill (double val, octave_idx_type r1, octave_idx_type c1,
              octave_idx_type r2, octave_idx_type c2)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  if (r2 >= r1 && c2 >= c1)
    {
      make_unique ();

      for (octave_idx_type j = c1; j <= c2; j++)
        for (octave_idx_type i = r1; i <= r2; i++)
          xelem (i, j) = val;
    }

  return *this;
}

Matrix
Matrix::append (const Matrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return Matrix ();
    }

  octave_idx_type nc_insert = nc;
  Matrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const RowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != 1)
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return Matrix ();
    }

  octave_idx_type nc_insert = nc;
  Matrix retval (nr, nc + a.length ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const ColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.length ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return Matrix ();
    }

  octave_idx_type nc_insert = nc;
  Matrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const DiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  Matrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::stack (const Matrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return Matrix ();
    }

  octave_idx_type nr_insert = nr;
  Matrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const RowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return Matrix ();
    }

  octave_idx_type nr_insert = nr;
  Matrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const ColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != 1)
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return Matrix ();
    }

  octave_idx_type nr_insert = nr;
  Matrix retval (nr + a.length (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const DiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return Matrix ();
    }

  octave_idx_type nr_insert = nr;
  Matrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
real (const ComplexMatrix& a)
{
  return do_mx_unary_op<double, Complex> (a, mx_inline_real);
}

Matrix
imag (const ComplexMatrix& a)
{
  return do_mx_unary_op<double, Complex> (a, mx_inline_imag);
}

Matrix
Matrix::extract (octave_idx_type r1, octave_idx_type c1,
                 octave_idx_type r2, octave_idx_type c2) const
{
  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  return index (idx_vector (r1, r2+1), idx_vector (c1, c2+1));
}

Matrix
Matrix::extract_n (octave_idx_type r1, octave_idx_type c1, octave_idx_type nr,
                   octave_idx_type nc) const
{
  return index (idx_vector (r1, r1 + nr), idx_vector (c1, c1 + nc));
}

// extract row or column i.

RowVector
Matrix::row (octave_idx_type i) const
{
  return index (idx_vector (i), idx_vector::colon);
}

ColumnVector
Matrix::column (octave_idx_type i) const
{
  return index (idx_vector::colon, idx_vector (i));
}

Matrix
Matrix::inverse (void) const
{
  octave_idx_type info;
  double rcon;
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, 0, 0);
}

Matrix
Matrix::inverse (octave_idx_type& info) const
{
  double rcon;
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, 0, 0);
}

Matrix
Matrix::inverse (octave_idx_type& info, double& rcon, int force,
                 int calc_cond) const
{
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, force, calc_cond);
}

Matrix
Matrix::inverse (MatrixType& mattype) const
{
  octave_idx_type info;
  double rcon;
  return inverse (mattype, info, rcon, 0, 0);
}

Matrix
Matrix::inverse (MatrixType &mattype, octave_idx_type& info) const
{
  double rcon;
  return inverse (mattype, info, rcon, 0, 0);
}

Matrix
Matrix::tinverse (MatrixType &mattype, octave_idx_type& info, double& rcon,
                  int force, int calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc || nr == 0 || nc == 0)
    (*current_liboctave_error_handler) ("inverse requires square matrix");
  else
    {
      int typ = mattype.type ();
      char uplo = (typ == MatrixType::Lower ? 'L' : 'U');
      char udiag = 'N';
      retval = *this;
      double *tmp_data = retval.fortran_vec ();

      F77_XFCN (dtrtri, DTRTRI, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                 F77_CONST_CHAR_ARG2 (&udiag, 1),
                                 nr, tmp_data, nr, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      // Throw-away extra info LAPACK gives so as to not change output.
      rcon = 0.0;
      if (info != 0)
        info = -1;
      else if (calc_cond)
        {
          octave_idx_type dtrcon_info = 0;
          char job = '1';

          OCTAVE_LOCAL_BUFFER (double, work, 3 * nr);
          OCTAVE_LOCAL_BUFFER (octave_idx_type, iwork, nr);

          F77_XFCN (dtrcon, DTRCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&udiag, 1),
                                     nr, tmp_data, nr, rcon,
                                     work, iwork, dtrcon_info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          if (dtrcon_info != 0)
            info = -1;
        }

      if (info == -1 && ! force)
        retval = *this; // Restore matrix contents.
    }

  return retval;
}


Matrix
Matrix::finverse (MatrixType &mattype, octave_idx_type& info, double& rcon,
                  int force, int calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc || nr == 0 || nc == 0)
    (*current_liboctave_error_handler) ("inverse requires square matrix");
  else
    {
      Array<octave_idx_type> ipvt (dim_vector (nr, 1));
      octave_idx_type *pipvt = ipvt.fortran_vec ();

      retval = *this;
      double *tmp_data = retval.fortran_vec ();

      Array<double> z (dim_vector (1, 1));
      octave_idx_type lwork = -1;

      // Query the optimum work array size.
      F77_XFCN (dgetri, DGETRI, (nc, tmp_data, nr, pipvt,
                                 z.fortran_vec (), lwork, info));

      lwork = static_cast<octave_idx_type> (z(0));
      lwork = (lwork < 2 *nc ? 2*nc : lwork);
      z.resize (dim_vector (lwork, 1));
      double *pz = z.fortran_vec ();

      info = 0;

      // Calculate the norm of the matrix, for later use.
      double anorm = 0;
      if (calc_cond)
        anorm = retval.abs ().sum ().row (static_cast<octave_idx_type>(0))
                .max ();

      F77_XFCN (dgetrf, DGETRF, (nc, nc, tmp_data, nr, pipvt, info));

      // Throw-away extra info LAPACK gives so as to not change output.
      rcon = 0.0;
      if (info != 0)
        info = -1;
      else if (calc_cond)
        {
          octave_idx_type dgecon_info = 0;

          // Now calculate the condition number for non-singular matrix.
          char job = '1';
          Array<octave_idx_type> iz (dim_vector (nc, 1));
          octave_idx_type *piz = iz.fortran_vec ();
          F77_XFCN (dgecon, DGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                     nc, tmp_data, nr, anorm,
                                     rcon, pz, piz, dgecon_info
                                     F77_CHAR_ARG_LEN (1)));

          if (dgecon_info != 0)
            info = -1;
        }

      if (info == -1 && ! force)
        retval = *this; // Restore matrix contents.
      else
        {
          octave_idx_type dgetri_info = 0;

          F77_XFCN (dgetri, DGETRI, (nc, tmp_data, nr, pipvt,
                                     pz, lwork, dgetri_info));

          if (dgetri_info != 0)
            info = -1;
        }

      if (info != 0)
        mattype.mark_as_rectangular ();
    }

  return retval;
}

Matrix
Matrix::inverse (MatrixType &mattype, octave_idx_type& info, double& rcon,
                 int force, int calc_cond) const
{
  int typ = mattype.type (false);
  Matrix ret;

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);

  if (typ == MatrixType::Upper || typ == MatrixType::Lower)
    ret = tinverse (mattype, info, rcon, force, calc_cond);
  else
    {
      if (mattype.is_hermitian ())
        {
          CHOL chol (*this, info, calc_cond);
          if (info == 0)
            {
              if (calc_cond)
                rcon = chol.rcond ();
              else
                rcon = 1.0;
              ret = chol.inverse ();
            }
          else
            mattype.mark_as_unsymmetric ();
        }

      if (!mattype.is_hermitian ())
        ret = finverse (mattype, info, rcon, force, calc_cond);

      if ((mattype.is_hermitian () || calc_cond) && rcon == 0.)
        ret = Matrix (rows (), columns (), octave_Inf);
    }

  return ret;
}

Matrix
Matrix::pseudo_inverse (double tol) const
{
  SVD result (*this, SVD::economy);

  DiagMatrix S = result.singular_values ();
  Matrix U = result.left_singular_matrix ();
  Matrix V = result.right_singular_matrix ();

  ColumnVector sigma = S.extract_diag ();

  octave_idx_type r = sigma.length () - 1;
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (tol <= 0.0)
    {
      if (nr > nc)
        tol = nr * sigma.elem (0) * std::numeric_limits<double>::epsilon ();
      else
        tol = nc * sigma.elem (0) * std::numeric_limits<double>::epsilon ();
    }

  while (r >= 0 && sigma.elem (r) < tol)
    r--;

  if (r < 0)
    return Matrix (nc, nr, 0.0);
  else
    {
      Matrix Ur = U.extract (0, 0, nr-1, r);
      DiagMatrix D = DiagMatrix (sigma.extract (0, r)) . inverse ();
      Matrix Vr = V.extract (0, 0, nc-1, r);
      return Vr * D * Ur.transpose ();
    }
}

#if defined (HAVE_FFTW)

ComplexMatrix
Matrix::fourier (void) const
{
  size_t nr = rows ();
  size_t nc = cols ();

  ComplexMatrix retval (nr, nc);

  size_t npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  const double *in (fortran_vec ());
  Complex *out (retval.fortran_vec ());

  octave_fftw::fft (in, out, npts, nsamples);

  return retval;
}

ComplexMatrix
Matrix::ifourier (void) const
{
  size_t nr = rows ();
  size_t nc = cols ();

  ComplexMatrix retval (nr, nc);

  size_t npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  ComplexMatrix tmp (*this);
  Complex *in (tmp.fortran_vec ());
  Complex *out (retval.fortran_vec ());

  octave_fftw::ifft (in, out, npts, nsamples);

  return retval;
}

ComplexMatrix
Matrix::fourier2d (void) const
{
  dim_vector dv(rows (), cols ());

  const double *in = fortran_vec ();
  ComplexMatrix retval (rows (), cols ());
  octave_fftw::fftNd (in, retval.fortran_vec (), 2, dv);

  return retval;
}

ComplexMatrix
Matrix::ifourier2d (void) const
{
  dim_vector dv(rows (), cols ());

  ComplexMatrix retval (*this);
  Complex *out (retval.fortran_vec ());

  octave_fftw::ifftNd (out, out, 2, dv);

  return retval;
}

#else

extern "C"
{
  // Note that the original complex fft routines were not written for
  // double complex arguments.  They have been modified by adding an
  // implicit double precision (a-h,o-z) statement at the beginning of
  // each subroutine.

  F77_RET_T
  F77_FUNC (zffti, ZFFTI) (const octave_idx_type&, Complex*);

  F77_RET_T
  F77_FUNC (zfftf, ZFFTF) (const octave_idx_type&, Complex*, Complex*);

  F77_RET_T
  F77_FUNC (zfftb, ZFFTB) (const octave_idx_type&, Complex*, Complex*);
}

ComplexMatrix
Matrix::fourier (void) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  octave_idx_type nn = 4*npts+15;

  Array<Complex> wsave (dim_vector (nn, 1));
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (zffti, ZFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      F77_FUNC (zfftf, ZFFTF) (npts, &tmp_data[npts*j], pwsave);
    }

  return retval;
}

ComplexMatrix
Matrix::ifourier (void) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  octave_idx_type nn = 4*npts+15;

  Array<Complex> wsave (dim_vector (nn, 1));
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (zffti, ZFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      F77_FUNC (zfftb, ZFFTB) (npts, &tmp_data[npts*j], pwsave);
    }

  for (octave_idx_type j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<double> (npts);

  return retval;
}

ComplexMatrix
Matrix::fourier2d (void) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  octave_idx_type nn = 4*npts+15;

  Array<Complex> wsave (dim_vector (nn, 1));
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (zffti, ZFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      F77_FUNC (zfftf, ZFFTF) (npts, &tmp_data[npts*j], pwsave);
    }

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;

  wsave.resize (dim_vector (nn, 1));
  pwsave = wsave.fortran_vec ();

  Array<Complex> tmp (dim_vector (npts, 1));
  Complex *prow = tmp.fortran_vec ();

  F77_FUNC (zffti, ZFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      for (octave_idx_type i = 0; i < npts; i++)
        prow[i] = tmp_data[i*nr + j];

      F77_FUNC (zfftf, ZFFTF) (npts, prow, pwsave);

      for (octave_idx_type i = 0; i < npts; i++)
        tmp_data[i*nr + j] = prow[i];
    }

  return retval;
}

ComplexMatrix
Matrix::ifourier2d (void) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  octave_idx_type nn = 4*npts+15;

  Array<Complex> wsave (dim_vector (nn, 1));
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (zffti, ZFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      F77_FUNC (zfftb, ZFFTB) (npts, &tmp_data[npts*j], pwsave);
    }

  for (octave_idx_type j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<double> (npts);

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;

  wsave.resize (dim_vector (nn, 1));
  pwsave = wsave.fortran_vec ();

  Array<Complex> tmp (dim_vector (npts, 1));
  Complex *prow = tmp.fortran_vec ();

  F77_FUNC (zffti, ZFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      for (octave_idx_type i = 0; i < npts; i++)
        prow[i] = tmp_data[i*nr + j];

      F77_FUNC (zfftb, ZFFTB) (npts, prow, pwsave);

      for (octave_idx_type i = 0; i < npts; i++)
        tmp_data[i*nr + j] = prow[i] / static_cast<double> (npts);
    }

  return retval;
}

#endif

DET
Matrix::determinant (void) const
{
  octave_idx_type info;
  double rcon;
  return determinant (info, rcon, 0);
}

DET
Matrix::determinant (octave_idx_type& info) const
{
  double rcon;
  return determinant (info, rcon, 0);
}

DET
Matrix::determinant (octave_idx_type& info, double& rcon, int calc_cond) const
{
  MatrixType mattype (*this);
  return determinant (mattype, info, rcon, calc_cond);
}

DET
Matrix::determinant (MatrixType& mattype,
                     octave_idx_type& info, double& rcon, int calc_cond) const
{
  DET retval (1.0);

  info = 0;
  rcon = 0.0;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc)
    (*current_liboctave_error_handler) ("matrix must be square");
  else
    {
      volatile int typ = mattype.type ();

      // Even though the matrix is marked as singular (Rectangular), we may
      // still get a useful number from the LU factorization, because it always
      // completes.

      if (typ == MatrixType::Unknown)
        typ = mattype.type (*this);
      else if (typ == MatrixType::Rectangular)
        typ = MatrixType::Full;

      if (typ == MatrixType::Lower || typ == MatrixType::Upper)
        {
          for (octave_idx_type i = 0; i < nc; i++)
            retval *= elem (i,i);
        }
      else if (typ == MatrixType::Hermitian)
        {
          Matrix atmp = *this;
          double *tmp_data = atmp.fortran_vec ();

          double anorm = 0;
          if (calc_cond) anorm = xnorm (*this, 1);


          char job = 'L';
          F77_XFCN (dpotrf, DPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                     tmp_data, nr, info
                                     F77_CHAR_ARG_LEN (1)));

          if (info != 0)
            {
              rcon = 0.0;
              mattype.mark_as_unsymmetric ();
              typ = MatrixType::Full;
            }
          else
            {
              Array<double> z (dim_vector (3 * nc, 1));
              double *pz = z.fortran_vec ();
              Array<octave_idx_type> iz (dim_vector (nc, 1));
              octave_idx_type *piz = iz.fortran_vec ();

              F77_XFCN (dpocon, DPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                         nr, tmp_data, nr, anorm,
                                         rcon, pz, piz, info
                                         F77_CHAR_ARG_LEN (1)));

              if (info != 0)
                rcon = 0.0;

              for (octave_idx_type i = 0; i < nc; i++)
                retval *= atmp (i,i);

              retval = retval.square ();
            }
        }
      else if (typ != MatrixType::Full)
        (*current_liboctave_error_handler) ("det: invalid dense matrix type");

      if (typ == MatrixType::Full)
        {
          Array<octave_idx_type> ipvt (dim_vector (nr, 1));
          octave_idx_type *pipvt = ipvt.fortran_vec ();

          Matrix atmp = *this;
          double *tmp_data = atmp.fortran_vec ();

          info = 0;

          // Calculate the norm of the matrix, for later use.
          double anorm = 0;
          if (calc_cond) anorm = xnorm (*this, 1);

          F77_XFCN (dgetrf, DGETRF, (nr, nr, tmp_data, nr, pipvt, info));

          // Throw-away extra info LAPACK gives so as to not change output.
          rcon = 0.0;
          if (info != 0)
            {
              info = -1;
              retval = DET ();
            }
          else
            {
              if (calc_cond)
                {
                  // Now calc the condition number for non-singular matrix.
                  char job = '1';
                  Array<double> z (dim_vector (4 * nc, 1));
                  double *pz = z.fortran_vec ();
                  Array<octave_idx_type> iz (dim_vector (nc, 1));
                  octave_idx_type *piz = iz.fortran_vec ();

                  F77_XFCN (dgecon, DGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, tmp_data, nr, anorm,
                                             rcon, pz, piz, info
                                             F77_CHAR_ARG_LEN (1)));
                }

              if (info != 0)
                {
                  info = -1;
                  retval = DET ();
                }
              else
                {
                  for (octave_idx_type i = 0; i < nc; i++)
                    {
                      double c = atmp(i,i);
                      retval *= (ipvt(i) != (i+1)) ? -c : c;
                    }
                }
            }
        }
    }

  return retval;
}

double
Matrix::rcond (void) const
{
  MatrixType mattype (*this);
  return rcond (mattype);
}

double
Matrix::rcond (MatrixType &mattype) const
{
  double rcon = octave_NaN;
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc)
    (*current_liboctave_error_handler) ("matrix must be square");
  else if (nr == 0 || nc == 0)
    rcon = octave_Inf;
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Unknown)
        typ = mattype.type (*this);

      // Only calculate the condition number for LU/Cholesky
      if (typ == MatrixType::Upper)
        {
          const double *tmp_data = fortran_vec ();
          octave_idx_type info = 0;
          char norm = '1';
          char uplo = 'U';
          char dia = 'N';

          Array<double> z (dim_vector (3 * nc, 1));
          double *pz = z.fortran_vec ();
          Array<octave_idx_type> iz (dim_vector (nc, 1));
          octave_idx_type *piz = iz.fortran_vec ();

          F77_XFCN (dtrcon, DTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, tmp_data, nr, rcon,
                                     pz, piz, info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          if (info != 0)
            rcon = 0.0;
        }
      else if  (typ == MatrixType::Permuted_Upper)
        (*current_liboctave_error_handler)
          ("permuted triangular matrix not implemented");
      else if (typ == MatrixType::Lower)
        {
          const double *tmp_data = fortran_vec ();
          octave_idx_type info = 0;
          char norm = '1';
          char uplo = 'L';
          char dia = 'N';

          Array<double> z (dim_vector (3 * nc, 1));
          double *pz = z.fortran_vec ();
          Array<octave_idx_type> iz (dim_vector (nc, 1));
          octave_idx_type *piz = iz.fortran_vec ();

          F77_XFCN (dtrcon, DTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, tmp_data, nr, rcon,
                                     pz, piz, info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          if (info != 0)
            rcon = 0.0;
        }
      else if (typ == MatrixType::Permuted_Lower)
        (*current_liboctave_error_handler)
          ("permuted triangular matrix not implemented");
      else if (typ == MatrixType::Full || typ == MatrixType::Hermitian)
        {
          double anorm = -1.0;

          if (typ == MatrixType::Hermitian)
            {
              octave_idx_type info = 0;
              char job = 'L';

              Matrix atmp = *this;
              double *tmp_data = atmp.fortran_vec ();

              anorm = atmp.abs().sum().
                      row(static_cast<octave_idx_type>(0)).max();

              F77_XFCN (dpotrf, DPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                         tmp_data, nr, info
                                         F77_CHAR_ARG_LEN (1)));

              if (info != 0)
                {
                  rcon = 0.0;
                  mattype.mark_as_unsymmetric ();
                  typ = MatrixType::Full;
                }
              else
                {
                  Array<double> z (dim_vector (3 * nc, 1));
                  double *pz = z.fortran_vec ();
                  Array<octave_idx_type> iz (dim_vector (nc, 1));
                  octave_idx_type *piz = iz.fortran_vec ();

                  F77_XFCN (dpocon, DPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, tmp_data, nr, anorm,
                                             rcon, pz, piz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    rcon = 0.0;
                }
            }

          if (typ == MatrixType::Full)
            {
              octave_idx_type info = 0;

              Matrix atmp = *this;
              double *tmp_data = atmp.fortran_vec ();

              Array<octave_idx_type> ipvt (dim_vector (nr, 1));
              octave_idx_type *pipvt = ipvt.fortran_vec ();

              if (anorm < 0.)
                anorm = atmp.abs ().sum ().
                        row(static_cast<octave_idx_type>(0)).max ();

              Array<double> z (dim_vector (4 * nc, 1));
              double *pz = z.fortran_vec ();
              Array<octave_idx_type> iz (dim_vector (nc, 1));
              octave_idx_type *piz = iz.fortran_vec ();

              F77_XFCN (dgetrf, DGETRF, (nr, nr, tmp_data, nr, pipvt, info));

              if (info != 0)
                {
                  rcon = 0.0;
                  mattype.mark_as_rectangular ();
                }
              else
                {
                  char job = '1';
                  F77_XFCN (dgecon, DGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, tmp_data, nr, anorm,
                                             rcon, pz, piz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    rcon = 0.0;
                }
            }
        }
      else
        rcon = 0.0;
    }

  return rcon;
}

Matrix
Matrix::utsolve (MatrixType &mattype, const Matrix& b, octave_idx_type& info,
                 double& rcon, solve_singularity_handler sing_handler,
                 bool calc_cond, blas_trans_type transt) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (nr == 0 || nc == 0 || b.cols () == 0)
    retval = Matrix (nc, b.cols (), 0.0);
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Permuted_Upper || typ == MatrixType::Upper)
        {
          octave_idx_type b_nc = b.cols ();
          rcon = 1.;
          info = 0;

          if (typ == MatrixType::Permuted_Upper)
            {
              (*current_liboctave_error_handler)
                ("permuted triangular matrix not implemented");
            }
          else
            {
              const double *tmp_data = fortran_vec ();

              retval = b;
              double *result = retval.fortran_vec ();

              char uplo = 'U';
              char trans = get_blas_char (transt);
              char dia = 'N';

              F77_XFCN (dtrtrs, DTRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                         F77_CONST_CHAR_ARG2 (&trans, 1),
                                         F77_CONST_CHAR_ARG2 (&dia, 1),
                                         nr, b_nc, tmp_data, nr,
                                         result, nr, info
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)));

              if (calc_cond)
                {
                  char norm = '1';
                  uplo = 'U';
                  dia = 'N';

                  Array<double> z (dim_vector (3 * nc, 1));
                  double *pz = z.fortran_vec ();
                  Array<octave_idx_type> iz (dim_vector (nc, 1));
                  octave_idx_type *piz = iz.fortran_vec ();

                  F77_XFCN (dtrcon, DTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                             F77_CONST_CHAR_ARG2 (&uplo, 1),
                                             F77_CONST_CHAR_ARG2 (&dia, 1),
                                             nr, tmp_data, nr, rcon,
                                             pz, piz, info
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    info = -2;

                  volatile double rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || xisnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        gripe_singular_matrix (rcon);
                    }
                }
            }
        }
      else
        (*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
Matrix::ltsolve (MatrixType &mattype, const Matrix& b, octave_idx_type& info,
                 double& rcon, solve_singularity_handler sing_handler,
                 bool calc_cond, blas_trans_type transt) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (nr == 0 || nc == 0 || b.cols () == 0)
    retval = Matrix (nc, b.cols (), 0.0);
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Permuted_Lower || typ == MatrixType::Lower)
        {
          octave_idx_type b_nc = b.cols ();
          rcon = 1.;
          info = 0;

          if (typ == MatrixType::Permuted_Lower)
            {
              (*current_liboctave_error_handler)
                ("permuted triangular matrix not implemented");
            }
          else
            {
              const double *tmp_data = fortran_vec ();

              retval = b;
              double *result = retval.fortran_vec ();

              char uplo = 'L';
              char trans = get_blas_char (transt);
              char dia = 'N';

              F77_XFCN (dtrtrs, DTRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                         F77_CONST_CHAR_ARG2 (&trans, 1),
                                         F77_CONST_CHAR_ARG2 (&dia, 1),
                                         nr, b_nc, tmp_data, nr,
                                         result, nr, info
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)));

              if (calc_cond)
                {
                  char norm = '1';
                  uplo = 'L';
                  dia = 'N';

                  Array<double> z (dim_vector (3 * nc, 1));
                  double *pz = z.fortran_vec ();
                  Array<octave_idx_type> iz (dim_vector (nc, 1));
                  octave_idx_type *piz = iz.fortran_vec ();

                  F77_XFCN (dtrcon, DTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                             F77_CONST_CHAR_ARG2 (&uplo, 1),
                                             F77_CONST_CHAR_ARG2 (&dia, 1),
                                             nr, tmp_data, nr, rcon,
                                             pz, piz, info
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    info = -2;

                  volatile double rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || xisnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        gripe_singular_matrix (rcon);
                    }
                }
            }
        }
      else
        (*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
Matrix::fsolve (MatrixType &mattype, const Matrix& b, octave_idx_type& info,
                double& rcon, solve_singularity_handler sing_handler,
                bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (nr == 0 || b.cols () == 0)
    retval = Matrix (nc, b.cols (), 0.0);
  else
    {
      volatile int typ = mattype.type ();

      // Calculate the norm of the matrix, for later use.
      double anorm = -1.;

      if (typ == MatrixType::Hermitian)
        {
          info = 0;
          char job = 'L';

          Matrix atmp = *this;
          double *tmp_data = atmp.fortran_vec ();

          anorm = atmp.abs().sum().row(static_cast<octave_idx_type>(0)).max();

          F77_XFCN (dpotrf, DPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                     tmp_data, nr, info
                                     F77_CHAR_ARG_LEN (1)));

          // Throw-away extra info LAPACK gives so as to not change output.
          rcon = 0.0;
          if (info != 0)
            {
              info = -2;

              mattype.mark_as_unsymmetric ();
              typ = MatrixType::Full;
            }
          else
            {
              if (calc_cond)
                {
                  Array<double> z (dim_vector (3 * nc, 1));
                  double *pz = z.fortran_vec ();
                  Array<octave_idx_type> iz (dim_vector (nc, 1));
                  octave_idx_type *piz = iz.fortran_vec ();

                  F77_XFCN (dpocon, DPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, tmp_data, nr, anorm,
                                             rcon, pz, piz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    info = -2;

                  volatile double rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || xisnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        gripe_singular_matrix (rcon);
                    }
                }

              if (info == 0)
                {
                  retval = b;
                  double *result = retval.fortran_vec ();

                  octave_idx_type b_nc = b.cols ();

                  F77_XFCN (dpotrs, DPOTRS, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, b_nc, tmp_data, nr,
                                             result, b.rows (), info
                                             F77_CHAR_ARG_LEN (1)));
                }
              else
                {
                  mattype.mark_as_unsymmetric ();
                  typ = MatrixType::Full;
                }
            }
        }

      if (typ == MatrixType::Full)
        {
          info = 0;

          Array<octave_idx_type> ipvt (dim_vector (nr, 1));
          octave_idx_type *pipvt = ipvt.fortran_vec ();

          Matrix atmp = *this;
          double *tmp_data = atmp.fortran_vec ();

          if (anorm < 0.)
            anorm = atmp.abs().sum().row(static_cast<octave_idx_type>(0)).max();

          Array<double> z (dim_vector (4 * nc, 1));
          double *pz = z.fortran_vec ();
          Array<octave_idx_type> iz (dim_vector (nc, 1));
          octave_idx_type *piz = iz.fortran_vec ();

          F77_XFCN (dgetrf, DGETRF, (nr, nr, tmp_data, nr, pipvt, info));

          // Throw-away extra info LAPACK gives so as to not change output.
          rcon = 0.0;
          if (info != 0)
            {
              info = -2;

              if (sing_handler)
                sing_handler (rcon);
              else
                gripe_singular_matrix ();

              mattype.mark_as_rectangular ();
            }
          else
            {
              if (calc_cond)
                {
                  // Now calculate the condition number for
                  // non-singular matrix.
                  char job = '1';
                  F77_XFCN (dgecon, DGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, tmp_data, nr, anorm,
                                             rcon, pz, piz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    info = -2;

                  volatile double rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || xisnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        gripe_singular_matrix (rcon);
                    }
                }

              if (info == 0)
                {
                  retval = b;
                  double *result = retval.fortran_vec ();

                  octave_idx_type b_nc = b.cols ();

                  char job = 'N';
                  F77_XFCN (dgetrs, DGETRS, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, b_nc, tmp_data, nr,
                                             pipvt, result, b.rows (), info
                                             F77_CHAR_ARG_LEN (1)));
                }
              else
                mattype.mark_as_rectangular ();
            }
        }
      else if (typ != MatrixType::Hermitian)
        (*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
Matrix::solve (MatrixType &typ, const Matrix& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (typ, b, info, rcon, 0);
}

Matrix
Matrix::solve (MatrixType &typ, const Matrix& b, octave_idx_type& info) const
{
  double rcon;
  return solve (typ, b, info, rcon, 0);
}

Matrix
Matrix::solve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
               double& rcon) const
{
  return solve (typ, b, info, rcon, 0);
}

Matrix
Matrix::solve (MatrixType &mattype, const Matrix& b, octave_idx_type& info,
               double& rcon, solve_singularity_handler sing_handler,
               bool singular_fallback, blas_trans_type transt) const
{
  Matrix retval;
  int typ = mattype.type ();

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);

  // Only calculate the condition number for LU/Cholesky
  if (typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
    retval = utsolve (mattype, b, info, rcon, sing_handler, true, transt);
  else if (typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
    retval = ltsolve (mattype, b, info, rcon, sing_handler, true, transt);
  else if (transt == blas_trans || transt == blas_conj_trans)
    return transpose ().solve (mattype, b, info, rcon, sing_handler,
                               singular_fallback);
  else if (typ == MatrixType::Full || typ == MatrixType::Hermitian)
    retval = fsolve (mattype, b, info, rcon, sing_handler, true);
  else if (typ != MatrixType::Rectangular)
    {
      (*current_liboctave_error_handler) ("unknown matrix type");
      return Matrix ();
    }

  // Rectangular or one of the above solvers flags a singular matrix
  if (singular_fallback && mattype.type () == MatrixType::Rectangular)
    {
      octave_idx_type rank;
      retval = lssolve (b, info, rank, rcon);
    }

  return retval;
}

ComplexMatrix
Matrix::solve (MatrixType &typ, const ComplexMatrix& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (typ, b, info, rcon, 0);
}

ComplexMatrix
Matrix::solve (MatrixType &typ, const ComplexMatrix& b,
               octave_idx_type& info) const
{
  double rcon;
  return solve (typ, b, info, rcon, 0);
}

ComplexMatrix
Matrix::solve (MatrixType &typ, const ComplexMatrix& b, octave_idx_type& info,
               double& rcon) const
{
  return solve (typ, b, info, rcon, 0);
}

static Matrix
stack_complex_matrix (const ComplexMatrix& cm)
{
  octave_idx_type m = cm.rows ();
  octave_idx_type n = cm.cols ();
  octave_idx_type nel = m*n;
  Matrix retval (m, 2*n);
  const Complex *cmd = cm.data ();
  double *rd = retval.fortran_vec ();
  for (octave_idx_type i = 0; i < nel; i++)
    {
      rd[i] = std::real (cmd[i]);
      rd[nel+i] = std::imag (cmd[i]);
    }
  return retval;
}

static ComplexMatrix
unstack_complex_matrix (const Matrix& sm)
{
  octave_idx_type m = sm.rows ();
  octave_idx_type n = sm.cols () / 2;
  octave_idx_type nel = m*n;
  ComplexMatrix retval (m, n);
  const double *smd = sm.data ();
  Complex *rd = retval.fortran_vec ();
  for (octave_idx_type i = 0; i < nel; i++)
    rd[i] = Complex (smd[i], smd[nel+i]);
  return retval;
}

ComplexMatrix
Matrix::solve (MatrixType &typ, const ComplexMatrix& b, octave_idx_type& info,
               double& rcon, solve_singularity_handler sing_handler,
               bool singular_fallback, blas_trans_type transt) const
{
  Matrix tmp = stack_complex_matrix (b);
  tmp = solve (typ, tmp, info, rcon, sing_handler, singular_fallback, transt);
  return unstack_complex_matrix (tmp);
}

ColumnVector
Matrix::solve (MatrixType &typ, const ColumnVector& b) const
{
  octave_idx_type info; double rcon;
  return solve (typ, b, info, rcon);
}

ColumnVector
Matrix::solve (MatrixType &typ, const ColumnVector& b,
               octave_idx_type& info) const
{
  double rcon;
  return solve (typ, b, info, rcon);
}

ColumnVector
Matrix::solve (MatrixType &typ, const ColumnVector& b, octave_idx_type& info,
               double& rcon) const
{
  return solve (typ, b, info, rcon, 0);
}

ColumnVector
Matrix::solve (MatrixType &typ, const ColumnVector& b, octave_idx_type& info,
               double& rcon, solve_singularity_handler sing_handler,
               blas_trans_type transt) const
{
  Matrix tmp (b);
  tmp = solve (typ, tmp, info, rcon, sing_handler, true, transt);
  return tmp.column (static_cast<octave_idx_type> (0));
}

ComplexColumnVector
Matrix::solve (MatrixType &typ, const ComplexColumnVector& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b);
}

ComplexColumnVector
Matrix::solve (MatrixType &typ, const ComplexColumnVector& b,
               octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b, info);
}

ComplexColumnVector
Matrix::solve (MatrixType &typ, const ComplexColumnVector& b,
               octave_idx_type& info, double& rcon) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b, info, rcon);
}

ComplexColumnVector
Matrix::solve (MatrixType &typ, const ComplexColumnVector& b,
               octave_idx_type& info, double& rcon,
               solve_singularity_handler sing_handler,
               blas_trans_type transt) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b, info, rcon, sing_handler, transt);
}

Matrix
Matrix::solve (const Matrix& b) const
{
  octave_idx_type info;
  double rcon;
  return solve (b, info, rcon, 0);
}

Matrix
Matrix::solve (const Matrix& b, octave_idx_type& info) const
{
  double rcon;
  return solve (b, info, rcon, 0);
}

Matrix
Matrix::solve (const Matrix& b, octave_idx_type& info, double& rcon) const
{
  return solve (b, info, rcon, 0);
}

Matrix
Matrix::solve (const Matrix& b, octave_idx_type& info,
               double& rcon, solve_singularity_handler sing_handler,
               blas_trans_type transt) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcon, sing_handler, true, transt);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b, octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b, octave_idx_type& info,
               double& rcon) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcon);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b, octave_idx_type& info, double& rcon,
               solve_singularity_handler sing_handler,
               blas_trans_type transt) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcon, sing_handler, transt);
}

ColumnVector
Matrix::solve (const ColumnVector& b) const
{
  octave_idx_type info; double rcon;
  return solve (b, info, rcon);
}

ColumnVector
Matrix::solve (const ColumnVector& b, octave_idx_type& info) const
{
  double rcon;
  return solve (b, info, rcon);
}

ColumnVector
Matrix::solve (const ColumnVector& b, octave_idx_type& info, double& rcon) const
{
  return solve (b, info, rcon, 0);
}

ColumnVector
Matrix::solve (const ColumnVector& b, octave_idx_type& info, double& rcon,
               solve_singularity_handler sing_handler,
               blas_trans_type transt) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcon, sing_handler, transt);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b, octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b, octave_idx_type& info,
               double& rcon) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcon);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b, octave_idx_type& info,
               double& rcon,
               solve_singularity_handler sing_handler,
               blas_trans_type transt) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcon, sing_handler, transt);
}

Matrix
Matrix::lssolve (const Matrix& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  double rcon;
  return lssolve (b, info, rank, rcon);
}

Matrix
Matrix::lssolve (const Matrix& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  double rcon;
  return lssolve (b, info, rank, rcon);
}

Matrix
Matrix::lssolve (const Matrix& b, octave_idx_type& info,
                 octave_idx_type& rank) const
{
  double rcon;
  return lssolve (b, info, rank, rcon);
}

Matrix
Matrix::lssolve (const Matrix& b, octave_idx_type& info,
                 octave_idx_type& rank, double &rcon) const
{
  Matrix retval;

  octave_idx_type nrhs = b.cols ();

  octave_idx_type m = rows ();
  octave_idx_type n = cols ();

  if (m != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (m == 0 || n == 0 || b.cols () == 0)
    retval = Matrix (n, b.cols (), 0.0);
  else
    {
      volatile octave_idx_type minmn = (m < n ? m : n);
      octave_idx_type maxmn = m > n ? m : n;
      rcon = -1.0;
      if (m != n)
        {
          retval = Matrix (maxmn, nrhs, 0.0);

          for (octave_idx_type j = 0; j < nrhs; j++)
            for (octave_idx_type i = 0; i < m; i++)
              retval.elem (i, j) = b.elem (i, j);
        }
      else
        retval = b;

      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      double *pretval = retval.fortran_vec ();
      Array<double> s (dim_vector (minmn, 1));
      double *ps = s.fortran_vec ();

      // Ask DGELSD what the dimension of WORK should be.
      octave_idx_type lwork = -1;

      Array<double> work (dim_vector (1, 1));

      octave_idx_type smlsiz;
      F77_FUNC (xilaenv, XILAENV) (9, F77_CONST_CHAR_ARG2 ("DGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   0, 0, 0, 0, smlsiz
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      octave_idx_type mnthr;
      F77_FUNC (xilaenv, XILAENV) (6, F77_CONST_CHAR_ARG2 ("DGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   m, n, nrhs, -1, mnthr
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      // We compute the size of iwork because DGELSD in older versions
      // of LAPACK does not return it on a query call.
      double dminmn = static_cast<double> (minmn);
      double dsmlsizp1 = static_cast<double> (smlsiz+1);
      double tmp = xlog2 (dminmn / dsmlsizp1);

      octave_idx_type nlvl = static_cast<octave_idx_type> (tmp) + 1;
      if (nlvl < 0)
        nlvl = 0;

      octave_idx_type liwork = 3 * minmn * nlvl + 11 * minmn;
      if (liwork < 1)
        liwork = 1;
      Array<octave_idx_type> iwork (dim_vector (liwork, 1));
      octave_idx_type* piwork = iwork.fortran_vec ();

      F77_XFCN (dgelsd, DGELSD, (m, n, nrhs, tmp_data, m, pretval, maxmn,
                                 ps, rcon, rank, work.fortran_vec (),
                                 lwork, piwork, info));

      // The workspace query is broken in at least LAPACK 3.0.0
      // through 3.1.1 when n >= mnthr.  The obtuse formula below
      // should provide sufficient workspace for DGELSD to operate
      // efficiently.
      if (n > m && n >= mnthr)
        {
          const octave_idx_type wlalsd
            = 9*m + 2*m*smlsiz + 8*m*nlvl + m*nrhs + (smlsiz+1)*(smlsiz+1);

          octave_idx_type addend = m;

          if (2*m-4 > addend)
            addend = 2*m-4;

          if (nrhs > addend)
            addend = nrhs;

          if (n-3*m > addend)
            addend = n-3*m;

          if (wlalsd > addend)
            addend = wlalsd;

          const octave_idx_type lworkaround = 4*m + m*m + addend;

          if (work(0) < lworkaround)
            work(0) = lworkaround;
        }
      else if (m >= n)
        {
          octave_idx_type lworkaround
            = 12*n + 2*n*smlsiz + 8*n*nlvl + n*nrhs + (smlsiz+1)*(smlsiz+1);

          if (work(0) < lworkaround)
            work(0) = lworkaround;
        }

      lwork = static_cast<octave_idx_type> (work(0));
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (dgelsd, DGELSD, (m, n, nrhs, tmp_data, m, pretval,
                                 maxmn, ps, rcon, rank,
                                 work.fortran_vec (), lwork,
                                 piwork, info));

      if (s.elem (0) == 0.0)
        rcon = 0.0;
      else
        rcon = s.elem (minmn - 1) / s.elem (0);

      retval.resize (n, nrhs);
    }

  return retval;
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b) const
{
  ComplexMatrix tmp (*this);
  octave_idx_type info;
  octave_idx_type rank;
  double rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b, octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  octave_idx_type rank;
  double rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b, octave_idx_type& info,
                 octave_idx_type& rank) const
{
  ComplexMatrix tmp (*this);
  double rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b, octave_idx_type& info,
                 octave_idx_type& rank, double& rcon) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b, info, rank, rcon);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  double rcon;
  return lssolve (b, info, rank, rcon);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  double rcon;
  return lssolve (b, info, rank, rcon);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b, octave_idx_type& info,
                 octave_idx_type& rank) const
{
  double rcon;
  return lssolve (b, info, rank, rcon);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b, octave_idx_type& info,
                 octave_idx_type& rank, double &rcon) const
{
  ColumnVector retval;

  octave_idx_type nrhs = 1;

  octave_idx_type m = rows ();
  octave_idx_type n = cols ();

  if (m != b.length ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (m == 0 || n == 0)
    retval = ColumnVector (n, 0.0);
  else
    {
      volatile octave_idx_type minmn = (m < n ? m : n);
      octave_idx_type maxmn = m > n ? m : n;
      rcon = -1.0;

      if (m != n)
        {
          retval = ColumnVector (maxmn, 0.0);

          for (octave_idx_type i = 0; i < m; i++)
            retval.elem (i) = b.elem (i);
        }
      else
        retval = b;

      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      double *pretval = retval.fortran_vec ();
      Array<double> s (dim_vector (minmn, 1));
      double *ps = s.fortran_vec ();

      // Ask DGELSD what the dimension of WORK should be.
      octave_idx_type lwork = -1;

      Array<double> work (dim_vector (1, 1));

      octave_idx_type smlsiz;
      F77_FUNC (xilaenv, XILAENV) (9, F77_CONST_CHAR_ARG2 ("DGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   0, 0, 0, 0, smlsiz
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      // We compute the size of iwork because DGELSD in older versions
      // of LAPACK does not return it on a query call.
      double dminmn = static_cast<double> (minmn);
      double dsmlsizp1 = static_cast<double> (smlsiz+1);
      double tmp = xlog2 (dminmn / dsmlsizp1);

      octave_idx_type nlvl = static_cast<octave_idx_type> (tmp) + 1;
      if (nlvl < 0)
        nlvl = 0;

      octave_idx_type liwork = 3 * minmn * nlvl + 11 * minmn;
      if (liwork < 1)
        liwork = 1;
      Array<octave_idx_type> iwork (dim_vector (liwork, 1));
      octave_idx_type* piwork = iwork.fortran_vec ();

      F77_XFCN (dgelsd, DGELSD, (m, n, nrhs, tmp_data, m, pretval, maxmn,
                                 ps, rcon, rank, work.fortran_vec (),
                                 lwork, piwork, info));

      lwork = static_cast<octave_idx_type> (work(0));
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (dgelsd, DGELSD, (m, n, nrhs, tmp_data, m, pretval,
                                 maxmn, ps, rcon, rank,
                                 work.fortran_vec (), lwork,
                                 piwork, info));

      if (rank < minmn)
        {
          if (s.elem (0) == 0.0)
            rcon = 0.0;
          else
            rcon = s.elem (minmn - 1) / s.elem (0);
        }

      retval.resize (n, nrhs);
    }

  return retval;
}

ComplexColumnVector
Matrix::lssolve (const ComplexColumnVector& b) const
{
  ComplexMatrix tmp (*this);
  octave_idx_type info;
  octave_idx_type rank;
  double rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

ComplexColumnVector
Matrix::lssolve (const ComplexColumnVector& b, octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  octave_idx_type rank;
  double rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

ComplexColumnVector
Matrix::lssolve (const ComplexColumnVector& b, octave_idx_type& info,
                 octave_idx_type& rank) const
{
  ComplexMatrix tmp (*this);
  double rcon;
  return tmp.lssolve (b, info, rank, rcon);
}

ComplexColumnVector
Matrix::lssolve (const ComplexColumnVector& b, octave_idx_type& info,
                 octave_idx_type& rank, double &rcon) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b, info, rank, rcon);
}

Matrix&
Matrix::operator += (const DiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator +=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) += a.elem (i, i);

  return *this;
}

Matrix&
Matrix::operator -= (const DiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator -=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
}

// unary operations

// column vector by row vector -> matrix operations

Matrix
operator * (const ColumnVector& v, const RowVector& a)
{
  Matrix retval;

  octave_idx_type len = v.length ();

  if (len != 0)
    {
      octave_idx_type a_len = a.length ();

      retval = Matrix (len, a_len);
      double *c = retval.fortran_vec ();

      F77_XFCN (dgemm, DGEMM, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 ("N", 1),
                               len, a_len, 1, 1.0, v.data (), len,
                               a.data (), 1, 0.0, c, len
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));
    }

  return retval;
}

// other operations.

// FIXME: Do these really belong here?  Maybe they should be in a base class?

boolMatrix
Matrix::all (int dim) const
{
  return NDArray::all (dim);
}

boolMatrix
Matrix::any (int dim) const
{
  return NDArray::any (dim);
}

Matrix
Matrix::cumprod (int dim) const
{
  return NDArray::cumprod (dim);
}

Matrix
Matrix::cumsum (int dim) const
{
  return NDArray::cumsum (dim);
}

Matrix
Matrix::prod (int dim) const
{
  return NDArray::prod (dim);
}

Matrix
Matrix::sum (int dim) const
{
  return NDArray::sum (dim);
}

Matrix
Matrix::sumsq (int dim) const
{
  return NDArray::sumsq (dim);
}

Matrix
Matrix::abs (void) const
{
  return NDArray::abs ();
}

Matrix
Matrix::diag (octave_idx_type k) const
{
  return NDArray::diag (k);
}

DiagMatrix
Matrix::diag (octave_idx_type m, octave_idx_type n) const
{
  DiagMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr == 1 || nc == 1)
    retval = DiagMatrix (*this, m, n);
  else
    (*current_liboctave_error_handler)
      ("diag: expecting vector argument");

  return retval;
}

ColumnVector
Matrix::row_min (void) const
{
  Array<octave_idx_type> dummy_idx;
  return row_min (dummy_idx);
}

ColumnVector
Matrix::row_min (Array<octave_idx_type>& idx_arg) const
{
  ColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (dim_vector (nr, 1));

      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_idx_type idx_j;

          double tmp_min = octave_NaN;

          for (idx_j = 0; idx_j < nc; idx_j++)
            {
              tmp_min = elem (i, idx_j);

              if (! xisnan (tmp_min))
                break;
            }

          for (octave_idx_type j = idx_j+1; j < nc; j++)
            {
              double tmp = elem (i, j);

              if (xisnan (tmp))
                continue;
              else if (tmp < tmp_min)
                {
                  idx_j = j;
                  tmp_min = tmp;
                }
            }

          result.elem (i) = tmp_min;
          idx_arg.elem (i) = xisnan (tmp_min) ? 0 : idx_j;
        }
    }

  return result;
}

ColumnVector
Matrix::row_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return row_max (dummy_idx);
}

ColumnVector
Matrix::row_max (Array<octave_idx_type>& idx_arg) const
{
  ColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (dim_vector (nr, 1));

      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_idx_type idx_j;

          double tmp_max = octave_NaN;

          for (idx_j = 0; idx_j < nc; idx_j++)
            {
              tmp_max = elem (i, idx_j);

              if (! xisnan (tmp_max))
                break;
            }

          for (octave_idx_type j = idx_j+1; j < nc; j++)
            {
              double tmp = elem (i, j);

              if (xisnan (tmp))
                continue;
              else if (tmp > tmp_max)
                {
                  idx_j = j;
                  tmp_max = tmp;
                }
            }

          result.elem (i) = tmp_max;
          idx_arg.elem (i) = xisnan (tmp_max) ? 0 : idx_j;
        }
    }

  return result;
}

RowVector
Matrix::column_min (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_min (dummy_idx);
}

RowVector
Matrix::column_min (Array<octave_idx_type>& idx_arg) const
{
  RowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (dim_vector (1, nc));

      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type idx_i;

          double tmp_min = octave_NaN;

          for (idx_i = 0; idx_i < nr; idx_i++)
            {
              tmp_min = elem (idx_i, j);

              if (! xisnan (tmp_min))
                break;
            }

          for (octave_idx_type i = idx_i+1; i < nr; i++)
            {
              double tmp = elem (i, j);

              if (xisnan (tmp))
                continue;
              else if (tmp < tmp_min)
                {
                  idx_i = i;
                  tmp_min = tmp;
                }
            }

          result.elem (j) = tmp_min;
          idx_arg.elem (j) = xisnan (tmp_min) ? 0 : idx_i;
        }
    }

  return result;
}

RowVector
Matrix::column_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_max (dummy_idx);
}

RowVector
Matrix::column_max (Array<octave_idx_type>& idx_arg) const
{
  RowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (dim_vector (1, nc));

      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type idx_i;

          double tmp_max = octave_NaN;

          for (idx_i = 0; idx_i < nr; idx_i++)
            {
              tmp_max = elem (idx_i, j);

              if (! xisnan (tmp_max))
                break;
            }

          for (octave_idx_type i = idx_i+1; i < nr; i++)
            {
              double tmp = elem (i, j);

              if (xisnan (tmp))
                continue;
              else if (tmp > tmp_max)
                {
                  idx_i = i;
                  tmp_max = tmp;
                }
            }

          result.elem (j) = tmp_max;
          idx_arg.elem (j) = xisnan (tmp_max) ? 0 : idx_i;
        }
    }

  return result;
}

std::ostream&
operator << (std::ostream& os, const Matrix& a)
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    {
      for (octave_idx_type j = 0; j < a.cols (); j++)
        {
          os << " ";
          octave_write_double (os, a.elem (i, j));
        }
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, Matrix& a)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr > 0 && nc > 0)
    {
      double tmp;
      for (octave_idx_type i = 0; i < nr; i++)
        for (octave_idx_type j = 0; j < nc; j++)
          {
            tmp = octave_read_value<double> (is);
            if (is)
              a.elem (i, j) = tmp;
            else
              goto done;
          }
    }

done:

  return is;
}

Matrix
Givens (double x, double y)
{
  double cc, s, temp_r;

  F77_FUNC (dlartg, DLARTG) (x, y, cc, s, temp_r);

  Matrix g (2, 2);

  g.elem (0, 0) = cc;
  g.elem (1, 1) = cc;
  g.elem (0, 1) = s;
  g.elem (1, 0) = -s;

  return g;
}

Matrix
Sylvester (const Matrix& a, const Matrix& b, const Matrix& c)
{
  Matrix retval;

  // FIXME: need to check that a, b, and c are all the same size.

  // Compute Schur decompositions.

  SCHUR as (a, "U");
  SCHUR bs (b, "U");

  // Transform c to new coordinates.

  Matrix ua = as.unitary_matrix ();
  Matrix sch_a = as.schur_matrix ();

  Matrix ub = bs.unitary_matrix ();
  Matrix sch_b = bs.schur_matrix ();

  Matrix cx = ua.transpose () * c * ub;

  // Solve the sylvester equation, back-transform, and return the solution.

  octave_idx_type a_nr = a.rows ();
  octave_idx_type b_nr = b.rows ();

  double scale;
  octave_idx_type info;

  double *pa = sch_a.fortran_vec ();
  double *pb = sch_b.fortran_vec ();
  double *px = cx.fortran_vec ();

  F77_XFCN (dtrsyl, DTRSYL, (F77_CONST_CHAR_ARG2 ("N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             1, a_nr, b_nr, pa, a_nr, pb,
                             b_nr, px, a_nr, scale, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));


  // FIXME: check info?

  retval = ua*cx*ub.transpose ();

  return retval;
}

// matrix by matrix -> matrix operations

/*

## Simple Dot Product, Matrix-Vector and Matrix-Matrix Unit tests
%!assert ([1 2 3] * [ 4 ; 5 ; 6], 32, 1e-14)
%!assert ([1 2 ; 3 4 ] * [5 ; 6], [17 ; 39 ], 1e-14)
%!assert ([1 2 ; 3 4 ] * [5 6 ; 7 8], [19 22; 43 50], 1e-14)

## Test some simple identities
%!shared M, cv, rv, Mt, rvt
%! M = randn (10,10) + 100*eye (10,10);
%! Mt = M';
%! cv = randn (10,1);
%! rv = randn (1,10);
%! rvt = rv';
%!assert ([M*cv,M*cv], M*[cv,cv], 1e-13)
%!assert ([M'*cv,M'*cv], M'*[cv,cv], 1e-13)
%!assert ([rv*M;rv*M], [rv;rv]*M, 1e-13)
%!assert ([rv*M';rv*M'], [rv;rv]*M', 1e-13)
%!assert (2*rv*cv, [rv,rv]*[cv;cv], 1e-13)
%!assert (M'\cv, Mt\cv, 1e-14)
%!assert (M'\rv', Mt\rvt, 1e-14)

*/

static inline char
get_blas_trans_arg (bool trans)
{
  return trans ? 'T' : 'N';
}

// the general GEMM operation

Matrix
xgemm (const Matrix& a, const Matrix& b,
       blas_trans_type transa, blas_trans_type transb)
{
  Matrix retval;

  bool tra = transa != blas_no_trans;
  bool trb = transb != blas_no_trans;

  octave_idx_type a_nr = tra ? a.cols () : a.rows ();
  octave_idx_type a_nc = tra ? a.rows () : a.cols ();

  octave_idx_type b_nr = trb ? b.cols () : b.rows ();
  octave_idx_type b_nc = trb ? b.rows () : b.cols ();

  if (a_nc != b_nr)
    gripe_nonconformant ("operator *", a_nr, a_nc, b_nr, b_nc);
  else
    {
      if (a_nr == 0 || a_nc == 0 || b_nc == 0)
        retval = Matrix (a_nr, b_nc, 0.0);
      else if (a.data () == b.data () && a_nr == b_nc && tra != trb)
        {
          octave_idx_type lda = a.rows ();

          retval = Matrix (a_nr, b_nc);
          double *c = retval.fortran_vec ();

          const char ctra = get_blas_trans_arg (tra);
          F77_XFCN (dsyrk, DSYRK, (F77_CONST_CHAR_ARG2 ("U", 1),
                                   F77_CONST_CHAR_ARG2 (&ctra, 1),
                                   a_nr, a_nc, 1.0,
                                   a.data (), lda, 0.0, c, a_nr
                                   F77_CHAR_ARG_LEN (1)
                                   F77_CHAR_ARG_LEN (1)));
          for (int j = 0; j < a_nr; j++)
            for (int i = 0; i < j; i++)
              retval.xelem (j,i) = retval.xelem (i,j);

        }
      else
        {
          octave_idx_type lda = a.rows ();
          octave_idx_type tda = a.cols ();
          octave_idx_type ldb = b.rows ();
          octave_idx_type tdb = b.cols ();

          retval = Matrix (a_nr, b_nc);
          double *c = retval.fortran_vec ();

          if (b_nc == 1)
            {
              if (a_nr == 1)
                F77_FUNC (xddot, XDDOT) (a_nc, a.data (), 1, b.data (), 1, *c);
              else
                {
                  const char ctra = get_blas_trans_arg (tra);
                  F77_XFCN (dgemv, DGEMV, (F77_CONST_CHAR_ARG2 (&ctra, 1),
                                           lda, tda, 1.0,  a.data (), lda,
                                           b.data (), 1, 0.0, c, 1
                                           F77_CHAR_ARG_LEN (1)));
                }
            }
          else if (a_nr == 1)
            {
              const char crevtrb = get_blas_trans_arg (! trb);
              F77_XFCN (dgemv, DGEMV, (F77_CONST_CHAR_ARG2 (&crevtrb, 1),
                                       ldb, tdb, 1.0,  b.data (), ldb,
                                       a.data (), 1, 0.0, c, 1
                                       F77_CHAR_ARG_LEN (1)));
            }
          else
            {
              const char ctra = get_blas_trans_arg (tra);
              const char ctrb = get_blas_trans_arg (trb);
              F77_XFCN (dgemm, DGEMM, (F77_CONST_CHAR_ARG2 (&ctra, 1),
                                       F77_CONST_CHAR_ARG2 (&ctrb, 1),
                                       a_nr, b_nc, a_nc, 1.0, a.data (),
                                       lda, b.data (), ldb, 0.0, c, a_nr
                                       F77_CHAR_ARG_LEN (1)
                                       F77_CHAR_ARG_LEN (1)));
            }
        }
    }

  return retval;
}

Matrix
operator * (const Matrix& a, const Matrix& b)
{
  return xgemm (a, b);
}

// FIXME: it would be nice to share code among the min/max functions below.

#define EMPTY_RETURN_CHECK(T) \
  if (nr == 0 || nc == 0) \
    return T (nr, nc);

Matrix
min (double d, const Matrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmin (d, m(i, j));
      }

  return result;
}

Matrix
min (const Matrix& m, double d)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmin (m(i, j), d);
      }

  return result;
}

Matrix
min (const Matrix& a, const Matrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    {
      (*current_liboctave_error_handler)
        ("two-arg min expecting args of same size");
      return Matrix ();
    }

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmin (a(i, j), b(i, j));
      }

  return result;
}

Matrix
max (double d, const Matrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmax (d, m(i, j));
      }

  return result;
}

Matrix
max (const Matrix& m, double d)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmax (m(i, j), d);
      }

  return result;
}

Matrix
max (const Matrix& a, const Matrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    {
      (*current_liboctave_error_handler)
        ("two-arg max expecting args of same size");
      return Matrix ();
    }

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmax (a(i, j), b(i, j));
      }

  return result;
}

Matrix linspace (const ColumnVector& x1,
                 const ColumnVector& x2,
                 octave_idx_type n)

{
  if (n < 1) n = 1;

  octave_idx_type m = x1.length ();

  if (x2.length () != m)
    (*current_liboctave_error_handler)
      ("linspace: vectors must be of equal length");

  NoAlias<Matrix> retval;

  retval.clear (m, n);
  for (octave_idx_type i = 0; i < m; i++)
    retval(i, 0) = x1(i);

  // The last column is not needed while using delta.
  double *delta = &retval(0, n-1);
  for (octave_idx_type i = 0; i < m; i++)
    delta[i] = (x2(i) - x1(i)) / (n - 1);

  for (octave_idx_type j = 1; j < n-1; j++)
    for (octave_idx_type i = 0; i < m; i++)
      retval(i, j) = x1(i) + j*delta[i];

  for (octave_idx_type i = 0; i < m; i++)
    retval(i, n-1) = x2(i);

  return retval;
}

MS_CMP_OPS (Matrix, double)
MS_BOOL_OPS (Matrix, double)

SM_CMP_OPS (double, Matrix)
SM_BOOL_OPS (double, Matrix)

MM_CMP_OPS (Matrix, Matrix)
MM_BOOL_OPS (Matrix, Matrix)
