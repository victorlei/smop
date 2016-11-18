/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek

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

#include <vector>

#include "fMatrix.h"
#include "fRowVector.h"
#include "fCmplxCHOL.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "oct-locbuf.h"
#include "oct-norm.h"
#ifndef HAVE_QRUPDATE
#include "dbleQR.h"
#endif

extern "C"
{
  F77_RET_T
  F77_FUNC (cpotrf, CPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (cpotri, CPOTRI) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cpocon, CPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, const float&,
                             float&, FloatComplex*, float*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);
#ifdef HAVE_QRUPDATE

  F77_RET_T
  F77_FUNC (cch1up, CCH1UP) (const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*, float*);

  F77_RET_T
  F77_FUNC (cch1dn, CCH1DN) (const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             float*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (cchinx, CCHINX) (const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, const octave_idx_type&,
                             FloatComplex*, float*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (cchdex, CCHDEX) (const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, const octave_idx_type&,
                             float*);

  F77_RET_T
  F77_FUNC (cchshx, CCHSHX) (const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*, float*);
#endif
}

octave_idx_type
FloatComplexCHOL::init (const FloatComplexMatrix& a, bool calc_cond)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler)
        ("FloatComplexCHOL requires square matrix");
      return -1;
    }

  octave_idx_type n = a_nc;
  octave_idx_type info;

  chol_mat.clear (n, n);
  for (octave_idx_type j = 0; j < n; j++)
    {
      for (octave_idx_type i = 0; i <= j; i++)
        chol_mat.xelem (i, j) = a(i, j);
      for (octave_idx_type i = j+1; i < n; i++)
        chol_mat.xelem (i, j) = 0.0f;
    }
  FloatComplex *h = chol_mat.fortran_vec ();

  // Calculate the norm of the matrix, for later use.
  float anorm = 0;
  if (calc_cond)
    anorm = xnorm (a, 1);

  F77_XFCN (cpotrf, CPOTRF, (F77_CONST_CHAR_ARG2 ("U", 1), n, h, n, info
                             F77_CHAR_ARG_LEN (1)));

  xrcond = 0.0;
  if (info > 0)
    chol_mat.resize (info - 1, info - 1);
  else if (calc_cond)
    {
      octave_idx_type cpocon_info = 0;

      // Now calculate the condition number for non-singular matrix.
      Array<FloatComplex> z (dim_vector (2*n, 1));
      FloatComplex *pz = z.fortran_vec ();
      Array<float> rz (dim_vector (n, 1));
      float *prz = rz.fortran_vec ();
      F77_XFCN (cpocon, CPOCON, (F77_CONST_CHAR_ARG2 ("U", 1), n, h,
                                 n, anorm, xrcond, pz, prz, cpocon_info
                                 F77_CHAR_ARG_LEN (1)));

      if (cpocon_info != 0)
        info = -1;
    }

  return info;
}

static FloatComplexMatrix
chol2inv_internal (const FloatComplexMatrix& r)
{
  FloatComplexMatrix retval;

  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();

  if (r_nr == r_nc)
    {
      octave_idx_type n = r_nc;
      octave_idx_type info;

      FloatComplexMatrix tmp = r;

      F77_XFCN (cpotri, CPOTRI, (F77_CONST_CHAR_ARG2 ("U", 1), n,
                                 tmp.fortran_vec (), n, info
                                 F77_CHAR_ARG_LEN (1)));

      // If someone thinks of a more graceful way of doing this (or
      // faster for that matter :-)), please let me know!

      if (n > 1)
        for (octave_idx_type j = 0; j < r_nc; j++)
          for (octave_idx_type i = j+1; i < r_nr; i++)
            tmp.xelem (i, j) = std::conj (tmp.xelem (j, i));

      retval = tmp;
    }
  else
    (*current_liboctave_error_handler) ("chol2inv requires square matrix");

  return retval;
}

// Compute the inverse of a matrix using the Cholesky factorization.
FloatComplexMatrix
FloatComplexCHOL::inverse (void) const
{
  return chol2inv_internal (chol_mat);
}

void
FloatComplexCHOL::set (const FloatComplexMatrix& R)
{
  if (R.is_square ())
    chol_mat = R;
  else
    (*current_liboctave_error_handler) ("CHOL requires square matrix");
}

#ifdef HAVE_QRUPDATE

void
FloatComplexCHOL::update (const FloatComplexColumnVector& u)
{
  octave_idx_type n = chol_mat.rows ();

  if (u.length () == n)
    {
      FloatComplexColumnVector utmp = u;

      OCTAVE_LOCAL_BUFFER (float, rw, n);

      F77_XFCN (cch1up, CCH1UP, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                                 utmp.fortran_vec (), rw));
    }
  else
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");
}

octave_idx_type
FloatComplexCHOL::downdate (const FloatComplexColumnVector& u)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.length () == n)
    {
      FloatComplexColumnVector utmp = u;

      OCTAVE_LOCAL_BUFFER (float, rw, n);

      F77_XFCN (cch1dn, CCH1DN, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                                 utmp.fortran_vec (), rw, info));
    }
  else
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  return info;
}

octave_idx_type
FloatComplexCHOL::insert_sym (const FloatComplexColumnVector& u,
                              octave_idx_type j)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.length () != n + 1)
    (*current_liboctave_error_handler) ("cholinsert: dimension mismatch");
  else if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("cholinsert: index out of range");
  else
    {
      FloatComplexColumnVector utmp = u;

      OCTAVE_LOCAL_BUFFER (float, rw, n);

      chol_mat.resize (n+1, n+1);

      F77_XFCN (cchinx, CCHINX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                                 j + 1, utmp.fortran_vec (), rw, info));
    }

  return info;
}

void
FloatComplexCHOL::delete_sym (octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("choldelete: index out of range");
  else
    {
      OCTAVE_LOCAL_BUFFER (float, rw, n);

      F77_XFCN (cchdex, CCHDEX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                                 j + 1, rw));

      chol_mat.resize (n-1, n-1);
    }
}

void
FloatComplexCHOL::shift_sym (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("cholshift: index out of range");
  else
    {
      OCTAVE_LOCAL_BUFFER (FloatComplex, w, n);
      OCTAVE_LOCAL_BUFFER (float, rw, n);

      F77_XFCN (cchshx, CCHSHX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                                 i + 1, j + 1, w, rw));
    }
}

#else

void
FloatComplexCHOL::update (const FloatComplexColumnVector& u)
{
  warn_qrupdate_once ();

  octave_idx_type n = chol_mat.rows ();

  if (u.length () == n)
    {
      init (chol_mat.hermitian () * chol_mat
            + FloatComplexMatrix (u) * FloatComplexMatrix (u).hermitian (),
            false);
    }
  else
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");
}

static bool
singular (const FloatComplexMatrix& a)
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    if (a(i,i) == 0.0f) return true;
  return false;
}

octave_idx_type
FloatComplexCHOL::downdate (const FloatComplexColumnVector& u)
{
  warn_qrupdate_once ();

  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.length () == n)
    {
      if (singular (chol_mat))
        info = 2;
      else
        {
          info = init (chol_mat.hermitian () * chol_mat
                       - FloatComplexMatrix (u)
                       * FloatComplexMatrix (u).hermitian (),
                       false);
          if (info) info = 1;
        }
    }
  else
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  return info;
}

octave_idx_type
FloatComplexCHOL::insert_sym (const FloatComplexColumnVector& u,
                              octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.length () != n + 1)
    (*current_liboctave_error_handler) ("cholinsert: dimension mismatch");
  else if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("cholinsert: index out of range");
  else
    {
      if (singular (chol_mat))
        info = 2;
      else if (u(j).imag () != 0.0)
        info = 3;
      else
        {
          FloatComplexMatrix a = chol_mat.hermitian () * chol_mat;
          FloatComplexMatrix a1 (n+1, n+1);
          for (octave_idx_type k = 0; k < n+1; k++)
            for (octave_idx_type l = 0; l < n+1; l++)
              {
                if (l == j)
                  a1(k, l) = u(k);
                else if (k == j)
                  a1(k, l) = std::conj (u(l));
                else
                  a1(k, l) = a(k < j ? k : k-1, l < j ? l : l-1);
              }
          info = init (a1, false);
          if (info) info = 1;
        }
    }

  return info;
}

void
FloatComplexCHOL::delete_sym (octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = chol_mat.rows ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("choldelete: index out of range");
  else
    {
      FloatComplexMatrix a = chol_mat.hermitian () * chol_mat;
      a.delete_elements (1, idx_vector (j));
      a.delete_elements (0, idx_vector (j));
      init (a, false);
    }
}

void
FloatComplexCHOL::shift_sym (octave_idx_type i, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = chol_mat.rows ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("cholshift: index out of range");
  else
    {
      FloatComplexMatrix a = chol_mat.hermitian () * chol_mat;
      Array<octave_idx_type> p (dim_vector (n, 1));
      for (octave_idx_type k = 0; k < n; k++) p(k) = k;
      if (i < j)
        {
          for (octave_idx_type k = i; k < j; k++) p(k) = k+1;
          p(j) = i;
        }
      else if (j < i)
        {
          p(j) = i;
          for (octave_idx_type k = j+1; k < i+1; k++) p(k) = k-1;
        }

      init (a.index (idx_vector (p), idx_vector (p)), false);
    }
}

#endif

FloatComplexMatrix
chol2inv (const FloatComplexMatrix& r)
{
  return chol2inv_internal (r);
}
