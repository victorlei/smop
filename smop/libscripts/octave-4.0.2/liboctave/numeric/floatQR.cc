/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek
Copyright (C) 2009 VZLU Prague

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

#include "floatQR.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "Range.h"
#include "idx-vector.h"
#include "oct-locbuf.h"

#include "base-qr.cc"

template class base_qr<FloatMatrix>;

extern "C"
{
  F77_RET_T
  F77_FUNC (sgeqrf, SGEQRF) (const octave_idx_type&, const octave_idx_type&,
                             float*, const octave_idx_type&, float*, float*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (sorgqr, SORGQR) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*, float*,
                             const octave_idx_type&, octave_idx_type&);

#ifdef HAVE_QRUPDATE

  F77_RET_T
  F77_FUNC (sqr1up, SQR1UP) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*, float*, float*);

  F77_RET_T
  F77_FUNC (sqrinc, SQRINC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&,
                             const octave_idx_type&, const float*, float*);

  F77_RET_T
  F77_FUNC (sqrdec, SQRDEC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&,
                             const octave_idx_type&, float*);

  F77_RET_T
  F77_FUNC (sqrinr, SQRINR) (const octave_idx_type&, const octave_idx_type&,
                             float*, const octave_idx_type&,
                             float*, const octave_idx_type&,
                             const octave_idx_type&, const float*, float*);

  F77_RET_T
  F77_FUNC (sqrder, SQRDER) (const octave_idx_type&, const octave_idx_type&,
                             float*, const octave_idx_type&,
                             float*, const octave_idx_type&,
                             const octave_idx_type&, float*);

  F77_RET_T
  F77_FUNC (sqrshc, SQRSHC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&,
                             const octave_idx_type&, const octave_idx_type&,
                             float*);

#endif
}

FloatQR::FloatQR (const FloatMatrix& a, qr_type_t qr_type)
{
  init (a, qr_type);
}

void
FloatQR::init (const FloatMatrix& a, qr_type_t qr_type)
{
  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  octave_idx_type min_mn = m < n ? m : n;
  OCTAVE_LOCAL_BUFFER (float, tau, min_mn);

  octave_idx_type info = 0;

  FloatMatrix afact = a;
  if (m > n && qr_type == qr_type_std)
    afact.resize (m, m);

  if (m > 0)
    {
      // workspace query.
      float rlwork;
      F77_XFCN (sgeqrf, SGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 &rlwork, -1, info));

      // allocate buffer and do the job.
      octave_idx_type lwork = rlwork;
      lwork = std::max (lwork, static_cast<octave_idx_type> (1));
      OCTAVE_LOCAL_BUFFER (float, work, lwork);
      F77_XFCN (sgeqrf, SGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 work, lwork, info));
    }

  form (n, afact, tau, qr_type);
}

void FloatQR::form (octave_idx_type n, FloatMatrix& afact,
                    float *tau, qr_type_t qr_type)
{
  octave_idx_type m = afact.rows ();
  octave_idx_type min_mn = std::min (m, n);
  octave_idx_type info;

  if (qr_type == qr_type_raw)
    {
      for (octave_idx_type j = 0; j < min_mn; j++)
        {
          octave_idx_type limit = j < min_mn - 1 ? j : min_mn - 1;
          for (octave_idx_type i = limit + 1; i < m; i++)
            afact.elem (i, j) *= tau[j];
        }

      r = afact;
    }
  else
    {
      // Attempt to minimize copying.
      if (m >= n)
        {
          // afact will become q.
          q = afact;
          octave_idx_type k = qr_type == qr_type_economy ? n : m;
          r = FloatMatrix (k, n);
          for (octave_idx_type j = 0; j < n; j++)
            {
              octave_idx_type i = 0;
              for (; i <= j; i++)
                r.xelem (i, j) = afact.xelem (i, j);
              for (; i < k; i++)
                r.xelem (i, j) = 0;
            }
          afact = FloatMatrix (); // optimize memory
        }
      else
        {
          // afact will become r.
          q = FloatMatrix (m, m);
          for (octave_idx_type j = 0; j < m; j++)
            for (octave_idx_type i = j + 1; i < m; i++)
              {
                q.xelem (i, j) = afact.xelem (i, j);
                afact.xelem (i, j) = 0;
              }
          r = afact;
        }


      if (m > 0)
        {
          octave_idx_type k = q.columns ();
          // workspace query.
          float rlwork;
          F77_XFCN (sorgqr, SORGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     &rlwork, -1, info));

          // allocate buffer and do the job.
          octave_idx_type lwork = rlwork;
          lwork = std::max (lwork, static_cast<octave_idx_type> (1));
          OCTAVE_LOCAL_BUFFER (float, work, lwork);
          F77_XFCN (sorgqr, SORGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     work, lwork, info));
        }
    }
}

#ifdef HAVE_QRUPDATE

void
FloatQR::update (const FloatColumnVector& u, const FloatColumnVector& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.length () == m && v.length () == n)
    {
      FloatColumnVector utmp = u;
      FloatColumnVector vtmp = v;
      OCTAVE_LOCAL_BUFFER (float, w, 2*k);
      F77_XFCN (sqr1up, SQR1UP, (m, n, k, q.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec (), w));
    }
  else
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");
}

void
FloatQR::update (const FloatMatrix& u, const FloatMatrix& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.rows () == m && v.rows () == n && u.cols () == v.cols ())
    {
      OCTAVE_LOCAL_BUFFER (float, w, 2*k);
      for (volatile octave_idx_type i = 0; i < u.cols (); i++)
        {
          FloatColumnVector utmp = u.column (i);
          FloatColumnVector vtmp = v.column (i);
          F77_XFCN (sqr1up, SQR1UP, (m, n, k, q.fortran_vec (),
                                     m, r.fortran_vec (), k,
                                     utmp.fortran_vec (), vtmp.fortran_vec (),
                                     w));
        }
    }
  else
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");
}

void
FloatQR::insert_col (const FloatColumnVector& u, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.length () != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  else if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");
  else
    {
      if (k < m)
        {
          q.resize (m, k+1);
          r.resize (k+1, n+1);
        }
      else
        {
          r.resize (k, n+1);
        }

      FloatColumnVector utmp = u;
      OCTAVE_LOCAL_BUFFER (float, w, k);
      F77_XFCN (sqrinc, SQRINC, (m, n, k, q.fortran_vec (), q.rows (),
                                 r.fortran_vec (), r.rows (), j + 1,
                                 utmp.data (), w));
    }
}

void
FloatQR::insert_col (const FloatMatrix& u, const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  octave_idx_type nj = js.length ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");
  else if (u.length () != m || u.columns () != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  else if (nj > 0 && (js(0) < 0 || js(nj-1) > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");
  else if (nj > 0)
    {
      octave_idx_type kmax = std::min (k + nj, m);
      if (k < m)
        {
          q.resize (m, kmax);
          r.resize (kmax, n + nj);
        }
      else
        {
          r.resize (k, n + nj);
        }

      OCTAVE_LOCAL_BUFFER (float, w, kmax);
      for (volatile octave_idx_type i = 0; i < js.length (); i++)
        {
          octave_idx_type ii = i;
          FloatColumnVector utmp = u.column (jsi(i));
          F77_XFCN (sqrinc, SQRINC, (m, n + ii, std::min (kmax, k + ii),
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (), js(ii) + 1,
                                     utmp.data (), w));
        }
    }
}

void
FloatQR::delete_col (octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");
  else
    {
      OCTAVE_LOCAL_BUFFER (float, w, k);
      F77_XFCN (sqrdec, SQRDEC, (m, n, k, q.fortran_vec (), q.rows (),
                                 r.fortran_vec (), r.rows (), j + 1, w));

      if (k < m)
        {
          q.resize (m, k-1);
          r.resize (k-1, n-1);
        }
      else
        {
          r.resize (k, n-1);
        }
    }
}

void
FloatQR::delete_col (const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, DESCENDING);
  octave_idx_type nj = js.length ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");
  else if (nj > 0 && (js(0) > n-1 || js(nj-1) < 0))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");
  else if (nj > 0)
    {
      OCTAVE_LOCAL_BUFFER (float, w, k);
      for (volatile octave_idx_type i = 0; i < js.length (); i++)
        {
          octave_idx_type ii = i;
          F77_XFCN (sqrdec, SQRDEC, (m, n - ii, k == m ? k : k - ii,
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (),
                                     js(ii) + 1, w));
        }
      if (k < m)
        {
          q.resize (m, k - nj);
          r.resize (k - nj, n - nj);
        }
      else
        {
          r.resize (k, n - nj);
        }

    }
}

void
FloatQR::insert_row (const FloatRowVector& u, octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = std::min (m, n);

  if (! q.is_square () || u.length () != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  else if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");
  else
    {
      q.resize (m + 1, m + 1);
      r.resize (m + 1, n);
      FloatRowVector utmp = u;
      OCTAVE_LOCAL_BUFFER (float, w, k);
      F77_XFCN (sqrinr, SQRINR, (m, n, q.fortran_vec (), q.rows (),
                                 r.fortran_vec (), r.rows (),
                                 j + 1, utmp.fortran_vec (), w));

    }
}

void
FloatQR::delete_row (octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");
  else if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");
  else
    {
      OCTAVE_LOCAL_BUFFER (float, w, 2*m);
      F77_XFCN (sqrder, SQRDER, (m, n, q.fortran_vec (), q.rows (),
                                 r.fortran_vec (), r.rows (), j + 1,
                                 w));

      q.resize (m - 1, m - 1);
      r.resize (m - 1, n);
    }
}

void
FloatQR::shift_cols (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");
  else
    {
      OCTAVE_LOCAL_BUFFER (float, w, 2*k);
      F77_XFCN (sqrshc, SQRSHC, (m, n, k,
                                 q.fortran_vec (), q.rows (),
                                 r.fortran_vec (), r.rows (),
                                 i + 1, j + 1, w));
    }
}

#else

// Replacement update methods.

void
FloatQR::update (const FloatColumnVector& u, const FloatColumnVector& v)
{
  warn_qrupdate_once ();

  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();

  if (u.length () == m && v.length () == n)
    {
      init (q*r + FloatMatrix (u) * FloatMatrix (v).transpose (), get_type ());
    }
  else
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");
}

void
FloatQR::update (const FloatMatrix& u, const FloatMatrix& v)
{
  warn_qrupdate_once ();

  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();

  if (u.rows () == m && v.rows () == n && u.cols () == v.cols ())
    {
      init (q*r + u * v.transpose (), get_type ());
    }
  else
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");
}

static
FloatMatrix insert_col (const FloatMatrix& a, octave_idx_type i,
                        const FloatColumnVector& x)
{
  FloatMatrix retval (a.rows (), a.columns () + 1);
  retval.assign (idx_vector::colon, idx_vector (0, i),
                 a.index (idx_vector::colon, idx_vector (0, i)));
  retval.assign (idx_vector::colon, idx_vector (i), x);
  retval.assign (idx_vector::colon, idx_vector (i+1, retval.columns ()),
                 a.index (idx_vector::colon, idx_vector (i, a.columns ())));
  return retval;
}

static
FloatMatrix insert_row (const FloatMatrix& a, octave_idx_type i,
                        const FloatRowVector& x)
{
  FloatMatrix retval (a.rows () + 1, a.columns ());
  retval.assign (idx_vector (0, i), idx_vector::colon,
                 a.index (idx_vector (0, i), idx_vector::colon));
  retval.assign (idx_vector (i), idx_vector::colon, x);
  retval.assign (idx_vector (i+1, retval.rows ()), idx_vector::colon,
                 a.index (idx_vector (i, a.rows ()), idx_vector::colon));
  return retval;
}

static
FloatMatrix delete_col (const FloatMatrix& a, octave_idx_type i)
{
  FloatMatrix retval = a;
  retval.delete_elements (1, idx_vector (i));
  return retval;
}

static
FloatMatrix delete_row (const FloatMatrix& a, octave_idx_type i)
{
  FloatMatrix retval = a;
  retval.delete_elements (0, idx_vector (i));
  return retval;
}

static
FloatMatrix shift_cols (const FloatMatrix& a,
                        octave_idx_type i, octave_idx_type j)
{
  octave_idx_type n = a.columns ();
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

  return a.index (idx_vector::colon, idx_vector (p));
}

void
FloatQR::insert_col (const FloatColumnVector& u, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();

  if (u.length () != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  else if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");
  else
    {
      init (::insert_col (q*r, j, u), get_type ());
    }
}

void
FloatQR::insert_col (const FloatMatrix& u, const Array<octave_idx_type>& j)
{
  warn_qrupdate_once ();

  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  octave_idx_type nj = js.length ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");
  else if (u.length () != m || u.columns () != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  else if (nj > 0 && (js(0) < 0 || js(nj-1) > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");
  else if (nj > 0)
    {
      FloatMatrix a = q*r;
      for (octave_idx_type i = 0; i < js.length (); i++)
        a = ::insert_col (a, js(i), u.column (i));
      init (a, get_type ());
    }
}

void
FloatQR::delete_col (octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = r.columns ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");
  else
    {
      init (::delete_col (q*r, j), get_type ());
    }
}

void
FloatQR::delete_col (const Array<octave_idx_type>& j)
{
  warn_qrupdate_once ();

  octave_idx_type n = r.columns ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, DESCENDING);
  octave_idx_type nj = js.length ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");
  else if (nj > 0 && (js(0) > n-1 || js(nj-1) < 0))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");
  else if (nj > 0)
    {
      FloatMatrix a = q*r;
      for (octave_idx_type i = 0; i < js.length (); i++)
        a = ::delete_col (a, js(i));
      init (a, get_type ());
    }
}

void
FloatQR::insert_row (const FloatRowVector& u, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square () || u.length () != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  else if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");
  else
    {
      init (::insert_row (q*r, j, u), get_type ());
    }
}

void
FloatQR::delete_row (octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type m = r.rows ();

  if (! q.is_square ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");
  else if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");
  else
    {
      init (::delete_row (q*r, j), get_type ());
    }
}

void
FloatQR::shift_cols (octave_idx_type i, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = r.columns ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");
  else
    {
      init (::shift_cols (q*r, i, j), get_type ());
    }
}

#endif
