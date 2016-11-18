/*

Copyright (C) 1994-2015 John W. Eaton
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

#include "dbleLU.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "oct-locbuf.h"
#include "dColVector.h"

// Instantiate the base LU class for the types we need.

#include "base-lu.h"
#include "base-lu.cc"

template class base_lu <Matrix>;

// Define the constructor for this particular derivation.

extern "C"
{
  F77_RET_T
  F77_FUNC (dgetrf, DGETRF) (const octave_idx_type&, const octave_idx_type&,
                             double*, const octave_idx_type&,
                             octave_idx_type*, octave_idx_type&);

#ifdef HAVE_QRUPDATE_LUU
  F77_RET_T
  F77_FUNC (dlu1up, DLU1UP) (const octave_idx_type&, const octave_idx_type&,
                             double *, const octave_idx_type&,
                             double *, const octave_idx_type&,
                             double *, double *);

  F77_RET_T
  F77_FUNC (dlup1up, DLUP1UP) (const octave_idx_type&, const octave_idx_type&,
                               double *, const octave_idx_type&,
                               double *, const octave_idx_type&,
                               octave_idx_type *, const double *,
                               const double *, double *);
#endif
}

LU::LU (const Matrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();
  octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

  ipvt.resize (dim_vector (mn, 1));
  octave_idx_type *pipvt = ipvt.fortran_vec ();

  a_fact = a;
  double *tmp_data = a_fact.fortran_vec ();

  octave_idx_type info = 0;

  F77_XFCN (dgetrf, DGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  for (octave_idx_type i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#ifdef HAVE_QRUPDATE_LUU

void LU::update (const ColumnVector& u, const ColumnVector& v)
{
  if (packed ())
    unpack ();

  Matrix& l = l_fact;
  Matrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.length () == m && v.length () == n)
    {
      ColumnVector utmp = u;
      ColumnVector vtmp = v;
      F77_XFCN (dlu1up, DLU1UP, (m, n, l.fortran_vec (), m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec ()));
    }
  else
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");
}

void LU::update (const Matrix& u, const Matrix& v)
{
  if (packed ())
    unpack ();

  Matrix& l = l_fact;
  Matrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () == m && v.rows () == n && u.cols () == v.cols ())
    {
      for (volatile octave_idx_type i = 0; i < u.cols (); i++)
        {
          ColumnVector utmp = u.column (i);
          ColumnVector vtmp = v.column (i);
          F77_XFCN (dlu1up, DLU1UP, (m, n, l.fortran_vec (),
                                     m, r.fortran_vec (), k,
                                     utmp.fortran_vec (), vtmp.fortran_vec ()));
        }
    }
  else
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");
}

void LU::update_piv (const ColumnVector& u, const ColumnVector& v)
{
  if (packed ())
    unpack ();

  Matrix& l = l_fact;
  Matrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.length () == m && v.length () == n)
    {
      ColumnVector utmp = u;
      ColumnVector vtmp = v;
      OCTAVE_LOCAL_BUFFER (double, w, m);
      for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
      F77_XFCN (dlup1up, DLUP1UP, (m, n, l.fortran_vec (),
                                   m, r.fortran_vec (), k,
                                   ipvt.fortran_vec (),
                                   utmp.data (), vtmp.data (), w));
      for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
    }
  else
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");
}

void LU::update_piv (const Matrix& u, const Matrix& v)
{
  if (packed ())
    unpack ();

  Matrix& l = l_fact;
  Matrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () == m && v.rows () == n && u.cols () == v.cols ())
    {
      OCTAVE_LOCAL_BUFFER (double, w, m);
      for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
      for (volatile octave_idx_type i = 0; i < u.cols (); i++)
        {
          ColumnVector utmp = u.column (i);
          ColumnVector vtmp = v.column (i);
          F77_XFCN (dlup1up, DLUP1UP, (m, n, l.fortran_vec (),
                                       m, r.fortran_vec (), k,
                                       ipvt.fortran_vec (),
                                       utmp.data (), vtmp.data (), w));
        }
      for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
    }
  else
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");
}

#else

void LU::update (const ColumnVector&, const ColumnVector&)
{
  (*current_liboctave_error_handler)
    ("luupdate: not available in this version");
}

void LU::update (const Matrix&, const Matrix&)
{
  (*current_liboctave_error_handler)
    ("luupdate: not available in this version");
}

void LU::update_piv (const ColumnVector&, const ColumnVector&)
{
  (*current_liboctave_error_handler)
    ("luupdate: not available in this version");
}

void LU::update_piv (const Matrix&, const Matrix&)
{
  (*current_liboctave_error_handler)
    ("luupdate: not available in this version");
}

#endif
