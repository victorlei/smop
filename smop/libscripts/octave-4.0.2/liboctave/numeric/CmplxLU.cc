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

#include "CmplxLU.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "oct-locbuf.h"
#include "CColVector.h"

// Instantiate the base LU class for the types we need.

#include "base-lu.h"
#include "base-lu.cc"

template class base_lu <ComplexMatrix>;

// Define the constructor for this particular derivation.

extern "C"
{
  F77_RET_T
  F77_FUNC (zgetrf, ZGETRF) (const octave_idx_type&, const octave_idx_type&,
                             Complex*, const octave_idx_type&,
                             octave_idx_type*, octave_idx_type&);

#ifdef HAVE_QRUPDATE_LUU
  F77_RET_T
  F77_FUNC (zlu1up, ZLU1UP) (const octave_idx_type&, const octave_idx_type&,
                             Complex *, const octave_idx_type&,
                             Complex *, const octave_idx_type&,
                             Complex *, Complex *);

  F77_RET_T
  F77_FUNC (zlup1up, ZLUP1UP) (const octave_idx_type&, const octave_idx_type&,
                               Complex *, const octave_idx_type&,
                               Complex *, const octave_idx_type&,
                               octave_idx_type *, const Complex *,
                               const Complex *, Complex *);
#endif
}

ComplexLU::ComplexLU (const ComplexMatrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();
  octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

  ipvt.resize (dim_vector (mn, 1));
  octave_idx_type *pipvt = ipvt.fortran_vec ();

  a_fact = a;
  Complex *tmp_data = a_fact.fortran_vec ();

  octave_idx_type info = 0;

  F77_XFCN (zgetrf, ZGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  for (octave_idx_type i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#ifdef HAVE_QRUPDATE_LUU

void ComplexLU::update (const ComplexColumnVector& u,
                        const ComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = l_fact;
  ComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.length () == m && v.length () == n)
    {
      ComplexColumnVector utmp = u;
      ComplexColumnVector vtmp = v;
      F77_XFCN (zlu1up, ZLU1UP, (m, n, l.fortran_vec (), m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec ()));
    }
  else
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");
}

void ComplexLU::update (const ComplexMatrix& u, const ComplexMatrix& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = l_fact;
  ComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () == m && v.rows () == n && u.cols () == v.cols ())
    {
      for (volatile octave_idx_type i = 0; i < u.cols (); i++)
        {
          ComplexColumnVector utmp = u.column (i);
          ComplexColumnVector vtmp = v.column (i);
          F77_XFCN (zlu1up, ZLU1UP, (m, n, l.fortran_vec (),
                                     m, r.fortran_vec (), k,
                                     utmp.fortran_vec (), vtmp.fortran_vec ()));
        }
    }
  else
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");
}

void ComplexLU::update_piv (const ComplexColumnVector& u,
                            const ComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = l_fact;
  ComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.length () == m && v.length () == n)
    {
      ComplexColumnVector utmp = u;
      ComplexColumnVector vtmp = v;
      OCTAVE_LOCAL_BUFFER (Complex, w, m);
      for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
      F77_XFCN (zlup1up, ZLUP1UP, (m, n, l.fortran_vec (),
                                   m, r.fortran_vec (), k,
                                   ipvt.fortran_vec (),
                                   utmp.data (), vtmp.data (), w));
      for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
    }
  else
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");
}

void ComplexLU::update_piv (const ComplexMatrix& u, const ComplexMatrix& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = l_fact;
  ComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () == m && v.rows () == n && u.cols () == v.cols ())
    {
      OCTAVE_LOCAL_BUFFER (Complex, w, m);
      for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
      for (volatile octave_idx_type i = 0; i < u.cols (); i++)
        {
          ComplexColumnVector utmp = u.column (i);
          ComplexColumnVector vtmp = v.column (i);
          F77_XFCN (zlup1up, ZLUP1UP, (m, n, l.fortran_vec (),
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

void ComplexLU::update (const ComplexColumnVector&, const ComplexColumnVector&)
{
  (*current_liboctave_error_handler)
    ("luupdate: not available in this version");
}

void ComplexLU::update (const ComplexMatrix&, const ComplexMatrix&)
{
  (*current_liboctave_error_handler)
    ("luupdate: not available in this version");
}

void ComplexLU::update_piv (const ComplexColumnVector&,
                            const ComplexColumnVector&)
{
  (*current_liboctave_error_handler)
    ("luupdate: not available in this version");
}

void ComplexLU::update_piv (const ComplexMatrix&, const ComplexMatrix&)
{
  (*current_liboctave_error_handler)
    ("luupdate: not available in this version");
}

#endif
