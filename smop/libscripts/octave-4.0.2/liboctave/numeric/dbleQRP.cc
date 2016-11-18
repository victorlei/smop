/*

Copyright (C) 1994-2015 John W. Eaton
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

#include <cassert>

#include "dbleQRP.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "oct-locbuf.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgeqp3, DGEQP3) (const octave_idx_type&, const octave_idx_type&,
                             double*, const octave_idx_type&,
                             octave_idx_type*, double*, double*,
                             const octave_idx_type&, octave_idx_type&);
}

// It would be best to share some of this code with QR class...

QRP::QRP (const Matrix& a, qr_type_t qr_type)
  : QR (), p ()
{
  init (a, qr_type);
}

void
QRP::init (const Matrix& a, qr_type_t qr_type)
{
  assert (qr_type != qr_type_raw);

  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  octave_idx_type min_mn = m < n ? m : n;
  OCTAVE_LOCAL_BUFFER (double, tau, min_mn);

  octave_idx_type info = 0;

  Matrix afact = a;
  if (m > n && qr_type == qr_type_std)
    afact.resize (m, m);

  MArray<octave_idx_type> jpvt (dim_vector (n, 1), 0);

  if (m > 0)
    {
      // workspace query.
      double rlwork;
      F77_XFCN (dgeqp3, DGEQP3, (m, n, afact.fortran_vec (),
                                 m, jpvt.fortran_vec (), tau,
                                 &rlwork, -1, info));

      // allocate buffer and do the job.
      octave_idx_type lwork = rlwork;
      lwork = std::max (lwork, static_cast<octave_idx_type> (1));
      OCTAVE_LOCAL_BUFFER (double, work, lwork);
      F77_XFCN (dgeqp3, DGEQP3, (m, n, afact.fortran_vec (),
                                 m, jpvt.fortran_vec (), tau,
                                 work, lwork, info));
    }
  else
    for (octave_idx_type i = 0; i < n; i++) jpvt(i) = i+1;

  // Form Permutation matrix (if economy is requested, return the
  // indices only!)

  jpvt -= static_cast<octave_idx_type> (1);
  p = PermMatrix (jpvt, true);


  form (n, afact, tau, qr_type);
}

RowVector
QRP::Pvec (void) const
{
  Array<double> pa (p.col_perm_vec ());
  RowVector pv (MArray<double> (pa) + 1.0);
  return pv;
}
