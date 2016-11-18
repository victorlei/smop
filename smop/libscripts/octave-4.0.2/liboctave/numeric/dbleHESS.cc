/*

Copyright (C) 1994-2015 John W. Eaton

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

#include "dbleHESS.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgebal, DGEBAL) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type&,
                             octave_idx_type&, double*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgehrd, DGEHRD) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*, double*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dorghr, DORGHR) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*, double*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgebak, DGEBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);
}

octave_idx_type
HESS::init (const Matrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("HESS requires square matrix");
      return -1;
    }

  char job = 'N';
  char side = 'R';

  octave_idx_type n = a_nc;
  octave_idx_type lwork = 32 * n;
  octave_idx_type info;
  octave_idx_type ilo;
  octave_idx_type ihi;

  hess_mat = a;
  double *h = hess_mat.fortran_vec ();

  Array<double> scale (dim_vector (n, 1));
  double *pscale = scale.fortran_vec ();

  F77_XFCN (dgebal, DGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, h, n, ilo, ihi, pscale, info
                             F77_CHAR_ARG_LEN (1)));

  Array<double> tau (dim_vector (n-1, 1));
  double *ptau = tau.fortran_vec ();

  Array<double> work (dim_vector (lwork, 1));
  double *pwork = work.fortran_vec ();

  F77_XFCN (dgehrd, DGEHRD, (n, ilo, ihi, h, n, ptau, pwork,
                             lwork, info));

  unitary_hess_mat = hess_mat;
  double *z = unitary_hess_mat.fortran_vec ();

  F77_XFCN (dorghr, DORGHR, (n, ilo, ihi, z, n, ptau, pwork,
                             lwork, info));

  F77_XFCN (dgebak, DGEBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 (&side, 1),
                             n, ilo, ihi, pscale, n, z,
                             n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // If someone thinks of a more graceful way of doing
  // this (or faster for that matter :-)), please let
  // me know!

  if (n > 2)
    for (octave_idx_type j = 0; j < a_nc; j++)
      for (octave_idx_type i = j+2; i < a_nr; i++)
        hess_mat.elem (i, j) = 0;

  return info;
}
