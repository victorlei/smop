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

#include "floatHESS.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (sgebal, SGEBAL) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, octave_idx_type&,
                             octave_idx_type&, float*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgehrd, SGEHRD) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*, float*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (sorghr, SORGHR) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*, float*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (sgebak, SGEBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);
}

octave_idx_type
FloatHESS::init (const FloatMatrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("FloatHESS requires square matrix");
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
  float *h = hess_mat.fortran_vec ();

  Array<float> scale (dim_vector (n, 1));
  float *pscale = scale.fortran_vec ();

  F77_XFCN (sgebal, SGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, h, n, ilo, ihi, pscale, info
                             F77_CHAR_ARG_LEN (1)));

  Array<float> tau (dim_vector (n-1, 1));
  float *ptau = tau.fortran_vec ();

  Array<float> work (dim_vector (lwork, 1));
  float *pwork = work.fortran_vec ();

  F77_XFCN (sgehrd, SGEHRD, (n, ilo, ihi, h, n, ptau, pwork,
                             lwork, info));

  unitary_hess_mat = hess_mat;
  float *z = unitary_hess_mat.fortran_vec ();

  F77_XFCN (sorghr, SORGHR, (n, ilo, ihi, z, n, ptau, pwork,
                             lwork, info));

  F77_XFCN (sgebak, SGEBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
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
