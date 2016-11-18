/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008 Jaroslav Hajek

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

#include <string>

#include "fCmplxAEPBAL.h"
#include "fMatrix.h"
#include "f77-fcn.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (cgebal, CGEBAL) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&,
                             octave_idx_type&, float*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgebak, CGEBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, const float*,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);
}

FloatComplexAEPBALANCE::FloatComplexAEPBALANCE (const FloatComplexMatrix& a,
                                                bool noperm, bool noscal)
  : base_aepbal<FloatComplexMatrix, FloatColumnVector> ()
{
  octave_idx_type n = a.cols ();

  if (a.rows () != n)
    {
      (*current_liboctave_error_handler) ("AEPBALANCE requires square matrix");
      return;
    }

  octave_idx_type info;

  scale = FloatColumnVector (n);
  float *pscale = scale.fortran_vec ();

  balanced_mat = a;
  FloatComplex *p_balanced_mat = balanced_mat.fortran_vec ();

  job = noperm ? (noscal ? 'N' : 'S') : (noscal ? 'P' : 'B');

  F77_XFCN (cgebal, CGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, p_balanced_mat, n, ilo, ihi,
                             pscale, info
                             F77_CHAR_ARG_LEN (1)));
}

FloatComplexMatrix
FloatComplexAEPBALANCE::balancing_matrix (void) const
{
  octave_idx_type n = balanced_mat.rows ();
  FloatComplexMatrix balancing_mat (n, n, 0.0);
  for (octave_idx_type i = 0; i < n; i++)
    balancing_mat.elem (i, i) = 1.0;

  FloatComplex *p_balancing_mat = balancing_mat.fortran_vec ();
  const float *pscale = scale.fortran_vec ();

  octave_idx_type info;

  char side = 'R';

  F77_XFCN (cgebak, CGEBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 (&side, 1),
                             n, ilo, ihi, pscale, n,
                             p_balancing_mat, n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return balancing_mat;
}
