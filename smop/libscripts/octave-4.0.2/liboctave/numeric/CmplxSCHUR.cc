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

#include "CmplxSCHUR.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "oct-locbuf.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (zgeesx, ZGEESX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             ComplexSCHUR::select_function,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, octave_idx_type&,
                             Complex*, Complex*, const octave_idx_type&,
                             double&, double&, Complex*,
                             const octave_idx_type&, double*,
                             octave_idx_type*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zrsf2csf, ZRSF2CSF) (const octave_idx_type&, Complex *,
                                 Complex *, double *, double *);
}

static octave_idx_type
select_ana (const Complex& a)
{
  return a.real () < 0.0;
}

static octave_idx_type
select_dig (const Complex& a)
{
  return (abs (a) < 1.0);
}

octave_idx_type
ComplexSCHUR::init (const ComplexMatrix& a, const std::string& ord,
                    bool calc_unitary)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler)
        ("ComplexSCHUR requires square matrix");
      return -1;
    }
  else if (a_nr == 0)
    {
      schur_mat.clear ();
      unitary_mat.clear ();
      return 0;
    }

  // Workspace requirements may need to be fixed if any of the
  // following change.

  char jobvs;
  char sense = 'N';
  char sort = 'N';

  if (calc_unitary)
    jobvs = 'V';
  else
    jobvs = 'N';

  char ord_char = ord.empty () ? 'U' : ord[0];

  if (ord_char == 'A' || ord_char == 'D' || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig;
  else
    selector = 0;

  octave_idx_type n = a_nc;
  octave_idx_type lwork = 8 * n;
  octave_idx_type info;
  octave_idx_type sdim;
  double rconde;
  double rcondv;

  schur_mat = a;
  if (calc_unitary)
    unitary_mat.clear (n, n);

  Complex *s = schur_mat.fortran_vec ();
  Complex *q = unitary_mat.fortran_vec ();

  Array<double> rwork (dim_vector (n, 1));
  double *prwork = rwork.fortran_vec ();

  Array<Complex> w (dim_vector (n, 1));
  Complex *pw = w.fortran_vec ();

  Array<Complex> work (dim_vector (lwork, 1));
  Complex *pwork = work.fortran_vec ();

  // BWORK is not referenced for non-ordered Schur.
  octave_idx_type ntmp = (ord_char == 'N' || ord_char == 'n') ? 0 : n;
  Array<octave_idx_type> bwork (dim_vector (ntmp, 1));
  octave_idx_type *pbwork = bwork.fortran_vec ();

  F77_XFCN (zgeesx, ZGEESX, (F77_CONST_CHAR_ARG2 (&jobvs, 1),
                             F77_CONST_CHAR_ARG2 (&sort, 1),
                             selector,
                             F77_CONST_CHAR_ARG2 (&sense, 1),
                             n, s, n, sdim, pw, q, n, rconde, rcondv,
                             pwork, lwork, prwork, pbwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return info;
}

ComplexSCHUR::ComplexSCHUR (const ComplexMatrix& s, const ComplexMatrix& u)
  : schur_mat (s), unitary_mat (u), selector (0)
{
  octave_idx_type n = s.rows ();
  if (s.columns () != n || u.rows () != n || u.columns () != n)
    (*current_liboctave_error_handler)
      ("schur: inconsistent matrix dimensions");
}

ComplexSCHUR::ComplexSCHUR (const SCHUR& s)
  : schur_mat (s.schur_matrix ()), unitary_mat (s.unitary_matrix ()),
    selector (0)
{
  octave_idx_type n = schur_mat.rows ();
  if (n > 0)
    {
      OCTAVE_LOCAL_BUFFER (double, c, n-1);
      OCTAVE_LOCAL_BUFFER (double, sx, n-1);

      F77_XFCN (zrsf2csf, ZRSF2CSF, (n, schur_mat.fortran_vec (),
                                     unitary_mat.fortran_vec (), c, sx));
    }
}
