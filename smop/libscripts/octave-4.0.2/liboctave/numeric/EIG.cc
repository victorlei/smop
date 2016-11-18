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

#include "EIG.h"
#include "dColVector.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgeev, DGEEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, double*,
                           const octave_idx_type&, double*, double*,
                           double*, const octave_idx_type&, double*,
                           const octave_idx_type&, double*,
                           const octave_idx_type&, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgeev, ZGEEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, Complex*,
                           const octave_idx_type&, Complex*,
                           Complex*, const octave_idx_type&, Complex*,
                           const octave_idx_type&, Complex*,
                           const octave_idx_type&, double*, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dsyev, DSYEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, double*,
                           const octave_idx_type&, double*, double*,
                           const octave_idx_type&, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zheev, ZHEEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, Complex*,
                           const octave_idx_type&, double*,
                           Complex*, const octave_idx_type&, double*,
                           octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpotrf, DPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpotrf, ZPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             Complex*, const octave_idx_type&,
                             octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dggev, DGGEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&,
                           double*, const octave_idx_type&,
                           double*, const octave_idx_type&,
                           double*, double*, double *, double*,
                           const octave_idx_type&, double*,
                           const octave_idx_type&, double*,
                           const octave_idx_type&, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dsygv, DSYGV) (const octave_idx_type&,
                           F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, double*,
                           const octave_idx_type&, double*,
                           const octave_idx_type&, double*, double*,
                           const octave_idx_type&, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zggev, ZGGEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&,
                           Complex*, const octave_idx_type&,
                           Complex*, const octave_idx_type&,
                           Complex*, Complex*, Complex*,
                           const octave_idx_type&, Complex*,
                           const octave_idx_type&, Complex*,
                           const octave_idx_type&, double*, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zhegv, ZHEGV) (const octave_idx_type&,
                           F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, Complex*,
                           const octave_idx_type&, Complex*,
                           const octave_idx_type&, double*, Complex*,
                           const octave_idx_type&, double*, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);
}

octave_idx_type
EIG::init (const Matrix& a, bool calc_ev)
{
  if (a.any_element_is_inf_or_nan ())
    {
      (*current_liboctave_error_handler)
        ("EIG: matrix contains Inf or NaN values");
      return -1;
    }

  if (a.is_symmetric ())
    return symmetric_init (a, calc_ev);

  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  Matrix atmp = a;
  double *tmp_data = atmp.fortran_vec ();

  Array<double> wr (dim_vector (n, 1));
  double *pwr = wr.fortran_vec ();

  Array<double> wi (dim_vector (n, 1));
  double *pwi = wi.fortran_vec ();

  octave_idx_type tnvr = calc_ev ? n : 0;
  Matrix vr (tnvr, tnvr);
  double *pvr = vr.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  double *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (dgeev, DGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           n, tmp_data, n, pwr, pwi, dummy,
                           idummy, pvr, n, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<double> work (dim_vector (lwork, 1));
      double *pwork = work.fortran_vec ();

      F77_XFCN (dgeev, DGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               n, tmp_data, n, pwr, pwi, dummy,
                               idummy, pvr, n, pwork, lwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in dgeev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("dgeev failed to converge");
          return info;
        }

      lambda.resize (n);
      octave_idx_type nvr = calc_ev ? n : 0;
      v.resize (nvr, nvr);

      for (octave_idx_type j = 0; j < n; j++)
        {
          if (wi.elem (j) == 0.0)
            {
              lambda.elem (j) = Complex (wr.elem (j));
              for (octave_idx_type i = 0; i < nvr; i++)
                v.elem (i, j) = vr.elem (i, j);
            }
          else
            {
              if (j+1 >= n)
                {
                  (*current_liboctave_error_handler) ("EIG: internal error");
                  return -1;
                }

              lambda.elem (j) = Complex (wr.elem (j), wi.elem (j));
              lambda.elem (j+1) = Complex (wr.elem (j+1), wi.elem (j+1));

              for (octave_idx_type i = 0; i < nvr; i++)
                {
                  double real_part = vr.elem (i, j);
                  double imag_part = vr.elem (i, j+1);
                  v.elem (i, j) = Complex (real_part, imag_part);
                  v.elem (i, j+1) = Complex (real_part, -imag_part);
                }
              j++;
            }
        }
    }
  else
    (*current_liboctave_error_handler) ("dgeev workspace query failed");

  return info;
}

octave_idx_type
EIG::symmetric_init (const Matrix& a, bool calc_ev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  Matrix atmp = a;
  double *tmp_data = atmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  F77_XFCN (dsyev, DSYEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, tmp_data, n, pwr, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<double> work (dim_vector (lwork, 1));
      double *pwork = work.fortran_vec ();

      F77_XFCN (dsyev, DSYEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               F77_CONST_CHAR_ARG2 ("U", 1),
                               n, tmp_data, n, pwr, pwork, lwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in dsyev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("dsyev failed to converge");
          return info;
        }

      lambda = ComplexColumnVector (wr);
      v = calc_ev ? ComplexMatrix (atmp) : ComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("dsyev workspace query failed");

  return info;
}

octave_idx_type
EIG::init (const ComplexMatrix& a, bool calc_ev)
{
  if (a.any_element_is_inf_or_nan ())
    {
      (*current_liboctave_error_handler)
        ("EIG: matrix contains Inf or NaN values");
      return -1;
    }

  if (a.is_hermitian ())
    return hermitian_init (a, calc_ev);

  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  ComplexColumnVector w (n);
  Complex *pw = w.fortran_vec ();

  octave_idx_type nvr = calc_ev ? n : 0;
  ComplexMatrix vtmp (nvr, nvr);
  Complex *pv = vtmp.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 2*n;
  Array<double> rwork (dim_vector (lrwork, 1));
  double *prwork = rwork.fortran_vec ();

  Complex *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (zgeev, ZGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           n, tmp_data, n, pw, dummy, idummy,
                           pv, n, &dummy_work, lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<Complex> work (dim_vector (lwork, 1));
      Complex *pwork = work.fortran_vec ();

      F77_XFCN (zgeev, ZGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               n, tmp_data, n, pw, dummy, idummy,
                               pv, n, pwork, lwork, prwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in zgeev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("zgeev failed to converge");
          return info;
        }

      lambda = w;
      v = vtmp;
    }
  else
    (*current_liboctave_error_handler) ("zgeev workspace query failed");

  return info;
}

octave_idx_type
EIG::hermitian_init (const ComplexMatrix& a, bool calc_ev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 3*n;
  Array<double> rwork (dim_vector (lrwork, 1));
  double *prwork = rwork.fortran_vec ();

  F77_XFCN (zheev, ZHEEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, tmp_data, n, pwr, &dummy_work, lwork,
                           prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<Complex> work (dim_vector (lwork, 1));
      Complex *pwork = work.fortran_vec ();

      F77_XFCN (zheev, ZHEEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               F77_CONST_CHAR_ARG2 ("U", 1),
                               n, tmp_data, n, pwr, pwork, lwork, prwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in zheev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("zheev failed to converge");
          return info;
        }

      lambda = ComplexColumnVector (wr);
      v = calc_ev ? ComplexMatrix (atmp) : ComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("zheev workspace query failed");

  return info;
}

octave_idx_type
EIG::init (const Matrix& a, const Matrix& b, bool calc_ev)
{
  if (a.any_element_is_inf_or_nan () || b.any_element_is_inf_or_nan ())
    {
      (*current_liboctave_error_handler)
        ("EIG: matrix contains Inf or NaN values");
      return -1;
    }

  octave_idx_type n = a.rows ();
  octave_idx_type nb = b.rows ();

  if (n != a.cols () || nb != b.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  if (n != nb)
    {
      (*current_liboctave_error_handler) ("EIG requires same size matrices");
      return -1;
    }

  octave_idx_type info = 0;

  Matrix tmp = b;
  double *tmp_data = tmp.fortran_vec ();

  F77_XFCN (dpotrf, DPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1),
                             n, tmp_data, n,
                             info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (a.is_symmetric () && b.is_symmetric () && info == 0)
    return symmetric_init (a, b, calc_ev);

  Matrix atmp = a;
  double *atmp_data = atmp.fortran_vec ();

  Matrix btmp = b;
  double *btmp_data = btmp.fortran_vec ();

  Array<double> ar (dim_vector (n, 1));
  double *par = ar.fortran_vec ();

  Array<double> ai (dim_vector (n, 1));
  double *pai = ai.fortran_vec ();

  Array<double> beta (dim_vector (n, 1));
  double *pbeta = beta.fortran_vec ();

  octave_idx_type tnvr = calc_ev ? n : 0;
  Matrix vr (tnvr, tnvr);
  double *pvr = vr.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  double *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (dggev, DGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           n, atmp_data, n, btmp_data, n,
                           par, pai, pbeta,
                           dummy, idummy, pvr, n,
                           &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<double> work (dim_vector (lwork, 1));
      double *pwork = work.fortran_vec ();

      F77_XFCN (dggev, DGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               n, atmp_data, n, btmp_data, n,
                               par, pai, pbeta,
                               dummy, idummy, pvr, n,
                               pwork, lwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in dggev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("dggev failed to converge");
          return info;
        }

      lambda.resize (n);
      octave_idx_type nvr = calc_ev ? n : 0;
      v.resize (nvr, nvr);

      for (octave_idx_type j = 0; j < n; j++)
        {
          if (ai.elem (j) == 0.0)
            {
              lambda.elem (j) = Complex (ar.elem (j) / beta.elem (j));
              for (octave_idx_type i = 0; i < nvr; i++)
                v.elem (i, j) = vr.elem (i, j);
            }
          else
            {
              if (j+1 >= n)
                {
                  (*current_liboctave_error_handler) ("EIG: internal error");
                  return -1;
                }

              lambda.elem (j) = Complex (ar.elem (j) / beta.elem (j),
                                         ai.elem (j) / beta.elem (j));
              lambda.elem (j+1) = Complex (ar.elem (j+1) / beta.elem (j+1),
                                           ai.elem (j+1) / beta.elem (j+1));

              for (octave_idx_type i = 0; i < nvr; i++)
                {
                  double real_part = vr.elem (i, j);
                  double imag_part = vr.elem (i, j+1);
                  v.elem (i, j) = Complex (real_part, imag_part);
                  v.elem (i, j+1) = Complex (real_part, -imag_part);
                }
              j++;
            }
        }
    }
  else
    (*current_liboctave_error_handler) ("dggev workspace query failed");

  return info;
}

octave_idx_type
EIG::symmetric_init (const Matrix& a, const Matrix& b, bool calc_ev)
{
  octave_idx_type n = a.rows ();
  octave_idx_type nb = b.rows ();

  if (n != a.cols () || nb != b.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  if (n != nb)
    {
      (*current_liboctave_error_handler) ("EIG requires same size matrices");
      return -1;
    }

  octave_idx_type info = 0;

  Matrix atmp = a;
  double *atmp_data = atmp.fortran_vec ();

  Matrix btmp = b;
  double *btmp_data = btmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  F77_XFCN (dsygv, DSYGV, (1, F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, atmp_data, n,
                           btmp_data, n,
                           pwr, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<double> work (dim_vector (lwork, 1));
      double *pwork = work.fortran_vec ();

      F77_XFCN (dsygv, DSYGV, (1, F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               F77_CONST_CHAR_ARG2 ("U", 1),
                               n, atmp_data, n,
                               btmp_data, n,
                               pwr, pwork, lwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in dsygv");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("dsygv failed to converge");
          return info;
        }

      lambda = ComplexColumnVector (wr);
      v = calc_ev ? ComplexMatrix (atmp) : ComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("dsygv workspace query failed");

  return info;
}

octave_idx_type
EIG::init (const ComplexMatrix& a, const ComplexMatrix& b, bool calc_ev)
{
  if (a.any_element_is_inf_or_nan () || b.any_element_is_inf_or_nan ())
    {
      (*current_liboctave_error_handler)
        ("EIG: matrix contains Inf or NaN values");
      return -1;
    }

  octave_idx_type n = a.rows ();
  octave_idx_type nb = b.rows ();

  if (n != a.cols () || nb != b.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  if (n != nb)
    {
      (*current_liboctave_error_handler) ("EIG requires same size matrices");
      return -1;
    }

  octave_idx_type info = 0;

  ComplexMatrix tmp = b;
  Complex*tmp_data = tmp.fortran_vec ();

  F77_XFCN (zpotrf, ZPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1),
                             n, tmp_data, n,
                             info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (a.is_hermitian () && b.is_hermitian () && info == 0)
    return hermitian_init (a, calc_ev);

  ComplexMatrix atmp = a;
  Complex *atmp_data = atmp.fortran_vec ();

  ComplexMatrix btmp = b;
  Complex *btmp_data = btmp.fortran_vec ();

  ComplexColumnVector alpha (n);
  Complex *palpha = alpha.fortran_vec ();

  ComplexColumnVector beta (n);
  Complex *pbeta = beta.fortran_vec ();

  octave_idx_type nvr = calc_ev ? n : 0;
  ComplexMatrix vtmp (nvr, nvr);
  Complex *pv = vtmp.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 8*n;
  Array<double> rwork (dim_vector (lrwork, 1));
  double *prwork = rwork.fortran_vec ();

  Complex *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (zggev, ZGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           n, atmp_data, n, btmp_data, n,
                           palpha, pbeta, dummy, idummy,
                           pv, n, &dummy_work, lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<Complex> work (dim_vector (lwork, 1));
      Complex *pwork = work.fortran_vec ();

      F77_XFCN (zggev, ZGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               n, atmp_data, n, btmp_data, n,
                               palpha, pbeta, dummy, idummy,
                               pv, n, pwork, lwork, prwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in zggev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("zggev failed to converge");
          return info;
        }

      lambda.resize (n);

      for (octave_idx_type j = 0; j < n; j++)
        lambda.elem (j) = alpha.elem (j) / beta.elem (j);

      v = vtmp;
    }
  else
    (*current_liboctave_error_handler) ("zggev workspace query failed");

  return info;
}

octave_idx_type
EIG::hermitian_init (const ComplexMatrix& a, const ComplexMatrix& b,
                     bool calc_ev)
{
  octave_idx_type n = a.rows ();
  octave_idx_type nb = b.rows ();

  if (n != a.cols () || nb != b.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  if (n != nb)
    {
      (*current_liboctave_error_handler) ("EIG requires same size matrices");
      return -1;
    }

  octave_idx_type info = 0;

  ComplexMatrix atmp = a;
  Complex *atmp_data = atmp.fortran_vec ();

  ComplexMatrix btmp = b;
  Complex *btmp_data = btmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 3*n;
  Array<double> rwork (dim_vector (lrwork, 1));
  double *prwork = rwork.fortran_vec ();

  F77_XFCN (zhegv, ZHEGV, (1, F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, atmp_data, n,
                           btmp_data, n,
                           pwr, &dummy_work, lwork,
                           prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<Complex> work (dim_vector (lwork, 1));
      Complex *pwork = work.fortran_vec ();

      F77_XFCN (zhegv, ZHEGV, (1, F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               F77_CONST_CHAR_ARG2 ("U", 1),
                               n, atmp_data, n,
                               btmp_data, n,
                               pwr, pwork, lwork, prwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in zhegv");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("zhegv failed to converge");
          return info;
        }

      lambda = ComplexColumnVector (wr);
      v = calc_ev ? ComplexMatrix (atmp) : ComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("zhegv workspace query failed");

  return info;
}
