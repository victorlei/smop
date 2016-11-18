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

#include "fEIG.h"
#include "fColVector.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (sgeev, SGEEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, float*,
                           const octave_idx_type&, float*, float*, float*,
                           const octave_idx_type&, float*,
                           const octave_idx_type&, float*,
                           const octave_idx_type&, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgeev, CGEEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, FloatComplex*, FloatComplex*,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, float*, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ssyev, SSYEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, float*,
                           const octave_idx_type&, float*, float*,
                           const octave_idx_type&, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cheev, CHEEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, float*, FloatComplex*,
                           const octave_idx_type&, float*, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (spotrf, SPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cpotrf, CPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sggev, SGGEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, float*,
                           const octave_idx_type&, float*,
                           const octave_idx_type&, float*, float*, float*,
                           float*, const octave_idx_type&, float*,
                           const octave_idx_type&, float*,
                           const octave_idx_type&, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ssygv, SSYGV) (const octave_idx_type&,
                           F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, float*,
                           const octave_idx_type&, float*,
                           const octave_idx_type&, float*, float*,
                           const octave_idx_type&, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cggev, CGGEV) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, FloatComplex*,
                           FloatComplex*, FloatComplex*,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, float*, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (chegv, CHEGV) (const octave_idx_type&,
                           F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, FloatComplex*,
                           const octave_idx_type&, float*, FloatComplex*,
                           const octave_idx_type&, float*, octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);
}

octave_idx_type
FloatEIG::init (const FloatMatrix& a, bool calc_ev)
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

  FloatMatrix atmp = a;
  float *tmp_data = atmp.fortran_vec ();

  Array<float> wr (dim_vector (n, 1));
  float *pwr = wr.fortran_vec ();

  Array<float> wi (dim_vector (n, 1));
  float *pwi = wi.fortran_vec ();

  volatile octave_idx_type nvr = calc_ev ? n : 0;
  FloatMatrix vr (nvr, nvr);
  float *pvr = vr.fortran_vec ();

  octave_idx_type lwork = -1;
  float dummy_work;

  float *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (sgeev, SGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           n, tmp_data, n, pwr, pwi, dummy,
                           idummy, pvr, n, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<float> work (dim_vector (lwork, 1));
      float *pwork = work.fortran_vec ();

      F77_XFCN (sgeev, SGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               n, tmp_data, n, pwr, pwi, dummy,
                               idummy, pvr, n, pwork, lwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in sgeev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("sgeev failed to converge");
          return info;
        }

      lambda.resize (n);
      v.resize (nvr, nvr);

      for (octave_idx_type j = 0; j < n; j++)
        {
          if (wi.elem (j) == 0.0)
            {
              lambda.elem (j) = FloatComplex (wr.elem (j));
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

              lambda.elem (j) = FloatComplex (wr.elem (j), wi.elem (j));
              lambda.elem (j+1) = FloatComplex (wr.elem (j+1), wi.elem (j+1));

              for (octave_idx_type i = 0; i < nvr; i++)
                {
                  float real_part = vr.elem (i, j);
                  float imag_part = vr.elem (i, j+1);
                  v.elem (i, j) = FloatComplex (real_part, imag_part);
                  v.elem (i, j+1) = FloatComplex (real_part, -imag_part);
                }
              j++;
            }
        }
    }
  else
    (*current_liboctave_error_handler) ("sgeev workspace query failed");

  return info;
}

octave_idx_type
FloatEIG::symmetric_init (const FloatMatrix& a, bool calc_ev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  FloatMatrix atmp = a;
  float *tmp_data = atmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  float dummy_work;

  F77_XFCN (ssyev, SSYEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, tmp_data, n, pwr, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<float> work (dim_vector (lwork, 1));
      float *pwork = work.fortran_vec ();

      F77_XFCN (ssyev, SSYEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               F77_CONST_CHAR_ARG2 ("U", 1),
                               n, tmp_data, n, pwr, pwork, lwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in ssyev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("ssyev failed to converge");
          return info;
        }

      lambda = FloatComplexColumnVector (wr);
      v = calc_ev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("ssyev workspace query failed");

  return info;
}

octave_idx_type
FloatEIG::init (const FloatComplexMatrix& a, bool calc_ev)
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

  FloatComplexMatrix atmp = a;
  FloatComplex *tmp_data = atmp.fortran_vec ();

  FloatComplexColumnVector w (n);
  FloatComplex *pw = w.fortran_vec ();

  octave_idx_type nvr = calc_ev ? n : 0;
  FloatComplexMatrix vtmp (nvr, nvr);
  FloatComplex *pv = vtmp.fortran_vec ();

  octave_idx_type lwork = -1;
  FloatComplex dummy_work;

  octave_idx_type lrwork = 2*n;
  Array<float> rwork (dim_vector (lrwork, 1));
  float *prwork = rwork.fortran_vec ();

  FloatComplex *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (cgeev, CGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           n, tmp_data, n, pw, dummy, idummy,
                           pv, n, &dummy_work, lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<FloatComplex> work (dim_vector (lwork, 1));
      FloatComplex *pwork = work.fortran_vec ();

      F77_XFCN (cgeev, CGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               n, tmp_data, n, pw, dummy, idummy,
                               pv, n, pwork, lwork, prwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in cgeev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("cgeev failed to converge");
          return info;
        }

      lambda = w;
      v = vtmp;
    }
  else
    (*current_liboctave_error_handler) ("cgeev workspace query failed");

  return info;
}

octave_idx_type
FloatEIG::hermitian_init (const FloatComplexMatrix& a, bool calc_ev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  FloatComplexMatrix atmp = a;
  FloatComplex *tmp_data = atmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  FloatComplex dummy_work;

  octave_idx_type lrwork = 3*n;
  Array<float> rwork (dim_vector (lrwork, 1));
  float *prwork = rwork.fortran_vec ();

  F77_XFCN (cheev, CHEEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, tmp_data, n, pwr, &dummy_work, lwork,
                           prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<FloatComplex> work (dim_vector (lwork, 1));
      FloatComplex *pwork = work.fortran_vec ();

      F77_XFCN (cheev, CHEEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               F77_CONST_CHAR_ARG2 ("U", 1),
                               n, tmp_data, n, pwr, pwork, lwork, prwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in cheev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("cheev failed to converge");
          return info;
        }

      lambda = FloatComplexColumnVector (wr);
      v = calc_ev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("cheev workspace query failed");

  return info;
}

octave_idx_type
FloatEIG::init (const FloatMatrix& a, const FloatMatrix& b, bool calc_ev)
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

  FloatMatrix tmp = b;
  float *tmp_data = tmp.fortran_vec ();

  F77_XFCN (spotrf, SPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1),
                             n, tmp_data, n,
                             info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (a.is_symmetric () && b.is_symmetric () && info == 0)
    return symmetric_init (a, b, calc_ev);

  FloatMatrix atmp = a;
  float *atmp_data = atmp.fortran_vec ();

  FloatMatrix btmp = b;
  float *btmp_data = btmp.fortran_vec ();

  Array<float> ar (dim_vector (n, 1));
  float *par = ar.fortran_vec ();

  Array<float> ai (dim_vector (n, 1));
  float *pai = ai.fortran_vec ();

  Array<float> beta (dim_vector (n, 1));
  float *pbeta = beta.fortran_vec ();

  volatile octave_idx_type nvr = calc_ev ? n : 0;
  FloatMatrix vr (nvr, nvr);
  float *pvr = vr.fortran_vec ();

  octave_idx_type lwork = -1;
  float dummy_work;

  float *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (sggev, SGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
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
      Array<float> work (dim_vector (lwork, 1));
      float *pwork = work.fortran_vec ();

      F77_XFCN (sggev, SGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               n, atmp_data, n, btmp_data, n,
                               par, pai, pbeta,
                               dummy, idummy, pvr, n,
                               pwork, lwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in sggev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("sggev failed to converge");
          return info;
        }

      lambda.resize (n);
      v.resize (nvr, nvr);

      for (octave_idx_type j = 0; j < n; j++)
        {
          if (ai.elem (j) == 0.0)
            {
              lambda.elem (j) = FloatComplex (ar.elem (j) / beta.elem (j));
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

              lambda.elem (j) = FloatComplex (ar.elem (j) / beta.elem (j),
                                              ai.elem (j) / beta.elem (j));
              lambda.elem (j+1) = FloatComplex (ar.elem (j+1) / beta.elem (j+1),
                                                ai.elem (j+1) / beta.elem (j+1));

              for (octave_idx_type i = 0; i < nvr; i++)
                {
                  float real_part = vr.elem (i, j);
                  float imag_part = vr.elem (i, j+1);
                  v.elem (i, j) = FloatComplex (real_part, imag_part);
                  v.elem (i, j+1) = FloatComplex (real_part, -imag_part);
                }
              j++;
            }
        }
    }
  else
    (*current_liboctave_error_handler) ("sggev workspace query failed");

  return info;
}

octave_idx_type
FloatEIG::symmetric_init (const FloatMatrix& a, const FloatMatrix& b,
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

  FloatMatrix atmp = a;
  float *atmp_data = atmp.fortran_vec ();

  FloatMatrix btmp = b;
  float *btmp_data = btmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  float dummy_work;

  F77_XFCN (ssygv, SSYGV, (1, F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, atmp_data, n,
                           btmp_data, n,
                           pwr, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<float> work (dim_vector (lwork, 1));
      float *pwork = work.fortran_vec ();

      F77_XFCN (ssygv, SSYGV, (1, F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               F77_CONST_CHAR_ARG2 ("U", 1),
                               n, atmp_data, n,
                               btmp_data, n,
                               pwr, pwork, lwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in ssygv");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("ssygv failed to converge");
          return info;
        }

      lambda = FloatComplexColumnVector (wr);
      v = calc_ev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("ssygv workspace query failed");

  return info;
}

octave_idx_type
FloatEIG::init (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
                bool calc_ev)
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

  FloatComplexMatrix tmp = b;
  FloatComplex *tmp_data = tmp.fortran_vec ();

  F77_XFCN (cpotrf, CPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1),
                             n, tmp_data, n,
                             info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (a.is_hermitian () && b.is_hermitian () && info == 0)
    return hermitian_init (a, calc_ev);

  FloatComplexMatrix atmp = a;
  FloatComplex *atmp_data = atmp.fortran_vec ();

  FloatComplexMatrix btmp = b;
  FloatComplex *btmp_data = btmp.fortran_vec ();

  FloatComplexColumnVector alpha (n);
  FloatComplex *palpha = alpha.fortran_vec ();

  FloatComplexColumnVector beta (n);
  FloatComplex *pbeta = beta.fortran_vec ();

  octave_idx_type nvr = calc_ev ? n : 0;
  FloatComplexMatrix vtmp (nvr, nvr);
  FloatComplex *pv = vtmp.fortran_vec ();

  octave_idx_type lwork = -1;
  FloatComplex dummy_work;

  octave_idx_type lrwork = 8*n;
  Array<float> rwork (dim_vector (lrwork, 1));
  float *prwork = rwork.fortran_vec ();

  FloatComplex *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (cggev, CGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                           n, atmp_data, n, btmp_data, n,
                           palpha, pbeta, dummy, idummy,
                           pv, n, &dummy_work, lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<FloatComplex> work (dim_vector (lwork, 1));
      FloatComplex *pwork = work.fortran_vec ();

      F77_XFCN (cggev, CGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
                               n, atmp_data, n, btmp_data, n,
                               palpha, pbeta, dummy, idummy,
                               pv, n, pwork, lwork, prwork, info
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));

      if (info < 0)
        {
          (*current_liboctave_error_handler) ("unrecoverable error in cggev");
          return info;
        }

      if (info > 0)
        {
          (*current_liboctave_error_handler) ("cggev failed to converge");
          return info;
        }

      lambda.resize (n);

      for (octave_idx_type j = 0; j < n; j++)
        lambda.elem (j) = alpha.elem (j) / beta.elem (j);

      v = vtmp;
    }
  else
    (*current_liboctave_error_handler) ("cggev workspace query failed");

  return info;
}

octave_idx_type
FloatEIG::hermitian_init (const FloatComplexMatrix& a,
                          const FloatComplexMatrix& b, bool calc_ev)
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

  FloatComplexMatrix atmp = a;
  FloatComplex *atmp_data = atmp.fortran_vec ();

  FloatComplexMatrix btmp = b;
  FloatComplex *btmp_data = btmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  FloatComplex dummy_work;

  octave_idx_type lrwork = 3*n;
  Array<float> rwork (dim_vector (lrwork, 1));
  float *prwork = rwork.fortran_vec ();

  F77_XFCN (chegv, CHEGV, (1, F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
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
      Array<FloatComplex> work (dim_vector (lwork, 1));
      FloatComplex *pwork = work.fortran_vec ();

      F77_XFCN (chegv, CHEGV, (1, F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
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

      lambda = FloatComplexColumnVector (wr);
      v = calc_ev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("zhegv workspace query failed");

  return info;
}
