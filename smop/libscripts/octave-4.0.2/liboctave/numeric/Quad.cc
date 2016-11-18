/*

Copyright (C) 1993-2015 John W. Eaton

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

#include "Quad.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "quit.h"
#include "sun-utils.h"

static integrand_fcn user_fcn;
static float_integrand_fcn float_user_fcn;

// FIXME: would be nice to not have to have this global variable.
// Nonzero means an error occurred in the calculation of the integrand
// function, and the user wants us to quit.
int quad_integration_error = 0;

typedef octave_idx_type (*quad_fcn_ptr) (double*, int&, double*);
typedef octave_idx_type (*quad_float_fcn_ptr) (float*, int&, float*);

extern "C"
{
  F77_RET_T
  F77_FUNC (dqagp, DQAGP) (quad_fcn_ptr, const double&, const double&,
                           const octave_idx_type&, const double*,
                           const double&, const double&, double&,
                           double&, octave_idx_type&, octave_idx_type&,
                           const octave_idx_type&, const octave_idx_type&,
                           octave_idx_type&, octave_idx_type*, double*);

  F77_RET_T
  F77_FUNC (dqagi, DQAGI) (quad_fcn_ptr, const double&,
                           const octave_idx_type&, const double&,
                           const double&, double&, double&,
                           octave_idx_type&, octave_idx_type&,
                           const octave_idx_type&, const octave_idx_type&,
                           octave_idx_type&, octave_idx_type*, double*);

  F77_RET_T
  F77_FUNC (qagp, QAGP) (quad_float_fcn_ptr, const float&, const float&,
                         const octave_idx_type&, const float*, const float&,
                         const float&, float&, float&, octave_idx_type&,
                         octave_idx_type&, const octave_idx_type&,
                         const octave_idx_type&, octave_idx_type&,
                         octave_idx_type*, float*);

  F77_RET_T
  F77_FUNC (qagi, QAGI) (quad_float_fcn_ptr, const float&,
                         const octave_idx_type&, const float&,
                         const float&, float&, float&, octave_idx_type&,
                         octave_idx_type&, const octave_idx_type&,
                         const octave_idx_type&, octave_idx_type&,
                         octave_idx_type*, float*);
}

static octave_idx_type
user_function (double *x, int& ierr, double *result)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

#if defined (__sparc) && defined (__GNUC__)
  double xx = access_double (x);
#else
  double xx = *x;
#endif

  quad_integration_error = 0;

  double xresult = (*user_fcn) (xx);

#if defined (__sparc) && defined (__GNUC__)
  assign_double (result, xresult);
#else
  *result = xresult;
#endif

  if (quad_integration_error)
    ierr = -1;

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

static octave_idx_type
float_user_function (float *x, int& ierr, float *result)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  quad_integration_error = 0;

  *result = (*float_user_fcn) (*x);

  if (quad_integration_error)
    ierr = -1;

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

double
DefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval,
                       double& abserr)
{
  octave_idx_type npts = singularities.capacity () + 2;
  double *points = singularities.fortran_vec ();
  double result = 0.0;

  octave_idx_type leniw = 183*npts - 122;
  Array<octave_idx_type> iwork (dim_vector (leniw, 1));
  octave_idx_type *piwork = iwork.fortran_vec ();

  octave_idx_type lenw = 2*leniw - npts;
  Array<double> work (dim_vector (lenw, 1));
  double *pwork = work.fortran_vec ();

  user_fcn = f;
  octave_idx_type last;

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  F77_XFCN (dqagp, DQAGP, (user_function, lower_limit, upper_limit,
                           npts, points, abs_tol, rel_tol, result,
                           abserr, neval, ier, leniw, lenw, last,
                           piwork, pwork));

  return result;
}

float
DefQuad::do_integrate (octave_idx_type&, octave_idx_type&, float&)
{
  (*current_liboctave_error_handler) ("incorrect integration function called");
  return 0.0;
}

double
IndefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval,
                         double& abserr)
{
  double result = 0.0;

  octave_idx_type leniw = 128;
  Array<octave_idx_type> iwork (dim_vector (leniw, 1));
  octave_idx_type *piwork = iwork.fortran_vec ();

  octave_idx_type lenw = 8*leniw;
  Array<double> work (dim_vector (lenw, 1));
  double *pwork = work.fortran_vec ();

  user_fcn = f;
  octave_idx_type last;

  octave_idx_type inf;
  switch (type)
    {
    case bound_to_inf:
      inf = 1;
      break;

    case neg_inf_to_bound:
      inf = -1;
      break;

    case doubly_infinite:
      inf = 2;
      break;

    default:
      assert (0);
      break;
    }

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  F77_XFCN (dqagi, DQAGI, (user_function, bound, inf, abs_tol, rel_tol,
                           result, abserr, neval, ier, leniw, lenw,
                           last, piwork, pwork));

  return result;
}

float
IndefQuad::do_integrate (octave_idx_type&, octave_idx_type&, float&)
{
  (*current_liboctave_error_handler) ("incorrect integration function called");
  return 0.0;
}

double
FloatDefQuad::do_integrate (octave_idx_type&, octave_idx_type&, double&)
{
  (*current_liboctave_error_handler) ("incorrect integration function called");
  return 0.0;
}

float
FloatDefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval,
                            float& abserr)
{
  octave_idx_type npts = singularities.capacity () + 2;
  float *points = singularities.fortran_vec ();
  float result = 0.0;

  octave_idx_type leniw = 183*npts - 122;
  Array<octave_idx_type> iwork (dim_vector (leniw, 1));
  octave_idx_type *piwork = iwork.fortran_vec ();

  octave_idx_type lenw = 2*leniw - npts;
  Array<float> work (dim_vector (lenw, 1));
  float *pwork = work.fortran_vec ();

  float_user_fcn = ff;
  octave_idx_type last;

  float abs_tol = single_precision_absolute_tolerance ();
  float rel_tol = single_precision_relative_tolerance ();

  F77_XFCN (qagp, QAGP, (float_user_function, lower_limit, upper_limit,
                         npts, points, abs_tol, rel_tol, result,
                         abserr, neval, ier, leniw, lenw, last,
                         piwork, pwork));

  return result;
}

double
FloatIndefQuad::do_integrate (octave_idx_type&, octave_idx_type&, double&)
{
  (*current_liboctave_error_handler) ("incorrect integration function called");
  return 0.0;
}

float
FloatIndefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval,
                              float& abserr)
{
  float result = 0.0;

  octave_idx_type leniw = 128;
  Array<octave_idx_type> iwork (dim_vector (leniw, 1));
  octave_idx_type *piwork = iwork.fortran_vec ();

  octave_idx_type lenw = 8*leniw;
  Array<float> work (dim_vector (lenw, 1));
  float *pwork = work.fortran_vec ();

  float_user_fcn = ff;
  octave_idx_type last;

  octave_idx_type inf;
  switch (type)
    {
    case bound_to_inf:
      inf = 1;
      break;

    case neg_inf_to_bound:
      inf = -1;
      break;

    case doubly_infinite:
      inf = 2;
      break;

    default:
      assert (0);
      break;
    }

  float abs_tol = single_precision_absolute_tolerance ();
  float rel_tol = single_precision_relative_tolerance ();

  F77_XFCN (qagi, QAGI, (float_user_function, bound, inf, abs_tol, rel_tol,
                         result, abserr, neval, ier, leniw, lenw,
                         last, piwork, pwork));

  return result;
}
