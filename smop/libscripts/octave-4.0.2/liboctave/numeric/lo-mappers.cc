/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#include <cfloat>

#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-math.h"
#include "lo-specfun.h"
#include "lo-utils.h"
#include "oct-cmplx.h"

#include "f77-fcn.h"

// double -> double mappers.

// Both xtrunc and xround belong here so we can keep gnulib:: out of
// lo-mappers.h.

double
xtrunc (double x)
{
  return gnulib::trunc (x);
}

double
xcopysign (double x, double y)
{
  return gnulib::copysign (x, y);
}

double xfloor (double x)
{
  return gnulib::floor (x);
}

double
xround (double x)
{
  return gnulib::round (x);
}

double
xroundb (double x)
{
  double t = xround (x);

  if (fabs (x - t) == 0.5)
    t = 2 * xtrunc (0.5 * t);

  return t;
}

double
signum (double x)
{
  double tmp = 0.0;

  if (x < 0.0)
    tmp = -1.0;
  else if (x > 0.0)
    tmp = 1.0;

  return xisnan (x) ? octave_NaN : tmp;
}

double
xlog2 (double x)
{
  return gnulib::log2 (x);
}

Complex
xlog2 (const Complex& x)
{
#if defined (M_LN2)
  static double ln2 = M_LN2;
#else
  static double ln2 = gnulib::log (2);
#endif

  return std::log (x) / ln2;
}

double
xexp2 (double x)
{
#if defined (HAVE_EXP2)
  return exp2 (x);
#else
#if defined (M_LN2)
  static double ln2 = M_LN2;
#else
  static double ln2 = gnulib::log (2);
#endif

  return exp (x * ln2);
#endif
}

double
xlog2 (double x, int& exp)
{
  return gnulib::frexp (x, &exp);
}

Complex
xlog2 (const Complex& x, int& exp)
{
  double ax = std::abs (x);
  double lax = xlog2 (ax, exp);
  return (ax != lax) ? (x / ax) * lax : x;
}

// double -> bool mappers.

#if ! defined(HAVE_CMATH_ISNAN)
bool
xisnan (double x)
{
  return lo_ieee_isnan (x);
}
#endif

#if ! defined(HAVE_CMATH_ISFINITE)
bool
xfinite (double x)
{
  return lo_ieee_finite (x);
}
#endif

#if ! defined(HAVE_CMATH_ISINF)
bool
xisinf (double x)
{
  return lo_ieee_isinf (x);
}
#endif

bool
octave_is_NA (double x)
{
  return lo_ieee_is_NA (x);
}

// (double, double) -> double mappers.

// complex -> complex mappers.

Complex
acos (const Complex& x)
{
  static Complex i (0, 1);

  Complex tmp;

  if (imag (x) == 0.0)
    {
      // If the imaginary part of X is 0, then avoid generating an
      // imaginary part of -0 for the expression 1-x*x.
      // This effectively chooses the same phase of the branch cut as Matlab.
      double xr = real (x);
      tmp = Complex (1.0 - xr*xr);
    }
  else
    tmp = 1.0 - x*x;

  return -i * log (x + i * sqrt (tmp));
}

Complex
acosh (const Complex& x)
{
  return log (x + sqrt (x + 1.0) * sqrt (x - 1.0));
}

Complex
asin (const Complex& x)
{
  static Complex i (0, 1);

  Complex tmp;

  if (imag (x) == 0.0)
    {
      // If the imaginary part of X is 0, then avoid generating an
      // imaginary part of -0 for the expression 1-x*x.
      // This effectively chooses the same phase of the branch cut as Matlab.
      double xr = real (x);
      tmp = Complex (1.0 - xr*xr);
    }
  else
    tmp = 1.0 - x*x;

  return -i * log (i*x + sqrt (tmp));
}

Complex
asinh (const Complex& x)
{
  return log (x + sqrt (x*x + 1.0));
}

Complex
atan (const Complex& x)
{
  static Complex i (0, 1);

  return i * log ((i + x) / (i - x)) / 2.0;
}

Complex
atanh (const Complex& x)
{
  return log ((1.0 + x) / (1.0 - x)) / 2.0;
}

// complex -> bool mappers.

bool
octave_is_NA (const Complex& x)
{
  return (octave_is_NA (real (x)) || octave_is_NA (imag (x)));
}

bool
octave_is_NaN_or_NA (const Complex& x)
{
  return (xisnan (real (x)) || xisnan (imag (x)));
}

// (complex, complex) -> complex mappers.

// FIXME: need to handle NA too?

Complex
xmin (const Complex& x, const Complex& y)
{
  return abs (x) <= abs (y) ? x : (xisnan (x) ? x : y);
}

Complex
xmax (const Complex& x, const Complex& y)
{
  return abs (x) >= abs (y) ? x : (xisnan (x) ? x : y);
}


// float -> float mappers.

// Both xtrunc and xround belong here so we can keep gnulib:: out of
// lo-mappers.h.

float
xtrunc (float x)
{
  return gnulib::truncf (x);
}

float
xcopysign (float x, float y)
{
  return gnulib::copysignf (x, y);
}

float xfloor (float x)
{
  return gnulib::floorf (x);
}

float
xround (float x)
{
  return gnulib::roundf (x);
}

float
xroundb (float x)
{
  float t = xround (x);

  if (fabsf (x - t) == 0.5)
    t = 2 * xtrunc (0.5 * t);

  return t;
}

float
signum (float x)
{
  float tmp = 0.0;

  if (x < 0.0)
    tmp = -1.0;
  else if (x > 0.0)
    tmp = 1.0;

  return xisnan (x) ? octave_Float_NaN : tmp;
}

float
xlog2 (float x)
{
  return gnulib::log2f (x);
}

FloatComplex
xlog2 (const FloatComplex& x)
{
#if defined (M_LN2)
  static float ln2 = M_LN2;
#else
  static float ln2 = log (2);
#endif

  return std::log (x) / ln2;
}

float
xexp2 (float x)
{
#if defined (HAVE_EXP2F)
  return exp2f (x);
#elif defined (HAVE_EXP2)
  return exp2 (x);
#else
#if defined (M_LN2)
  static float ln2 = M_LN2;
#else
  static float ln2 = log2 (2);
#endif

  return exp (x * ln2);
#endif
}

float
xlog2 (float x, int& exp)
{
  return gnulib::frexpf (x, &exp);
}

FloatComplex
xlog2 (const FloatComplex& x, int& exp)
{
  float ax = std::abs (x);
  float lax = xlog2 (ax, exp);
  return (ax != lax) ? (x / ax) * lax : x;
}

// float -> bool mappers.

#if ! defined(HAVE_CMATH_ISNANF)
bool
xisnan (float x)
{
  return lo_ieee_isnan (x);
}
#endif

#if ! defined(HAVE_CMATH_ISFINITEF)
bool
xfinite (float x)
{
  return lo_ieee_finite (x);
}
#endif

#if ! defined(HAVE_CMATH_ISINFF)
bool
xisinf (float x)
{
  return lo_ieee_isinf (x);
}
#endif

bool
octave_is_NA (float x)
{
  return lo_ieee_is_NA (x);
}

// (float, float) -> float mappers.

// complex -> complex mappers.

FloatComplex
acos (const FloatComplex& x)
{
  static FloatComplex i (0, 1);

  FloatComplex tmp;

  if (imag (x) == 0.0f)
    {
      // If the imaginary part of X is 0, then avoid generating an
      // imaginary part of -0 for the expression 1-x*x.
      // This effectively chooses the same phase of the branch cut as Matlab.
      float xr = real (x);
      tmp = FloatComplex (1.0f - xr*xr);
    }
  else
    tmp = 1.0f - x*x;

  return -i * log (x + i * sqrt (tmp));
}

FloatComplex
acosh (const FloatComplex& x)
{
  return log (x + sqrt (x + 1.0f) * sqrt (x - 1.0f));
}

FloatComplex
asin (const FloatComplex& x)
{
  static FloatComplex i (0, 1);

  FloatComplex tmp;

  if (imag (x) == 0.0f)
    {
      // If the imaginary part of X is 0, then avoid generating an
      // imaginary part of -0 for the expression 1-x*x.
      // This effectively chooses the same phase of the branch cut as Matlab.
      float xr = real (x);
      tmp = FloatComplex (1.0f - xr*xr);
    }
  else
    tmp = 1.0f - x*x;

  return -i * log (i*x + sqrt (tmp));
}

FloatComplex
asinh (const FloatComplex& x)
{
  return log (x + sqrt (x*x + 1.0f));
}

FloatComplex
atan (const FloatComplex& x)
{
  static FloatComplex i (0, 1);

  return i * log ((i + x) / (i - x)) / 2.0f;
}

FloatComplex
atanh (const FloatComplex& x)
{
  return log ((1.0f + x) / (1.0f - x)) / 2.0f;
}

// complex -> bool mappers.

bool
octave_is_NA (const FloatComplex& x)
{
  return (octave_is_NA (real (x)) || octave_is_NA (imag (x)));
}

bool
octave_is_NaN_or_NA (const FloatComplex& x)
{
  return (xisnan (real (x)) || xisnan (imag (x)));
}

// (complex, complex) -> complex mappers.

// FIXME: need to handle NA too?

FloatComplex
xmin (const FloatComplex& x, const FloatComplex& y)
{
  return abs (x) <= abs (y) ? x : (xisnan (x) ? x : y);
}

FloatComplex
xmax (const FloatComplex& x, const FloatComplex& y)
{
  return abs (x) >= abs (y) ? x : (xisnan (x) ? x : y);
}

Complex
rc_acos (double x)
{
  return fabs (x) > 1.0 ? acos (Complex (x)) : Complex (acos (x));
}

FloatComplex
rc_acos (float x)
{
  return fabsf (x) > 1.0f ? acos (FloatComplex (x)) : FloatComplex (acosf (x));
}

Complex
rc_acosh (double x)
{
  return x < 1.0 ? acosh (Complex (x)) : Complex (acosh (x));
}

FloatComplex
rc_acosh (float x)
{
  return x < 1.0f ? acosh (FloatComplex (x)) : FloatComplex (acoshf (x));
}

Complex
rc_asin (double x)
{
  return fabs (x) > 1.0 ? asin (Complex (x)) : Complex (asin (x));
}

FloatComplex
rc_asin (float x)
{
  return fabsf (x) > 1.0f ? asin (FloatComplex (x)) : FloatComplex (asinf (x));
}

Complex
rc_atanh (double x)
{
  return fabs (x) > 1.0 ? atanh (Complex (x)) : Complex (atanh (x));
}

FloatComplex
rc_atanh (float x)
{
  return fabsf (x) > 1.0f ? atanh (FloatComplex (x))
                          : FloatComplex (atanhf (x));
}

Complex
rc_log (double x)
{
  const double pi = 3.14159265358979323846;
  return x < 0.0 ? Complex (gnulib::log (-x), pi) : Complex (gnulib::log (x));
}

FloatComplex
rc_log (float x)
{
  const float pi = 3.14159265358979323846f;
  return (x < 0.0f
          ? FloatComplex (gnulib::logf (-x), pi)
          : FloatComplex (gnulib::logf (x)));
}

Complex
rc_log2 (double x)
{
  const double pil2 = 4.53236014182719380962; // = pi / log(2)
  return x < 0.0 ? Complex (xlog2 (-x), pil2) : Complex (xlog2 (x));
}

FloatComplex
rc_log2 (float x)
{
  const float pil2 = 4.53236014182719380962f; // = pi / log(2)
  return x < 0.0f ? FloatComplex (xlog2 (-x), pil2) : FloatComplex (xlog2 (x));
}

Complex
rc_log10 (double x)
{
  const double pil10 = 1.36437635384184134748; // = pi / log(10)
  return x < 0.0 ? Complex (log10 (-x), pil10) : Complex (log10 (x));
}

FloatComplex
rc_log10 (float x)
{
  const float pil10 = 1.36437635384184134748f; // = pi / log(10)
  return x < 0.0f ? FloatComplex (log10 (-x), pil10)
                  : FloatComplex (log10f (x));
}

Complex
rc_sqrt (double x)
{
  return x < 0.0 ? Complex (0.0, sqrt (-x)) : Complex (sqrt (x));
}

FloatComplex
rc_sqrt (float x)
{
  return x < 0.0f ? FloatComplex (0.0f, sqrtf (-x)) : FloatComplex (sqrtf (x));
}

bool
xnegative_sign (double x)
{
  return __lo_ieee_signbit (x);
}

bool
xnegative_sign (float x)
{
  return __lo_ieee_float_signbit (x);
}

// Convert X to the nearest integer value.  Should not pass NaN to
// this function.

// Sometimes you need a large integer, but not always.

octave_idx_type
NINTbig (double x)
{
  if (x > std::numeric_limits<octave_idx_type>::max ())
    return std::numeric_limits<octave_idx_type>::max ();
  else if (x < std::numeric_limits<octave_idx_type>::min ())
    return std::numeric_limits<octave_idx_type>::min ();
  else
    return static_cast<octave_idx_type> ((x > 0) ? (x + 0.5) : (x - 0.5));
}

octave_idx_type
NINTbig (float x)
{
  if (x > std::numeric_limits<octave_idx_type>::max ())
    return std::numeric_limits<octave_idx_type>::max ();
  else if (x < std::numeric_limits<octave_idx_type>::min ())
    return std::numeric_limits<octave_idx_type>::min ();
  else
    return static_cast<octave_idx_type> ((x > 0) ? (x + 0.5) : (x - 0.5));
}

int
NINT (double x)
{
  if (x > std::numeric_limits<int>::max ())
    return std::numeric_limits<int>::max ();
  else if (x < std::numeric_limits<int>::min ())
    return std::numeric_limits<int>::min ();
  else
    return static_cast<int> ((x > 0) ? (x + 0.5) : (x - 0.5));
}

int
NINT (float x)
{
  if (x > std::numeric_limits<int>::max ())
    return std::numeric_limits<int>::max ();
  else if (x < std::numeric_limits<int>::min ())
    return std::numeric_limits<int>::min ();
  else
    return static_cast<int> ((x > 0) ? (x + 0.5) : (x - 0.5));
}
