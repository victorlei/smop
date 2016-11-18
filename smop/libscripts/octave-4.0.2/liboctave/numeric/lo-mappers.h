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

#if !defined (octave_lo_mappers_h)
#define octave_lo_mappers_h 1

#include <limits>

#include "oct-cmplx.h"
#include "lo-math.h"

// Double Precision
extern OCTAVE_API double xtrunc (double x);
extern OCTAVE_API double xcopysign (double x, double y);
inline double xceil (double x) { return ceil (x); }
extern OCTAVE_API double xfloor (double x);
inline double arg (double x) { return atan2 (0.0, x); }
inline double conj (double x) { return x; }
inline double fix (double x) { return xtrunc (x); }
inline double imag (double) { return 0.0; }
inline double real (double x) { return x; }
extern OCTAVE_API double xround (double x);
extern OCTAVE_API double xroundb (double x);
extern OCTAVE_API double signum (double x);
extern OCTAVE_API double xlog2 (double x);
extern OCTAVE_API Complex xlog2 (const Complex& x);
extern OCTAVE_API double xlog2 (double x, int& exp);
extern OCTAVE_API Complex xlog2 (const Complex& x, int& exp);
extern OCTAVE_API double xexp2 (double x);

// These are used by the BOOL_OP macros in mx-op-defs.h.
inline bool xisnan (bool) { return false; }
inline bool xisnan (char) { return false; }

#if defined (HAVE_CMATH_ISNAN)
inline bool xisnan (double x)
{ return std::isnan (x); }
#else
extern OCTAVE_API bool xisnan (double x);
#endif
#if defined (HAVE_CMATH_ISFINITE)
inline bool xfinite (double x)
{ return std::isfinite (x); }
#else
extern OCTAVE_API bool xfinite (double x);
#endif
#if defined (HAVE_CMATH_ISINF)
inline bool xisinf (double x)
{ return std::isinf (x); }
#else
extern OCTAVE_API bool xisinf (double x);
#endif

extern OCTAVE_API bool octave_is_NA (double x);

// Generic xmin, xmax definitions
template <class T>
inline T xmin (T x, T y)
{
  return x <= y ? x : y;
}

template <class T>
inline T xmax (T x, T y)
{
  return x >= y ? x : y;
}

// This form is favorable. GCC will translate (x <= y ? x : y) without a
// jump, hence the only conditional jump involved will be the first
// (xisnan), infrequent and hence friendly to branch prediction.
inline double
xmin (double x, double y)
{
  return xisnan (y) ? x : (x <= y ? x : y);
}

inline double
xmax (double x, double y)
{
  return xisnan (y) ? x : (x >= y ? x : y);
}

extern OCTAVE_API Complex acos (const Complex& x);
extern OCTAVE_API Complex acosh (const Complex& x);
extern OCTAVE_API Complex asin (const Complex& x);
extern OCTAVE_API Complex asinh (const Complex& x);
extern OCTAVE_API Complex atan (const Complex& x);
extern OCTAVE_API Complex atanh (const Complex& x);

extern OCTAVE_API bool octave_is_NA (const Complex& x);
extern OCTAVE_API bool octave_is_NaN_or_NA (const Complex& x);

extern OCTAVE_API Complex xmin (const Complex& x, const Complex& y);
extern OCTAVE_API Complex xmax (const Complex& x, const Complex& y);

// Single Precision
extern OCTAVE_API float xtrunc (float x);
extern OCTAVE_API float xcopysign (float x, float y);
inline float xceil (float x) { return ceilf (x); }
extern OCTAVE_API float xfloor (float x);
inline float arg (float x) { return atan2f (0.0f, x); }
inline float conj (float x) { return x; }
inline float fix (float x) { return xtrunc (x); }
inline float imag (float) { return 0.0f; }
inline float real (float x) { return x; }
extern OCTAVE_API float xround (float x);
extern OCTAVE_API float xroundb (float x);
extern OCTAVE_API float signum (float x);
extern OCTAVE_API float xlog2 (float x);
extern OCTAVE_API FloatComplex xlog2 (const FloatComplex& x);
extern OCTAVE_API float xlog2 (float x, int& exp);
extern OCTAVE_API FloatComplex xlog2 (const FloatComplex& x, int& exp);
extern OCTAVE_API float xexp2 (float x);

#if defined (HAVE_CMATH_ISNANF)
inline bool xisnan (float x)
{ return std::isnan (x); }
#else
extern OCTAVE_API bool xisnan (float x);
#endif
#if defined (HAVE_CMATH_ISFINITEF)
inline bool xfinite (float x)
{ return std::isfinite (x); }
#else
extern OCTAVE_API bool xfinite (float x);
#endif
#if defined (HAVE_CMATH_ISINFF)
inline bool xisinf (float x)
{ return std::isinf (x); }
#else
extern OCTAVE_API bool xisinf (float x);
#endif

extern OCTAVE_API bool octave_is_NA (float x);

inline float
xmin (float x, float y)
{
  return xisnan (y) ? x : (x <= y ? x : y);
}

inline float
xmax (float x, float y)
{
  return xisnan (y) ? x : (x >= y ? x : y);
}

extern OCTAVE_API FloatComplex acos (const FloatComplex& x);
extern OCTAVE_API FloatComplex acosh (const FloatComplex& x);
extern OCTAVE_API FloatComplex asin (const FloatComplex& x);
extern OCTAVE_API FloatComplex asinh (const FloatComplex& x);
extern OCTAVE_API FloatComplex atan (const FloatComplex& x);
extern OCTAVE_API FloatComplex atanh (const FloatComplex& x);

extern OCTAVE_API bool octave_is_NA (const FloatComplex& x);
extern OCTAVE_API bool octave_is_NaN_or_NA (const FloatComplex& x);

extern OCTAVE_API FloatComplex xmin (const FloatComplex& x,
                                     const FloatComplex& y);
extern OCTAVE_API FloatComplex xmax (const FloatComplex& x,
                                     const FloatComplex& y);

// These map reals to Complex.

extern OCTAVE_API Complex rc_acos (double);
extern OCTAVE_API FloatComplex rc_acos (float);
extern OCTAVE_API Complex rc_acosh (double);
extern OCTAVE_API FloatComplex rc_acosh (float);
extern OCTAVE_API Complex rc_asin (double);
extern OCTAVE_API FloatComplex rc_asin (float);
extern OCTAVE_API Complex rc_atanh (double);
extern OCTAVE_API FloatComplex rc_atanh (float);
extern OCTAVE_API Complex rc_log (double);
extern OCTAVE_API FloatComplex rc_log (float);
extern OCTAVE_API Complex rc_log2 (double);
extern OCTAVE_API FloatComplex rc_log2 (float);
extern OCTAVE_API Complex rc_log10 (double);
extern OCTAVE_API FloatComplex rc_log10 (float);
extern OCTAVE_API Complex rc_sqrt (double);
extern OCTAVE_API FloatComplex rc_sqrt (float);

// Some useful tests, that are commonly repeated.
// Test for a finite integer.
inline bool
xisinteger (double x)
{
  return xfinite (x) && x == xround (x);
}

inline bool
xisinteger (float x)
{
  return xfinite (x) && x == xround (x);
}

// Test for negative sign.
extern OCTAVE_API bool xnegative_sign (double x);
extern OCTAVE_API bool xnegative_sign (float x);

// Test for positive sign.
inline bool xpositive_sign (double x) { return ! xnegative_sign (x); }
inline bool xpositive_sign (float x) { return ! xnegative_sign (x); }

// Some old rounding functions.

extern OCTAVE_API octave_idx_type NINTbig (double x);
extern OCTAVE_API octave_idx_type NINTbig (float x);

extern OCTAVE_API int NINT (double x);
extern OCTAVE_API int NINT (float x);

template <typename T>
T
X_NINT (T x)
{
  return (xfinite (x) ? xfloor (x + 0.5) : x);
}

inline OCTAVE_API double D_NINT (double x) { return X_NINT (x); }
inline OCTAVE_API float F_NINT (float x) { return X_NINT (x); }

// Template functions can have either float or double arguments.

template <typename T>
bool
xisnan (const std::complex<T>& x)
{
  return (xisnan (real (x)) || xisnan (imag (x)));
}

template <typename T>
bool
xfinite (const std::complex<T>& x)
{
  return (xfinite (real (x)) && xfinite (imag (x)));
}

template <typename T>
bool
xisinf (const std::complex<T>& x)
{
  return (xisinf (real (x)) || xisinf (imag (x)));
}

template <typename T>
std::complex<T>
fix (const std::complex<T>& x)
{
  return std::complex<T> (fix (real (x)), fix (imag (x)));
}

template <typename T>
std::complex<T>
ceil (const std::complex<T>& x)
{
  return std::complex<T> (xceil (real (x)), xceil (imag (x)));
}

template <typename T>
std::complex<T>
floor (const std::complex<T>& x)
{
  return std::complex<T> (xfloor (real (x)), xfloor (imag (x)));
}

template <typename T>
std::complex<T>
xround (const std::complex<T>& x)
{
  return std::complex<T> (xround (real (x)), xround (imag (x)));
}

template <typename T>
std::complex<T>
xroundb (const std::complex<T>& x)
{
  return std::complex<T> (xroundb (real (x)), xroundb (imag (x)));
}

template <typename T>
std::complex<T>
signum (const std::complex<T>& x)
{
  T tmp = abs (x);

  return tmp == 0 ? 0.0 : x / tmp;
}

template <typename T>
T
xmod (T x, T y)
{
  T retval;

  if (y == 0)
    retval = x;
  else
    {
      T q = x / y;

      if (X_NINT (y) != y
          && (std::abs ((q - X_NINT (q)) / X_NINT (q))
              < std::numeric_limits<T>::epsilon ()))
        retval = 0;
      else
        {
          T n = xfloor (q);

          // Prevent use of extra precision.
          volatile T tmp = y * n;

          retval = x - tmp;
        }
    }

  if (x != y && y != 0 && retval != 0)
    retval = xcopysign (retval, y);

  return retval;
}

template <typename T>
T
xrem (T x, T y)
{
  T retval;

  if (y == 0)
    retval = x;
  else
    {
      T q = x / y;

      if (X_NINT (y) != y
          && (std::abs ((q - X_NINT (q)) / X_NINT (q))
              < std::numeric_limits<T>::epsilon ()))
        retval = 0;
      else
        {
          T n = xtrunc (q);

          // Prevent use of extra precision.
          volatile T tmp = y * n;

          retval = x - tmp;
        }
    }

  if (x != y && y != 0 && retval != 0)
    retval = xcopysign (retval, x);

  return retval;
}

template <typename T>
T
xsignbit (T x)
{
  return signbit (x);
}

#endif
