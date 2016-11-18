/*

Copyright (C) 1995-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.

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

#if !defined (octave_oct_cmplx_h)
#define octave_oct_cmplx_h 1

#include <complex>

typedef std::complex<double> Complex;
typedef std::complex<float> FloatComplex;

// For complex-complex and complex-real comparisons, we use the following
// ordering: compare absolute values first; if they match, compare phase angles.
// This is partially inconsistent with M*b, which compares complex numbers only
// by their real parts; OTOH, it uses the same definition for max/min and sort.
// The abs/arg comparison is definitely more useful (the other one is emulated
// rather trivially), so let's be consistent and use that all over.

// The standard C library function arg() returns [-pi,pi], which creates a
// non-unique representation for numbers along the negative real axis branch
// cut.  Change this to principal value (-pi,pi] by mapping -pi to pi.

#define DEF_COMPLEXR_COMP(OP, OPS) \
template <class T> \
inline bool operator OP (const std::complex<T>& a, const std::complex<T>& b) \
{ \
  FLOAT_TRUNCATE const T ax = std::abs (a); \
  FLOAT_TRUNCATE const T bx = std::abs (b); \
  if (ax == bx) \
    { \
      FLOAT_TRUNCATE const T ay = std::arg (a); \
      FLOAT_TRUNCATE const T by = std::arg (b); \
      if (ay == static_cast<T> (-M_PI)) \
        { \
          if (by != static_cast<T> (-M_PI)) \
            return static_cast<T> (M_PI) OP by; \
        } \
      else if (by == static_cast<T> (-M_PI)) \
        { \
          return ay OP static_cast<T> (M_PI); \
        } \
      return ay OP by; \
    } \
  else \
    return ax OPS bx; \
} \
template <class T> \
inline bool operator OP (const std::complex<T>& a, T b) \
{ \
  FLOAT_TRUNCATE const T ax = std::abs (a); \
  FLOAT_TRUNCATE const T bx = std::abs (b); \
  if (ax == bx) \
    { \
      FLOAT_TRUNCATE const T ay = std::arg (a); \
      if (ay == static_cast<T> (-M_PI)) \
        return static_cast<T> (M_PI) OP 0; \
      return ay OP 0; \
    } \
  else \
    return ax OPS bx; \
} \
template <class T> \
inline bool operator OP (T a, const std::complex<T>& b) \
{ \
  FLOAT_TRUNCATE const T ax = std::abs (a); \
  FLOAT_TRUNCATE const T bx = std::abs (b); \
  if (ax == bx) \
    { \
      FLOAT_TRUNCATE const T by = std::arg (b); \
      if (by == static_cast<T> (-M_PI)) \
        return 0 OP static_cast<T> (M_PI); \
      return 0 OP by; \
    } \
  else \
    return ax OPS bx; \
}

DEF_COMPLEXR_COMP (>, >)
DEF_COMPLEXR_COMP (<, <)
DEF_COMPLEXR_COMP (<=, <)
DEF_COMPLEXR_COMP (>=, >)

#endif
