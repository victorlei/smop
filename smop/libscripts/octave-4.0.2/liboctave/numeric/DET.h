/*

Copyright (C) 2008-2015 Jaroslav Hajek

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

#if !defined (octave_DET_h)
#define octave_DET_h 1

#include <cmath>
#include "oct-cmplx.h"
#include "lo-mappers.h"

template <class T>
class
base_det
{
public:

  base_det (T c = 1, int e = 0)
    : c2 (), e2 ()
  {
    c2 = xlog2 (c, e2);
    e2 += e;
  }

  base_det (T c, double e, double b)
    : c2 (), e2 ()
  {
    e *= xlog2 (b);
    e2 = e;
    c *= xexp2 (e - e2);
    int f;
    c2 = xlog2 (c, f);
    e2 += f;
  }

  base_det (const base_det& a) : c2 (a.c2), e2 (a.e2) { }

  base_det& operator = (const base_det& a)
  {
    c2 = a.c2;
    e2 = a.e2;
    return *this;
  }

  T coef (void) const { return c2; }
  int exp (void) const { return e2; }

  T value () const { return c2 * static_cast<T> (std::ldexp (1.0, e2)); }
  operator T () const { return value (); }

  base_det square () const { return base_det (c2*c2, e2+e2); }

  void operator *= (T t)
  {
    int e;
    c2 *= xlog2 (t, e);
    e2 += e;
  }

private:

  T c2;
  int e2;
};

// Provide the old types by typedefs.
typedef base_det<double> DET;
typedef base_det<float> FloatDET;
typedef base_det<Complex> ComplexDET;
typedef base_det<FloatComplex> FloatComplexDET;

#endif
