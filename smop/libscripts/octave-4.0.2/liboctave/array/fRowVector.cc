// RowVector manipulations.
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

#include <iostream>

#include "Array-util.h"
#include "f77-fcn.h"
#include "functor.h"
#include "lo-error.h"
#include "mx-base.h"
#include "mx-inlines.cc"
#include "oct-cmplx.h"

// Fortran functions we call.

extern "C"
{
  F77_RET_T
  F77_FUNC (sgemv, SGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const float&, const float*,
                           const octave_idx_type&, const float*,
                           const octave_idx_type&, const float&,
                           float*, const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (xsdot, XSDOT) (const octave_idx_type&, const float*,
                           const octave_idx_type&, const float*,
                           const octave_idx_type&, float&);
}

// Row Vector class.

bool
FloatRowVector::operator == (const FloatRowVector& a) const
{
  octave_idx_type len = length ();
  if (len != a.length ())
    return 0;
  return mx_inline_equal (len, data (), a.data ());
}

bool
FloatRowVector::operator != (const FloatRowVector& a) const
{
  return !(*this == a);
}

FloatRowVector&
FloatRowVector::insert (const FloatRowVector& a, octave_idx_type c)
{
  octave_idx_type a_len = a.length ();

  if (c < 0 || c + a_len > length ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (c+i) = a.elem (i);
    }

  return *this;
}

FloatRowVector&
FloatRowVector::fill (float val)
{
  octave_idx_type len = length ();

  if (len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < len; i++)
        xelem (i) = val;
    }

  return *this;
}

FloatRowVector&
FloatRowVector::fill (float val, octave_idx_type c1, octave_idx_type c2)
{
  octave_idx_type len = length ();

  if (c1 < 0 || c2 < 0 || c1 >= len || c2 >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (c1 > c2) { std::swap (c1, c2); }

  if (c2 >= c1)
    {
      make_unique ();

      for (octave_idx_type i = c1; i <= c2; i++)
        xelem (i) = val;
    }

  return *this;
}

FloatRowVector
FloatRowVector::append (const FloatRowVector& a) const
{
  octave_idx_type len = length ();
  octave_idx_type nc_insert = len;
  FloatRowVector retval (len + a.length ());
  retval.insert (*this, 0);
  retval.insert (a, nc_insert);
  return retval;
}

FloatColumnVector
FloatRowVector::transpose (void) const
{
  return MArray<float>::transpose ();
}

FloatRowVector
real (const FloatComplexRowVector& a)
{
  return do_mx_unary_op<float, FloatComplex> (a, mx_inline_real);
}

FloatRowVector
imag (const FloatComplexRowVector& a)
{
  return do_mx_unary_op<float, FloatComplex> (a, mx_inline_imag);
}

FloatRowVector
FloatRowVector::extract (octave_idx_type c1, octave_idx_type c2) const
{
  if (c1 > c2) { std::swap (c1, c2); }

  octave_idx_type new_c = c2 - c1 + 1;

  FloatRowVector result (new_c);

  for (octave_idx_type i = 0; i < new_c; i++)
    result.xelem (i) = elem (c1+i);

  return result;
}

FloatRowVector
FloatRowVector::extract_n (octave_idx_type r1, octave_idx_type n) const
{
  FloatRowVector result (n);

  for (octave_idx_type i = 0; i < n; i++)
    result.xelem (i) = elem (r1+i);

  return result;
}

// row vector by matrix -> row vector

FloatRowVector
operator * (const FloatRowVector& v, const FloatMatrix& a)
{
  FloatRowVector retval;

  octave_idx_type len = v.length ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != len)
    gripe_nonconformant ("operator *", 1, len, a_nr, a_nc);
  else
    {
      if (len == 0)
        retval.resize (a_nc, 0.0);
      else
        {
          // Transpose A to form A'*x == (x'*A)'

          octave_idx_type ld = a_nr;

          retval.resize (a_nc);
          float *y = retval.fortran_vec ();

          F77_XFCN (sgemv, SGEMV, (F77_CONST_CHAR_ARG2 ("T", 1),
                                   a_nr, a_nc, 1.0, a.data (),
                                   ld, v.data (), 1, 0.0, y, 1
                                   F77_CHAR_ARG_LEN (1)));
        }
    }

  return retval;
}

// other operations

float
FloatRowVector::min (void) const
{
  octave_idx_type len = length ();
  if (len == 0)
    return 0;

  float res = elem (0);

  for (octave_idx_type i = 1; i < len; i++)
    if (elem (i) < res)
      res = elem (i);

  return res;
}

float
FloatRowVector::max (void) const
{
  octave_idx_type len = length ();
  if (len == 0)
    return 0;

  float res = elem (0);

  for (octave_idx_type i = 1; i < len; i++)
    if (elem (i) > res)
      res = elem (i);

  return res;
}

std::ostream&
operator << (std::ostream& os, const FloatRowVector& a)
{
//  int field_width = os.precision () + 7;

  for (octave_idx_type i = 0; i < a.length (); i++)
    os << " " /* setw (field_width) */ << a.elem (i);
  return os;
}

std::istream&
operator >> (std::istream& is, FloatRowVector& a)
{
  octave_idx_type len = a.length ();

  if (len > 0)
    {
      float tmp;
      for (octave_idx_type i = 0; i < len; i++)
        {
          is >> tmp;
          if (is)
            a.elem (i) = tmp;
          else
            break;
        }
    }
  return is;
}

// other operations

FloatRowVector
linspace (float x1, float x2, octave_idx_type n)
{
  if (n < 1) n = 1;

  NoAlias<FloatRowVector> retval (n);

  float delta = (x2 - x1) / (n - 1);
  retval(0) = x1;
  for (octave_idx_type i = 1; i < n-1; i++)
    retval(i) = x1 + i*delta;
  retval(n-1) = x2;

  return retval;
}

// row vector by column vector -> scalar

float
operator * (const FloatRowVector& v, const FloatColumnVector& a)
{
  float retval = 0.0;

  octave_idx_type len = v.length ();

  octave_idx_type a_len = a.length ();

  if (len != a_len)
    gripe_nonconformant ("operator *", len, a_len);
  else if (len != 0)
    F77_FUNC (xsdot, XSDOT) (len, v.data (), 1, a.data (), 1, retval);

  return retval;
}

FloatComplex
operator * (const FloatRowVector& v, const FloatComplexColumnVector& a)
{
  FloatComplexRowVector tmp (v);
  return tmp * a;
}
