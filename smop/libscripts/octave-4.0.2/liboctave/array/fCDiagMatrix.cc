// DiagMatrix manipulations.
/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

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
#include "lo-error.h"
#include "lo-ieee.h"
#include "mx-base.h"
#include "mx-inlines.cc"
#include "oct-cmplx.h"

// FloatComplex Diagonal Matrix class

FloatComplexDiagMatrix::FloatComplexDiagMatrix (const FloatDiagMatrix& a)
  : MDiagArray2<FloatComplex> (a.rows (), a.cols ())
{
  for (octave_idx_type i = 0; i < length (); i++)
    elem (i, i) = a.elem (i, i);
}

bool
FloatComplexDiagMatrix::operator == (const FloatComplexDiagMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return mx_inline_equal (length (), data (), a.data ());
}

bool
FloatComplexDiagMatrix::operator != (const FloatComplexDiagMatrix& a) const
{
  return !(*this == a);
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (float val)
{
  for (octave_idx_type i = 0; i < length (); i++)
    elem (i, i) = val;
  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatComplex& val)
{
  for (octave_idx_type i = 0; i < length (); i++)
    elem (i, i) = val;
  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (float val,
                              octave_idx_type beg, octave_idx_type end)
{
  if (beg < 0 || end >= length () || end < beg)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = beg; i <= end; i++)
    elem (i, i) = val;

  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatComplex& val,
                              octave_idx_type beg, octave_idx_type end)
{
  if (beg < 0 || end >= length () || end < beg)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = beg; i <= end; i++)
    elem (i, i) = val;

  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatColumnVector& a)
{
  octave_idx_type len = length ();
  if (a.length () != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = 0; i < len; i++)
    elem (i, i) = a.elem (i);

  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatComplexColumnVector& a)
{
  octave_idx_type len = length ();
  if (a.length () != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = 0; i < len; i++)
    elem (i, i) = a.elem (i);

  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatRowVector& a)
{
  octave_idx_type len = length ();
  if (a.length () != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = 0; i < len; i++)
    elem (i, i) = a.elem (i);

  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatComplexRowVector& a)
{
  octave_idx_type len = length ();
  if (a.length () != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = 0; i < len; i++)
    elem (i, i) = a.elem (i);

  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatColumnVector& a, octave_idx_type beg)
{
  octave_idx_type a_len = a.length ();
  if (beg < 0 || beg + a_len >= length ())
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = 0; i < a_len; i++)
    elem (i+beg, i+beg) = a.elem (i);

  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatComplexColumnVector& a,
                              octave_idx_type beg)
{
  octave_idx_type a_len = a.length ();
  if (beg < 0 || beg + a_len >= length ())
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = 0; i < a_len; i++)
    elem (i+beg, i+beg) = a.elem (i);

  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatRowVector& a, octave_idx_type beg)
{
  octave_idx_type a_len = a.length ();
  if (beg < 0 || beg + a_len >= length ())
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = 0; i < a_len; i++)
    elem (i+beg, i+beg) = a.elem (i);

  return *this;
}

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::fill (const FloatComplexRowVector& a,
                              octave_idx_type beg)
{
  octave_idx_type a_len = a.length ();
  if (beg < 0 || beg + a_len >= length ())
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  for (octave_idx_type i = 0; i < a_len; i++)
    elem (i+beg, i+beg) = a.elem (i);

  return *this;
}

FloatDiagMatrix
FloatComplexDiagMatrix::abs (void) const
{
  return FloatDiagMatrix (extract_diag ().abs (), rows (), columns ());
}

FloatComplexDiagMatrix
conj (const FloatComplexDiagMatrix& a)
{
  return FloatComplexDiagMatrix (conj (a.extract_diag ()), a.rows (),
                                 a.columns ());
}

// resize is the destructive analog for this one

FloatComplexMatrix
FloatComplexDiagMatrix::extract (octave_idx_type r1, octave_idx_type c1,
                                 octave_idx_type r2, octave_idx_type c2) const
{
  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  octave_idx_type new_r = r2 - r1 + 1;
  octave_idx_type new_c = c2 - c1 + 1;

  FloatComplexMatrix result (new_r, new_c);

  for (octave_idx_type j = 0; j < new_c; j++)
    for (octave_idx_type i = 0; i < new_r; i++)
      result.elem (i, j) = elem (r1+i, c1+j);

  return result;
}

// extract row or column i.

FloatComplexRowVector
FloatComplexDiagMatrix::row (octave_idx_type i) const
{
  octave_idx_type r = rows ();
  octave_idx_type c = cols ();
  if (i < 0 || i >= r)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return FloatComplexRowVector ();
    }

  FloatComplexRowVector retval (c, 0.0);
  if (r <= c || (r > c && i < c))
    retval.elem (i) = elem (i, i);

  return retval;
}

FloatComplexRowVector
FloatComplexDiagMatrix::row (char *s) const
{
  if (! s)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return FloatComplexRowVector ();
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return row (static_cast<octave_idx_type>(0));
  else if (c == 'l' || c == 'L')
    return row (rows () - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return FloatComplexRowVector ();
    }
}

FloatComplexColumnVector
FloatComplexDiagMatrix::column (octave_idx_type i) const
{
  octave_idx_type r = rows ();
  octave_idx_type c = cols ();
  if (i < 0 || i >= c)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return FloatComplexColumnVector ();
    }

  FloatComplexColumnVector retval (r, 0.0);
  if (r >= c || (r < c && i < r))
    retval.elem (i) = elem (i, i);

  return retval;
}

FloatComplexColumnVector
FloatComplexDiagMatrix::column (char *s) const
{
  if (! s)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return FloatComplexColumnVector ();
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return column (static_cast<octave_idx_type>(0));
  else if (c == 'l' || c == 'L')
    return column (cols () - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return FloatComplexColumnVector ();
    }
}

FloatComplexDiagMatrix
FloatComplexDiagMatrix::inverse (void) const
{
  octave_idx_type info;
  return inverse (info);
}

FloatComplexDiagMatrix
FloatComplexDiagMatrix::inverse (octave_idx_type& info) const
{
  octave_idx_type r = rows ();
  octave_idx_type c = cols ();
  if (r != c)
    {
      (*current_liboctave_error_handler) ("inverse requires square matrix");
      return FloatComplexDiagMatrix ();
    }

  FloatComplexDiagMatrix retval (r, c);

  info = 0;
  for (octave_idx_type i = 0; i < length (); i++)
    {
      if (elem (i, i) == 0.0f)
        {
          info = -1;
          return *this;
        }
      else
        retval.elem (i, i) = 1.0f / elem (i, i);
    }

  return retval;
}

FloatComplexDiagMatrix
FloatComplexDiagMatrix::pseudo_inverse (float tol) const
{
  octave_idx_type r = rows ();
  octave_idx_type c = cols ();
  octave_idx_type len = length ();

  FloatComplexDiagMatrix retval (c, r);

  for (octave_idx_type i = 0; i < len; i++)
    {
      float val = std::abs (elem (i, i));
      if (val < tol || val == 0.0f)
        retval.elem (i, i) = 0.0f;
      else
        retval.elem (i, i) = 1.0f / elem (i, i);
    }

  return retval;
}

bool
FloatComplexDiagMatrix::all_elements_are_real (void) const
{
  return mx_inline_all_real (length (), data ());
}

// diagonal matrix by diagonal matrix -> diagonal matrix operations

FloatComplexDiagMatrix&
FloatComplexDiagMatrix::operator += (const FloatDiagMatrix& a)
{
  octave_idx_type r = rows ();
  octave_idx_type c = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (r != a_nr || c != a_nc)
    {
      gripe_nonconformant ("operator +=", r, c, a_nr, a_nc);
      return *this;
    }

  if (r == 0 || c == 0)
    return *this;

  FloatComplex *d = fortran_vec (); // Ensures only 1 reference to my privates!

  mx_inline_add2 (length (), d, a.data ());
  return *this;
}

FloatComplexDiagMatrix
operator * (const FloatComplexDiagMatrix& a, const FloatDiagMatrix& b)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nc != b_nr)
    gripe_nonconformant ("operator *", a_nr, a_nc, b_nr, b_nc);

  FloatComplexDiagMatrix c (a_nr, b_nc);

  octave_idx_type len = c.length ();
  octave_idx_type lenm = len < a_nc ? len : a_nc;

  for (octave_idx_type i = 0; i < lenm; i++)
    c.dgxelem (i) = a.dgelem (i) * b.dgelem (i);
  for (octave_idx_type i = lenm; i < len; i++)
    c.dgxelem (i) = 0.0f;

  return c;
}

FloatComplexDiagMatrix
operator * (const FloatDiagMatrix& a, const FloatComplexDiagMatrix& b)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nc != b_nr)
    {
      gripe_nonconformant ("operator *", a_nr, a_nc, b_nr, b_nc);
      return FloatComplexDiagMatrix ();
    }

  if (a_nr == 0 || a_nc == 0 || b_nc == 0)
    return FloatComplexDiagMatrix (a_nr, a_nc, 0.0);

  FloatComplexDiagMatrix c (a_nr, b_nc);

  octave_idx_type len = a_nr < b_nc ? a_nr : b_nc;

  for (octave_idx_type i = 0; i < len; i++)
    {
      float a_element = a.elem (i, i);
      FloatComplex b_element = b.elem (i, i);

      c.elem (i, i) = a_element * b_element;
    }

  return c;
}

FloatComplexDiagMatrix
operator * (const FloatComplexDiagMatrix& a, const FloatComplexDiagMatrix& b)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nc != b_nr)
    {
      gripe_nonconformant ("operator *", a_nr, a_nc, b_nr, b_nc);
      return FloatComplexDiagMatrix ();
    }

  if (a_nr == 0 || a_nc == 0 || b_nc == 0)
    return FloatComplexDiagMatrix (a_nr, a_nc, 0.0);

  FloatComplexDiagMatrix c (a_nr, b_nc);

  octave_idx_type len = a_nr < b_nc ? a_nr : b_nc;

  for (octave_idx_type i = 0; i < len; i++)
    {
      FloatComplex a_element = a.elem (i, i);
      FloatComplex b_element = b.elem (i, i);

      c.elem (i, i) = a_element * b_element;
    }

  return c;
}

// other operations

FloatComplexDET
FloatComplexDiagMatrix::determinant (void) const
{
  FloatComplexDET det (1.0f);
  if (rows () != cols ())
    {
      (*current_liboctave_error_handler) ("determinant requires square matrix");
      det = FloatComplexDET (0.0);
    }
  else
    {
      octave_idx_type len = length ();
      for (octave_idx_type i = 0; i < len; i++)
        det *= elem (i, i);
    }

  return det;
}

float
FloatComplexDiagMatrix::rcond (void) const
{
  FloatColumnVector av = extract_diag (0).map<float> (std::abs);
  float amx = av.max ();
  float amn = av.min ();
  return amx == 0 ? 0.0f : amn / amx;
}

// i/o

std::ostream&
operator << (std::ostream& os, const FloatComplexDiagMatrix& a)
{
  FloatComplex ZERO (0.0);
//  int field_width = os.precision () + 7;
  for (octave_idx_type i = 0; i < a.rows (); i++)
    {
      for (octave_idx_type j = 0; j < a.cols (); j++)
        {
          if (i == j)
            os << " " /* setw (field_width) */ << a.elem (i, i);
          else
            os << " " /* setw (field_width) */ << ZERO;
        }
      os << "\n";
    }
  return os;
}
