// ColumnVector manipulations.
/*

Copyright (C) 1994-2015 John W. Eaton
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
  F77_FUNC (dgemv, DGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const double&, const double*,
                           const octave_idx_type&, const double*,
                           const octave_idx_type&, const double&, double*,
                           const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL);
}

// Column Vector class.

bool
ColumnVector::operator == (const ColumnVector& a) const
{
  octave_idx_type len = length ();
  if (len != a.length ())
    return 0;
  return mx_inline_equal (len, data (), a.data ());
}

bool
ColumnVector::operator != (const ColumnVector& a) const
{
  return !(*this == a);
}

ColumnVector&
ColumnVector::insert (const ColumnVector& a, octave_idx_type r)
{
  octave_idx_type a_len = a.length ();

  if (r < 0 || r + a_len > length ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r+i) = a.elem (i);
    }

  return *this;
}

ColumnVector&
ColumnVector::fill (double val)
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

ColumnVector&
ColumnVector::fill (double val, octave_idx_type r1, octave_idx_type r2)
{
  octave_idx_type len = length ();

  if (r1 < 0 || r2 < 0 || r1 >= len || r2 >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { std::swap (r1, r2); }

  if (r2 >= r1)
    {
      make_unique ();

      for (octave_idx_type i = r1; i <= r2; i++)
        xelem (i) = val;
    }

  return *this;
}

ColumnVector
ColumnVector::stack (const ColumnVector& a) const
{
  octave_idx_type len = length ();
  octave_idx_type nr_insert = len;
  ColumnVector retval (len + a.length ());
  retval.insert (*this, 0);
  retval.insert (a, nr_insert);
  return retval;
}

RowVector
ColumnVector::transpose (void) const
{
  return MArray<double>::transpose ();
}

ColumnVector
ColumnVector::abs (void) const
{
  return do_mx_unary_map<double, double, std::abs> (*this);
}

ColumnVector
real (const ComplexColumnVector& a)
{
  return do_mx_unary_op<double, Complex> (a, mx_inline_real);
}

ColumnVector
imag (const ComplexColumnVector& a)
{
  return do_mx_unary_op<double, Complex> (a, mx_inline_imag);
}

// resize is the destructive equivalent for this one

ColumnVector
ColumnVector::extract (octave_idx_type r1, octave_idx_type r2) const
{
  if (r1 > r2) { std::swap (r1, r2); }

  octave_idx_type new_r = r2 - r1 + 1;

  ColumnVector result (new_r);

  for (octave_idx_type i = 0; i < new_r; i++)
    result.xelem (i) = elem (r1+i);

  return result;
}

ColumnVector
ColumnVector::extract_n (octave_idx_type r1, octave_idx_type n) const
{
  ColumnVector result (n);

  for (octave_idx_type i = 0; i < n; i++)
    result.xelem (i) = elem (r1+i);

  return result;
}

// matrix by column vector -> column vector operations

ColumnVector
operator * (const Matrix& m, const ColumnVector& a)
{
  ColumnVector retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.cols ();

  octave_idx_type a_len = a.length ();

  if (nc != a_len)
    gripe_nonconformant ("operator *", nr, nc, a_len, 1);
  else
    {
      retval.clear (nr);

      if (nr != 0)
        {
          if (nc == 0)
            retval.fill (0.0);
          else
            {
              double *y = retval.fortran_vec ();

              F77_XFCN (dgemv, DGEMV, (F77_CONST_CHAR_ARG2 ("N", 1),
                                       nr, nc, 1.0, m.data (), nr,
                                       a.data (), 1, 0.0, y, 1
                                       F77_CHAR_ARG_LEN (1)));
            }
        }
    }

  return retval;
}

// diagonal matrix by column vector -> column vector operations

ColumnVector
operator * (const DiagMatrix& m, const ColumnVector& a)
{
  ColumnVector retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.cols ();

  octave_idx_type a_len = a.length ();

  if (nc != a_len)
    gripe_nonconformant ("operator *", nr, nc, a_len, 1);
  else
    {
      if (nr == 0 || nc == 0)
        retval.resize (nr, 0.0);
      else
        {
          retval.resize (nr);

          for (octave_idx_type i = 0; i < a_len; i++)
            retval.elem (i) = a.elem (i) * m.elem (i, i);

          for (octave_idx_type i = a_len; i < nr; i++)
            retval.elem (i) = 0.0;
        }
    }

  return retval;
}

// other operations

double
ColumnVector::min (void) const
{
  octave_idx_type len = length ();
  if (len == 0)
    return 0.0;

  double res = elem (0);

  for (octave_idx_type i = 1; i < len; i++)
    if (elem (i) < res)
      res = elem (i);

  return res;
}

double
ColumnVector::max (void) const
{
  octave_idx_type len = length ();
  if (len == 0)
    return 0.0;

  double res = elem (0);

  for (octave_idx_type i = 1; i < len; i++)
    if (elem (i) > res)
      res = elem (i);

  return res;
}

std::ostream&
operator << (std::ostream& os, const ColumnVector& a)
{
//  int field_width = os.precision () + 7;
  for (octave_idx_type i = 0; i < a.length (); i++)
    os << /* setw (field_width) << */ a.elem (i) << "\n";
  return os;
}

std::istream&
operator >> (std::istream& is, ColumnVector& a)
{
  octave_idx_type len = a.length ();

  if (len > 0)
    {
      double tmp;
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
