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

#if !defined (octave_dRowVector_h)
#define octave_dRowVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
RowVector : public MArray<double>
{
public:

  RowVector (void) : MArray<double> (dim_vector (1, 0)) { }

  explicit RowVector (octave_idx_type n)
    : MArray<double> (dim_vector (1, n)) { }

  explicit RowVector (const dim_vector& dv) : MArray<double> (dv.as_row ()) { }

  RowVector (octave_idx_type n, double val)
    : MArray<double> (dim_vector (1, n), val) { }

  RowVector (const RowVector& a) : MArray<double> (a) { }

  RowVector (const MArray<double>& a) : MArray<double> (a.as_row ()) { }

  RowVector (const Array<double>& a) : MArray<double> (a.as_row ()) { }

  RowVector& operator = (const RowVector& a)
  {
    MArray<double>::operator = (a);
    return *this;
  }

  bool operator == (const RowVector& a) const;
  bool operator != (const RowVector& a) const;

  // destructive insert/delete/reorder operations

  RowVector& insert (const RowVector& a, octave_idx_type c);

  RowVector& fill (double val);
  RowVector& fill (double val, octave_idx_type c1, octave_idx_type c2);

  RowVector append (const RowVector& a) const;

  ColumnVector transpose (void) const;

  friend OCTAVE_API RowVector real (const ComplexRowVector& a);
  friend OCTAVE_API RowVector imag (const ComplexRowVector& a);

  // resize is the destructive equivalent for this one

  RowVector extract (octave_idx_type c1, octave_idx_type c2) const;

  RowVector extract_n (octave_idx_type c1, octave_idx_type n) const;

  // row vector by matrix -> row vector

  friend OCTAVE_API RowVector operator * (const RowVector& a, const Matrix& b);

  // other operations

  double min (void) const;
  double max (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const RowVector& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, RowVector& a);

  void resize (octave_idx_type n, const double& rfv = 0)
  {
    Array<double>::resize (dim_vector (1, n), rfv);
  }

  void clear (octave_idx_type n)
  { Array<double>::clear (1, n); }

};

// row vector by column vector -> scalar

double OCTAVE_API operator * (const RowVector& a, const ColumnVector& b);

Complex OCTAVE_API operator * (const RowVector& a,
                               const ComplexColumnVector& b);

// other operations

OCTAVE_API RowVector linspace (double x1, double x2, octave_idx_type n);

MARRAY_FORWARD_DEFS (MArray, RowVector, double)

#endif
