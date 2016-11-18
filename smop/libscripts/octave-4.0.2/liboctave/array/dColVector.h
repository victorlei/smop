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

#if !defined (octave_dColVector_h)
#define octave_dColVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
ColumnVector : public MArray<double>
{
public:

  ColumnVector (void) : MArray<double> (dim_vector (0, 1)) { }

  explicit ColumnVector (octave_idx_type n)
    : MArray<double> (dim_vector (n, 1)) { }

  explicit ColumnVector (const dim_vector& dv)
    : MArray<double> (dv.as_column ()) { }

  ColumnVector (octave_idx_type n, double val)
    : MArray<double> (dim_vector (n, 1), val) { }

  ColumnVector (const ColumnVector& a) : MArray<double> (a) { }

  ColumnVector (const MArray<double>& a) : MArray<double> (a.as_column ()) { }
  ColumnVector (const Array<double>& a) : MArray<double> (a.as_column ()) { }

  ColumnVector& operator = (const ColumnVector& a)
  {
    MArray<double>::operator = (a);
    return *this;
  }

  bool operator == (const ColumnVector& a) const;
  bool operator != (const ColumnVector& a) const;

  // destructive insert/delete/reorder operations

  ColumnVector& insert (const ColumnVector& a, octave_idx_type r);

  ColumnVector& fill (double val);
  ColumnVector& fill (double val, octave_idx_type r1, octave_idx_type r2);

  ColumnVector stack (const ColumnVector& a) const;

  RowVector transpose (void) const;

  friend OCTAVE_API ColumnVector real (const ComplexColumnVector& a);
  friend OCTAVE_API ColumnVector imag (const ComplexColumnVector& a);

  // resize is the destructive equivalent for this one

  ColumnVector extract (octave_idx_type r1, octave_idx_type r2) const;

  ColumnVector extract_n (octave_idx_type r1, octave_idx_type n) const;

  // matrix by column vector -> column vector operations

  friend OCTAVE_API ColumnVector operator * (const Matrix& a,
                                             const ColumnVector& b);

  // diagonal matrix by column vector -> column vector operations

  friend OCTAVE_API ColumnVector operator * (const DiagMatrix& a,
                                             const ColumnVector& b);

  // other operations

  double min (void) const;
  double max (void) const;

  ColumnVector abs (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const ColumnVector& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               ColumnVector& a);

  void resize (octave_idx_type n, const double& rfv = 0)
  {
    Array<double>::resize (dim_vector (n, 1), rfv);
  }

  void clear (octave_idx_type n)
  { Array<double>::clear (n, 1); }

};

// Publish externally used friend functions.

extern OCTAVE_API ColumnVector real (const ComplexColumnVector& a);
extern OCTAVE_API ColumnVector imag (const ComplexColumnVector& a);

MARRAY_FORWARD_DEFS (MArray, ColumnVector, double)

#endif
