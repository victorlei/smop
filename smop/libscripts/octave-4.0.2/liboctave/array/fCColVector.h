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

#if !defined (octave_fCColVector_h)
#define octave_fCColVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
FloatComplexColumnVector : public MArray<FloatComplex>
{
  friend class FloatComplexMatrix;
  friend class FloatComplexRowVector;

public:

  FloatComplexColumnVector (void)
    : MArray<FloatComplex> (dim_vector (0, 1)) { }

  explicit FloatComplexColumnVector (octave_idx_type n)
    : MArray<FloatComplex> (dim_vector (n, 1)) { }

  explicit FloatComplexColumnVector (const dim_vector& dv)
    : MArray<FloatComplex> (dv.as_column ()) { }

  FloatComplexColumnVector (octave_idx_type n, const FloatComplex& val)
    : MArray<FloatComplex> (dim_vector (n, 1), val) { }

  FloatComplexColumnVector (const FloatComplexColumnVector& a)
    : MArray<FloatComplex> (a) { }

  FloatComplexColumnVector (const MArray<FloatComplex>& a)
    : MArray<FloatComplex> (a.as_column ()) { }

  FloatComplexColumnVector (const Array<FloatComplex>& a)
    : MArray<FloatComplex> (a.as_column ()) { }

  explicit FloatComplexColumnVector (const FloatColumnVector& a);

  FloatComplexColumnVector& operator = (const FloatComplexColumnVector& a)
  {
    MArray<FloatComplex>::operator = (a);
    return *this;
  }

  bool operator == (const FloatComplexColumnVector& a) const;
  bool operator != (const FloatComplexColumnVector& a) const;

  // destructive insert/delete/reorder operations

  FloatComplexColumnVector& insert (const FloatColumnVector& a,
                                    octave_idx_type r);
  FloatComplexColumnVector& insert (const FloatComplexColumnVector& a,
                                    octave_idx_type r);

  FloatComplexColumnVector& fill (float val);
  FloatComplexColumnVector& fill (const FloatComplex& val);
  FloatComplexColumnVector& fill (float val,
                                  octave_idx_type r1, octave_idx_type r2);
  FloatComplexColumnVector& fill (const FloatComplex& val,
                                  octave_idx_type r1, octave_idx_type r2);

  FloatComplexColumnVector stack (const FloatColumnVector& a) const;
  FloatComplexColumnVector stack (const FloatComplexColumnVector& a) const;

  FloatComplexRowVector hermitian (void) const;
  FloatComplexRowVector transpose (void) const;

  friend OCTAVE_API FloatComplexColumnVector
  conj (const FloatComplexColumnVector& a);

  // resize is the destructive equivalent for this one

  FloatComplexColumnVector extract (octave_idx_type r1,
                                    octave_idx_type r2) const;

  FloatComplexColumnVector extract_n (octave_idx_type r1,
                                      octave_idx_type n) const;

  // column vector by column vector -> column vector operations

  FloatComplexColumnVector& operator += (const FloatColumnVector& a);
  FloatComplexColumnVector& operator -= (const FloatColumnVector& a);

  // matrix by column vector -> column vector operations

  friend OCTAVE_API FloatComplexColumnVector
  operator * (const FloatComplexMatrix& a, const FloatColumnVector& b);

  friend OCTAVE_API FloatComplexColumnVector
  operator * (const FloatComplexMatrix& a, const FloatComplexColumnVector& b);

  // matrix by column vector -> column vector operations

  friend OCTAVE_API FloatComplexColumnVector
  operator * (const FloatMatrix& a, const FloatComplexColumnVector& b);

  // diagonal matrix by column vector -> column vector operations

  friend OCTAVE_API FloatComplexColumnVector
  operator * (const FloatDiagMatrix& a, const FloatComplexColumnVector& b);

  friend OCTAVE_API FloatComplexColumnVector
  operator * (const FloatComplexDiagMatrix& a, const ColumnVector& b);

  friend OCTAVE_API FloatComplexColumnVector
  operator * (const FloatComplexDiagMatrix& a, const FloatComplexColumnVector& b);

  // other operations

  FloatComplex min (void) const;
  FloatComplex max (void) const;

  FloatColumnVector abs (void) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const FloatComplexColumnVector& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               FloatComplexColumnVector& a);

  void resize (octave_idx_type n, const FloatComplex& rfv = FloatComplex (0))
  {
    Array<FloatComplex>::resize (dim_vector (n, 1), rfv);
  }

  void clear (octave_idx_type n)
  { Array<FloatComplex>::clear (n, 1); }

};

MARRAY_FORWARD_DEFS (MArray, FloatComplexColumnVector, FloatComplex)

#endif
