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

#if !defined (octave_CColVector_h)
#define octave_CColVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
ComplexColumnVector : public MArray<Complex>
{
  friend class ComplexMatrix;
  friend class ComplexRowVector;

public:

  ComplexColumnVector (void) : MArray<Complex> (dim_vector (0, 1)) { }

  explicit ComplexColumnVector (octave_idx_type n)
    : MArray<Complex> (dim_vector (n, 1)) { }

  explicit ComplexColumnVector (const dim_vector& dv)
    : MArray<Complex> (dv.as_column ()) { }

  ComplexColumnVector (octave_idx_type n, const Complex& val)
    : MArray<Complex> (dim_vector (n, 1), val) { }

  ComplexColumnVector (const ComplexColumnVector& a) : MArray<Complex> (a) { }

  ComplexColumnVector (const MArray<Complex>& a)
    : MArray<Complex> (a.as_column ()) { }

  ComplexColumnVector (const Array<Complex>& a)
    : MArray<Complex> (a.as_column ()) { }

  explicit ComplexColumnVector (const ColumnVector& a);

  ComplexColumnVector& operator = (const ComplexColumnVector& a)
  {
    MArray<Complex>::operator = (a);
    return *this;
  }

  bool operator == (const ComplexColumnVector& a) const;
  bool operator != (const ComplexColumnVector& a) const;

  // destructive insert/delete/reorder operations

  ComplexColumnVector& insert (const ColumnVector& a, octave_idx_type r);
  ComplexColumnVector& insert (const ComplexColumnVector& a, octave_idx_type r);

  ComplexColumnVector& fill (double val);
  ComplexColumnVector& fill (const Complex& val);
  ComplexColumnVector& fill (double val,
                             octave_idx_type r1, octave_idx_type r2);
  ComplexColumnVector& fill (const Complex& val,
                             octave_idx_type r1, octave_idx_type r2);

  ComplexColumnVector stack (const ColumnVector& a) const;
  ComplexColumnVector stack (const ComplexColumnVector& a) const;

  ComplexRowVector hermitian (void) const;
  ComplexRowVector transpose (void) const;

  friend OCTAVE_API ComplexColumnVector conj (const ComplexColumnVector& a);

  // resize is the destructive equivalent for this one

  ComplexColumnVector extract (octave_idx_type r1, octave_idx_type r2) const;

  ComplexColumnVector extract_n (octave_idx_type r1, octave_idx_type n) const;

  // column vector by column vector -> column vector operations

  ComplexColumnVector& operator += (const ColumnVector& a);
  ComplexColumnVector& operator -= (const ColumnVector& a);

  // matrix by column vector -> column vector operations

  friend OCTAVE_API ComplexColumnVector operator * (const ComplexMatrix& a,
                                                    const ColumnVector& b);

  friend OCTAVE_API ComplexColumnVector operator * (const ComplexMatrix& a,
                                                    const ComplexColumnVector& b);

  // matrix by column vector -> column vector operations

  friend OCTAVE_API ComplexColumnVector operator * (const Matrix& a,
                                                    const ComplexColumnVector& b);

  // diagonal matrix by column vector -> column vector operations

  friend OCTAVE_API ComplexColumnVector operator * (const DiagMatrix& a,
                                                    const ComplexColumnVector& b);

  friend OCTAVE_API ComplexColumnVector operator * (const ComplexDiagMatrix& a,
                                                    const ColumnVector& b);

  friend OCTAVE_API ComplexColumnVector operator * (const ComplexDiagMatrix& a,
                                                    const ComplexColumnVector& b);

  // other operations

  Complex min (void) const;
  Complex max (void) const;

  ColumnVector abs (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const ComplexColumnVector& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               ComplexColumnVector& a);

  void resize (octave_idx_type n, const Complex& rfv = Complex (0))
  {
    Array<Complex>::resize (dim_vector (n, 1), rfv);
  }

  void clear (octave_idx_type n)
  { Array<Complex>::clear (n, 1); }

};

MARRAY_FORWARD_DEFS (MArray, ComplexColumnVector, Complex)

#endif
