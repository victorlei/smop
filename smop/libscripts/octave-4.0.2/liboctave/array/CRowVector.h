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

#if !defined (octave_CRowVector_h)
#define octave_CRowVector_h 1

#include "MArray.h"
#include "dRowVector.h"

#include "mx-defs.h"

class
OCTAVE_API
ComplexRowVector : public MArray<Complex>
{
  friend class ComplexColumnVector;

public:

  ComplexRowVector (void) : MArray<Complex> (dim_vector (1, 0)) { }

  explicit ComplexRowVector (octave_idx_type n)
    : MArray<Complex> (dim_vector (1, n)) { }

  explicit ComplexRowVector (const dim_vector& dv) : MArray<Complex> (dv) { }

  ComplexRowVector (octave_idx_type n, const Complex& val)
    : MArray<Complex> (dim_vector (1, n), val) { }

  ComplexRowVector (const ComplexRowVector& a) : MArray<Complex> (a) { }

  ComplexRowVector (const MArray<Complex>& a)
    : MArray<Complex> (a.as_row ()) { }

  ComplexRowVector (const Array<Complex>& a)
    : MArray<Complex> (a.as_row ()) { }

  explicit ComplexRowVector (const RowVector& a) : MArray<Complex> (a) { }

  ComplexRowVector& operator = (const ComplexRowVector& a)
  {
    MArray<Complex>::operator = (a);
    return *this;
  }

  bool operator == (const ComplexRowVector& a) const;
  bool operator != (const ComplexRowVector& a) const;

  // destructive insert/delete/reorder operations

  ComplexRowVector& insert (const RowVector& a, octave_idx_type c);
  ComplexRowVector& insert (const ComplexRowVector& a, octave_idx_type c);

  ComplexRowVector& fill (double val);
  ComplexRowVector& fill (const Complex& val);
  ComplexRowVector& fill (double val, octave_idx_type c1, octave_idx_type c2);
  ComplexRowVector& fill (const Complex& val,
                          octave_idx_type c1, octave_idx_type c2);

  ComplexRowVector append (const RowVector& a) const;
  ComplexRowVector append (const ComplexRowVector& a) const;

  ComplexColumnVector hermitian (void) const;
  ComplexColumnVector transpose (void) const;

  friend ComplexRowVector conj (const ComplexRowVector& a);

  // resize is the destructive equivalent for this one

  ComplexRowVector extract (octave_idx_type c1, octave_idx_type c2) const;

  ComplexRowVector extract_n (octave_idx_type c1, octave_idx_type n) const;

  // row vector by row vector -> row vector operations

  ComplexRowVector& operator += (const RowVector& a);
  ComplexRowVector& operator -= (const RowVector& a);

  // row vector by matrix -> row vector

  friend ComplexRowVector operator * (const ComplexRowVector& a,
                                      const ComplexMatrix& b);

  friend ComplexRowVector operator * (const RowVector& a,
                                      const ComplexMatrix& b);

  // other operations

  Complex min (void) const;
  Complex max (void) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os,
                                    const ComplexRowVector& a);
  friend std::istream& operator >> (std::istream& is, ComplexRowVector& a);

  void resize (octave_idx_type n, const Complex& rfv = Complex (0))
  {
    Array<Complex>::resize (dim_vector (1, n), rfv);
  }

  void clear (octave_idx_type n)
  { Array<Complex>::clear (1, n); }

};

// row vector by column vector -> scalar

Complex OCTAVE_API operator * (const ComplexRowVector& a,
                               const ColumnVector& b);

Complex OCTAVE_API operator * (const ComplexRowVector& a,
                               const ComplexColumnVector& b);

// other operations

OCTAVE_API ComplexRowVector linspace (const Complex& x1, const Complex& x2,
                                      octave_idx_type n);

MARRAY_FORWARD_DEFS (MArray, ComplexRowVector, Complex)

#endif
