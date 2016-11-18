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

#if !defined (octave_fCDiagMatrix_h)
#define octave_fCDiagMatrix_h 1

#include "MDiagArray2.h"

#include "fRowVector.h"
#include "fCRowVector.h"
#include "fColVector.h"
#include "fCColVector.h"
#include "DET.h"

#include "mx-defs.h"

class
OCTAVE_API
FloatComplexDiagMatrix : public MDiagArray2<FloatComplex>
{
public:

  FloatComplexDiagMatrix (void) : MDiagArray2<FloatComplex> () { }

  FloatComplexDiagMatrix (octave_idx_type r,
                          octave_idx_type c)
    : MDiagArray2<FloatComplex> (r, c) { }

  FloatComplexDiagMatrix (octave_idx_type r, octave_idx_type c,
                          const FloatComplex& val)
    : MDiagArray2<FloatComplex> (r, c, val) { }

  explicit FloatComplexDiagMatrix (const Array<FloatComplex>& a)
    : MDiagArray2<FloatComplex> (a) { }

  FloatComplexDiagMatrix (const Array<FloatComplex>& a, octave_idx_type r,
                          octave_idx_type c)
    : MDiagArray2<FloatComplex> (a, r, c) { }

  explicit FloatComplexDiagMatrix (const Array<float>& a)
    : MDiagArray2<FloatComplex> (Array<FloatComplex> (a)) { }

  explicit FloatComplexDiagMatrix (const FloatDiagMatrix& a);

  FloatComplexDiagMatrix (const MDiagArray2<FloatComplex>& a)
    : MDiagArray2<FloatComplex> (a) { }

  FloatComplexDiagMatrix (const FloatComplexDiagMatrix& a)
    : MDiagArray2<FloatComplex> (a) { }

  template <class U>
  FloatComplexDiagMatrix (const DiagArray2<U>& a)
    : MDiagArray2<FloatComplex> (a) { }

  FloatComplexDiagMatrix& operator = (const FloatComplexDiagMatrix& a)
  {
    MDiagArray2<FloatComplex>::operator = (a);
    return *this;
  }

  bool operator == (const FloatComplexDiagMatrix& a) const;
  bool operator != (const FloatComplexDiagMatrix& a) const;

  FloatComplexDiagMatrix& fill (float val);
  FloatComplexDiagMatrix& fill (const FloatComplex& val);
  FloatComplexDiagMatrix& fill (float val,
                                octave_idx_type beg, octave_idx_type end);
  FloatComplexDiagMatrix& fill (const FloatComplex& val,
                                octave_idx_type beg, octave_idx_type end);
  FloatComplexDiagMatrix& fill (const FloatColumnVector& a);
  FloatComplexDiagMatrix& fill (const FloatComplexColumnVector& a);
  FloatComplexDiagMatrix& fill (const FloatRowVector& a);
  FloatComplexDiagMatrix& fill (const FloatComplexRowVector& a);
  FloatComplexDiagMatrix& fill (const FloatColumnVector& a,
                                octave_idx_type beg);
  FloatComplexDiagMatrix& fill (const FloatComplexColumnVector& a,
                                octave_idx_type beg);
  FloatComplexDiagMatrix& fill (const FloatRowVector& a, octave_idx_type beg);
  FloatComplexDiagMatrix& fill (const FloatComplexRowVector& a,
                                octave_idx_type beg);

  FloatComplexDiagMatrix hermitian (void) const
  { return MDiagArray2<FloatComplex>::hermitian (std::conj); }
  FloatComplexDiagMatrix transpose (void) const
  { return MDiagArray2<FloatComplex>::transpose (); }
  FloatDiagMatrix abs (void) const;

  friend OCTAVE_API FloatComplexDiagMatrix
  conj (const FloatComplexDiagMatrix& a);

  // resize is the destructive analog for this one

  FloatComplexMatrix extract (octave_idx_type r1, octave_idx_type c1,
                              octave_idx_type r2, octave_idx_type c2) const;

  // extract row or column i

  FloatComplexRowVector row (octave_idx_type i) const;
  FloatComplexRowVector row (char *s) const;

  FloatComplexColumnVector column (octave_idx_type i) const;
  FloatComplexColumnVector column (char *s) const;

  FloatComplexDiagMatrix inverse (octave_idx_type& info) const;
  FloatComplexDiagMatrix inverse (void) const;
  FloatComplexDiagMatrix pseudo_inverse (float tol = 0.0f) const;

  bool all_elements_are_real (void) const;

  // diagonal matrix by diagonal matrix -> diagonal matrix operations

  FloatComplexDiagMatrix& operator += (const FloatDiagMatrix& a);
  FloatComplexDiagMatrix& operator -= (const FloatDiagMatrix& a);

  // other operations

  FloatComplexColumnVector extract_diag (octave_idx_type k = 0) const
  { return MDiagArray2<FloatComplex>::extract_diag (k); }

  FloatComplexDET determinant (void) const;
  float rcond (void) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os,
                                    const FloatComplexDiagMatrix& a);

};

OCTAVE_API FloatComplexDiagMatrix conj (const FloatComplexDiagMatrix& a);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

OCTAVE_API FloatComplexDiagMatrix
operator * (const FloatComplexDiagMatrix& a, const FloatComplexDiagMatrix& b);

OCTAVE_API FloatComplexDiagMatrix
operator * (const FloatComplexDiagMatrix& a, const FloatDiagMatrix& b);

OCTAVE_API FloatComplexDiagMatrix
operator * (const FloatDiagMatrix& a, const FloatComplexDiagMatrix& b);

MDIAGARRAY2_FORWARD_DEFS (MDiagArray2, FloatComplexDiagMatrix, FloatComplex)

#endif
