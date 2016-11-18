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

#if !defined (octave_fDiagMatrix_h)
#define octave_fDiagMatrix_h 1

#include "MDiagArray2.h"

#include "fRowVector.h"
#include "fColVector.h"
#include "DET.h"

#include "mx-defs.h"

class
OCTAVE_API
FloatDiagMatrix : public MDiagArray2<float>
{
  friend class FloatSVD;
  friend class FloatComplexSVD;

public:

  FloatDiagMatrix (void) : MDiagArray2<float> () { }

  FloatDiagMatrix (octave_idx_type r, octave_idx_type c)
    : MDiagArray2<float> (r, c) { }

  FloatDiagMatrix (octave_idx_type r, octave_idx_type c, float val)
    : MDiagArray2<float> (r, c, val) { }

  FloatDiagMatrix (const FloatDiagMatrix& a) : MDiagArray2<float> (a) { }

  FloatDiagMatrix (const MDiagArray2<float>& a) : MDiagArray2<float> (a) { }

  template <class U>
  FloatDiagMatrix (const DiagArray2<U>& a) : MDiagArray2<float> (a) { }

  explicit FloatDiagMatrix (const Array<double>& a) : MDiagArray2<float> (a) { }

  FloatDiagMatrix (const Array<float>& a, octave_idx_type r, octave_idx_type c)
    : MDiagArray2<float> (a, r, c) { }

  FloatDiagMatrix& operator = (const FloatDiagMatrix& a)
  {
    MDiagArray2<float>::operator = (a);
    return *this;
  }

  bool operator == (const FloatDiagMatrix& a) const;
  bool operator != (const FloatDiagMatrix& a) const;

  FloatDiagMatrix& fill (float val);
  FloatDiagMatrix& fill (float val, octave_idx_type beg, octave_idx_type end);
  FloatDiagMatrix& fill (const FloatColumnVector& a);
  FloatDiagMatrix& fill (const FloatRowVector& a);
  FloatDiagMatrix& fill (const FloatColumnVector& a, octave_idx_type beg);
  FloatDiagMatrix& fill (const FloatRowVector& a, octave_idx_type beg);

  FloatDiagMatrix transpose (void) const
  { return MDiagArray2<float>::transpose (); }

  FloatDiagMatrix abs (void) const;

  friend OCTAVE_API FloatDiagMatrix real (const FloatComplexDiagMatrix& a);
  friend OCTAVE_API FloatDiagMatrix imag (const FloatComplexDiagMatrix& a);

  // resize is the destructive analog for this one

  FloatMatrix extract (octave_idx_type r1, octave_idx_type c1,
                       octave_idx_type r2, octave_idx_type c2) const;

  // extract row or column i.

  FloatRowVector row (octave_idx_type i) const;
  FloatRowVector row (char *s) const;

  FloatColumnVector column (octave_idx_type i) const;
  FloatColumnVector column (char *s) const;

  FloatDiagMatrix inverse (void) const;
  FloatDiagMatrix inverse (octave_idx_type& info) const;
  FloatDiagMatrix pseudo_inverse (float tol = 0.0f) const;

  // other operations

  FloatColumnVector extract_diag (octave_idx_type k = 0) const
  { return MDiagArray2<float>::extract_diag (k); }

  FloatDET determinant (void) const;
  float rcond (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const FloatDiagMatrix& a);

};

OCTAVE_API FloatDiagMatrix real (const FloatComplexDiagMatrix& a);
OCTAVE_API FloatDiagMatrix imag (const FloatComplexDiagMatrix& a);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

OCTAVE_API FloatDiagMatrix operator * (const FloatDiagMatrix& a,
                                       const FloatDiagMatrix& b);

MDIAGARRAY2_FORWARD_DEFS (MDiagArray2, FloatDiagMatrix, float)

#endif
