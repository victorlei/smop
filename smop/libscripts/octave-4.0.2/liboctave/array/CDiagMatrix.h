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

#if !defined (octave_CDiagMatrix_h)
#define octave_CDiagMatrix_h 1

#include "MDiagArray2.h"

#include "dRowVector.h"
#include "CRowVector.h"
#include "dColVector.h"
#include "CColVector.h"
#include "DET.h"

#include "mx-defs.h"

class
OCTAVE_API
ComplexDiagMatrix : public MDiagArray2<Complex>
{
public:

  ComplexDiagMatrix (void) : MDiagArray2<Complex> () { }

  ComplexDiagMatrix (octave_idx_type r, octave_idx_type c)
    : MDiagArray2<Complex> (r, c) { }

  ComplexDiagMatrix (octave_idx_type r, octave_idx_type c, const Complex& val)
    : MDiagArray2<Complex> (r, c, val) { }

  explicit ComplexDiagMatrix (const Array<Complex>& a)
    : MDiagArray2<Complex> (a) { }

  explicit ComplexDiagMatrix (const Array<double>& a)
    : MDiagArray2<Complex> (Array<Complex> (a)) { }

  ComplexDiagMatrix (const Array<Complex>& a, octave_idx_type r,
                     octave_idx_type c)
    : MDiagArray2<Complex> (a, r, c) { }

  explicit ComplexDiagMatrix (const DiagMatrix& a);

  ComplexDiagMatrix (const MDiagArray2<Complex>& a)
    : MDiagArray2<Complex> (a) { }

  ComplexDiagMatrix (const ComplexDiagMatrix& a)
    : MDiagArray2<Complex> (a) { }

  template <class U>
  ComplexDiagMatrix (const DiagArray2<U>& a)
    : MDiagArray2<Complex> (a) { }

  ComplexDiagMatrix& operator = (const ComplexDiagMatrix& a)
  {
    MDiagArray2<Complex>::operator = (a);
    return *this;
  }

  bool operator == (const ComplexDiagMatrix& a) const;
  bool operator != (const ComplexDiagMatrix& a) const;

  ComplexDiagMatrix& fill (double val);
  ComplexDiagMatrix& fill (const Complex& val);
  ComplexDiagMatrix& fill (double val,
                           octave_idx_type beg, octave_idx_type end);
  ComplexDiagMatrix& fill (const Complex& val,
                           octave_idx_type beg, octave_idx_type end);
  ComplexDiagMatrix& fill (const ColumnVector& a);
  ComplexDiagMatrix& fill (const ComplexColumnVector& a);
  ComplexDiagMatrix& fill (const RowVector& a);
  ComplexDiagMatrix& fill (const ComplexRowVector& a);
  ComplexDiagMatrix& fill (const ColumnVector& a, octave_idx_type beg);
  ComplexDiagMatrix& fill (const ComplexColumnVector& a, octave_idx_type beg);
  ComplexDiagMatrix& fill (const RowVector& a, octave_idx_type beg);
  ComplexDiagMatrix& fill (const ComplexRowVector& a, octave_idx_type beg);

  ComplexDiagMatrix hermitian (void) const
  { return MDiagArray2<Complex>::hermitian (std::conj); }
  ComplexDiagMatrix transpose (void) const
  { return MDiagArray2<Complex>::transpose (); }
  DiagMatrix abs (void) const;

  friend OCTAVE_API ComplexDiagMatrix conj (const ComplexDiagMatrix& a);

  // resize is the destructive analog for this one

  ComplexMatrix extract (octave_idx_type r1, octave_idx_type c1,
                         octave_idx_type r2, octave_idx_type c2) const;

  // extract row or column i

  ComplexRowVector row (octave_idx_type i) const;
  ComplexRowVector row (char *s) const;

  ComplexColumnVector column (octave_idx_type i) const;
  ComplexColumnVector column (char *s) const;

  ComplexDiagMatrix inverse (octave_idx_type& info) const;
  ComplexDiagMatrix inverse (void) const;
  ComplexDiagMatrix pseudo_inverse (double tol = 0.0) const;

  bool all_elements_are_real (void) const;

  // diagonal matrix by diagonal matrix -> diagonal matrix operations

  ComplexDiagMatrix& operator += (const DiagMatrix& a);
  ComplexDiagMatrix& operator -= (const DiagMatrix& a);

  // other operations

  ComplexColumnVector extract_diag (octave_idx_type k = 0) const
  { return MDiagArray2<Complex>::extract_diag (k); }

  ComplexDET determinant (void) const;
  double rcond (void) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os,
                                    const ComplexDiagMatrix& a);

};

OCTAVE_API ComplexDiagMatrix conj (const ComplexDiagMatrix& a);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

OCTAVE_API ComplexDiagMatrix
operator * (const ComplexDiagMatrix& a, const ComplexDiagMatrix& b);

OCTAVE_API ComplexDiagMatrix
operator * (const ComplexDiagMatrix& a, const DiagMatrix& b);

OCTAVE_API ComplexDiagMatrix
operator * (const DiagMatrix& a, const ComplexDiagMatrix& b);

MDIAGARRAY2_FORWARD_DEFS (MDiagArray2, ComplexDiagMatrix, Complex)

#endif
