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

#if !defined (octave_dDiagMatrix_h)
#define octave_dDiagMatrix_h 1

#include "MDiagArray2.h"

#include "dRowVector.h"
#include "dColVector.h"
#include "DET.h"

#include "mx-defs.h"

class
OCTAVE_API
DiagMatrix : public MDiagArray2<double>
{
  friend class SVD;
  friend class ComplexSVD;

public:

  DiagMatrix (void) : MDiagArray2<double> () { }

  DiagMatrix (octave_idx_type r, octave_idx_type c)
    : MDiagArray2<double> (r, c) { }

  DiagMatrix (octave_idx_type r, octave_idx_type c, double val)
    : MDiagArray2<double> (r, c, val) { }

  DiagMatrix (const DiagMatrix& a) : MDiagArray2<double> (a) { }

  DiagMatrix (const MDiagArray2<double>& a) : MDiagArray2<double> (a) { }

  template <class U>
  DiagMatrix (const DiagArray2<U>& a) : MDiagArray2<double> (a) { }

  explicit DiagMatrix (const Array<double>& a) : MDiagArray2<double> (a) { }

  DiagMatrix (const Array<double>& a, octave_idx_type r, octave_idx_type c)
    : MDiagArray2<double> (a, r, c) { }

  DiagMatrix& operator = (const DiagMatrix& a)
  {
    MDiagArray2<double>::operator = (a);
    return *this;
  }

  bool operator == (const DiagMatrix& a) const;
  bool operator != (const DiagMatrix& a) const;

  DiagMatrix& fill (double val);
  DiagMatrix& fill (double val, octave_idx_type beg, octave_idx_type end);
  DiagMatrix& fill (const ColumnVector& a);
  DiagMatrix& fill (const RowVector& a);
  DiagMatrix& fill (const ColumnVector& a, octave_idx_type beg);
  DiagMatrix& fill (const RowVector& a, octave_idx_type beg);

  DiagMatrix transpose (void) const
  { return MDiagArray2<double>::transpose (); }
  DiagMatrix abs (void) const;

  friend OCTAVE_API DiagMatrix real (const ComplexDiagMatrix& a);
  friend OCTAVE_API DiagMatrix imag (const ComplexDiagMatrix& a);

  // resize is the destructive analog for this one

  Matrix extract (octave_idx_type r1, octave_idx_type c1,
                  octave_idx_type r2, octave_idx_type c2) const;

  // extract row or column i.

  RowVector row (octave_idx_type i) const;
  RowVector row (char *s) const;

  ColumnVector column (octave_idx_type i) const;
  ColumnVector column (char *s) const;

  DiagMatrix inverse (void) const;
  DiagMatrix inverse (octave_idx_type& info) const;
  DiagMatrix pseudo_inverse (double tol = 0.0) const;

  // other operations

  ColumnVector extract_diag (octave_idx_type k = 0) const
  { return MDiagArray2<double>::extract_diag (k); }

  DET determinant (void) const;
  double rcond (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const DiagMatrix& a);

};

OCTAVE_API DiagMatrix real (const ComplexDiagMatrix& a);
OCTAVE_API DiagMatrix imag (const ComplexDiagMatrix& a);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

OCTAVE_API DiagMatrix
operator * (const DiagMatrix& a, const DiagMatrix& b);

MDIAGARRAY2_FORWARD_DEFS (MDiagArray2, DiagMatrix, double)

#endif
