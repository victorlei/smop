/*

Copyright (C) 1996-2015 John W. Eaton
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

#if !defined (octave_boolMatrix_h)
#define octave_boolMatrix_h 1

#include "Array.h"
#include "boolNDArray.h"

#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
boolMatrix : public boolNDArray
{
public:

  boolMatrix (void) : boolNDArray () { }

  boolMatrix (octave_idx_type r, octave_idx_type c)
    : boolNDArray (dim_vector (r, c)) { }

  boolMatrix (octave_idx_type r, octave_idx_type c, bool val)
    : boolNDArray (dim_vector (r, c), val) { }

  boolMatrix (const dim_vector& dv) : boolNDArray (dv.redim (2)) { }

  boolMatrix (const dim_vector& dv, bool val)
    : boolNDArray (dv.redim (2), val) { }

  boolMatrix (const Array<bool>& a) : boolNDArray (a.as_matrix ()) { }

  boolMatrix (const boolMatrix& a) : boolNDArray (a) { }

  bool operator == (const boolMatrix& a) const;
  bool operator != (const boolMatrix& a) const;

  boolMatrix transpose (void) const { return Array<bool>::transpose (); }

  // destructive insert/delete/reorder operations

  boolMatrix& insert (const boolMatrix& a,
                      octave_idx_type r, octave_idx_type c);

  // unary operations

  boolMatrix operator ! (void) const;

  // other operations

  boolMatrix diag (octave_idx_type k = 0) const;

#if 0
  // i/o

  friend std::ostream& operator << (std::ostream& os, const Matrix& a);
  friend std::istream& operator >> (std::istream& is, Matrix& a);
#endif

  void resize (octave_idx_type nr, octave_idx_type nc, bool rfv = false)
  {
    Array<bool>::resize (dim_vector (nr, nc), rfv);
  }
};

MM_BOOL_OP_DECLS (boolMatrix, boolMatrix, OCTAVE_API)
MS_BOOL_OP_DECLS (boolMatrix, bool, OCTAVE_API)
SM_BOOL_OP_DECLS (bool, boolMatrix, OCTAVE_API)
MM_CMP_OP_DECLS (boolMatrix, boolMatrix, OCTAVE_API)

#endif
