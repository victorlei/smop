/*

Copyright (C) 1995-2015 John W. Eaton
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

#if !defined (octave_chMatrix_h)
#define octave_chMatrix_h 1

#include <string>

#include "Array.h"
#include "chNDArray.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "str-vec.h"

class
OCTAVE_API
charMatrix : public charNDArray
{
  friend class ComplexMatrix;

public:

  charMatrix (void) : charNDArray () { }

  charMatrix (octave_idx_type r, octave_idx_type c)
    : charNDArray (dim_vector (r, c)) { }

  charMatrix (octave_idx_type r, octave_idx_type c, char val)
    : charNDArray (dim_vector (r, c), val) { }

  charMatrix (const dim_vector& dv) : charNDArray (dv.redim (2)) { }

  charMatrix (const dim_vector& dv, char val)
    : charNDArray (dv.redim (2), val) { }

  charMatrix (const Array<char>& a) : charNDArray (a.as_matrix ()) { }

  charMatrix (const charMatrix& a) : charNDArray (a) { }

  charMatrix (char c) : charNDArray (c) { }

  charMatrix (const char *s) : charNDArray (s) { }

  charMatrix (const std::string& s) : charNDArray (s) { }

  charMatrix (const string_vector& s, char fill_value = '\0')
    : charNDArray (s, fill_value) { }

  bool operator == (const charMatrix& a) const;
  bool operator != (const charMatrix& a) const;

  charMatrix transpose (void) const { return Array<char>::transpose (); }

  // destructive insert/delete/reorder operations

  charMatrix& insert (const char *s, octave_idx_type r, octave_idx_type c);
  charMatrix& insert (const charMatrix& a,
                      octave_idx_type r, octave_idx_type c);

  std::string row_as_string (octave_idx_type, bool strip_ws = false) const;

  // resize is the destructive equivalent for this one

  charMatrix extract (octave_idx_type r1, octave_idx_type c1,
                      octave_idx_type r2, octave_idx_type c2) const;

  void resize (octave_idx_type nr, octave_idx_type nc, char rfv = 0)
  {
    Array<char>::resize (dim_vector (nr, nc), rfv);
  }

#if 0
  // i/o

  friend std::ostream& operator << (std::ostream& os, const Matrix& a);
  friend std::istream& operator >> (std::istream& is, Matrix& a);
#endif
};

MS_CMP_OP_DECLS (charMatrix, char, OCTAVE_API)
MS_BOOL_OP_DECLS (charMatrix, char, OCTAVE_API)

SM_CMP_OP_DECLS (char, charMatrix, OCTAVE_API)
SM_BOOL_OP_DECLS (char, charMatrix, OCTAVE_API)

MM_CMP_OP_DECLS (charMatrix, charMatrix, OCTAVE_API)
MM_BOOL_OP_DECLS (charMatrix, charMatrix, OCTAVE_API)

#endif
