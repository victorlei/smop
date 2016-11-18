/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#if !defined (octave_boolSparse_h)
#define octave_boolSparse_h 1

#include "Sparse.h"
#include "MSparse-defs.h"

#include "boolMatrix.h"
#include "boolNDArray.h"
#include "PermMatrix.h"

#include "Sparse-op-decls.h"

class SparseMatrix;

class
OCTAVE_API
SparseBoolMatrix : public Sparse<bool>
{
public:

  SparseBoolMatrix (void) : Sparse<bool> () { }

  SparseBoolMatrix (octave_idx_type r, octave_idx_type c)
    : Sparse<bool> (r, c) { }

  explicit SparseBoolMatrix (octave_idx_type r, octave_idx_type c, bool val)
    : Sparse<bool> (r, c, val) { }

  SparseBoolMatrix (const dim_vector& dv, octave_idx_type nz = 0)
    : Sparse<bool> (dv, nz) { }

  SparseBoolMatrix (const Sparse<bool>& a) : Sparse<bool> (a) { }

  SparseBoolMatrix (const SparseBoolMatrix& a) : Sparse<bool> (a) { }

  SparseBoolMatrix (const SparseBoolMatrix& a, const dim_vector& dv)
    : Sparse<bool> (a, dv) { }

  explicit SparseBoolMatrix (const boolMatrix& a) : Sparse<bool> (a) { }

  explicit SparseBoolMatrix (const boolNDArray& a) : Sparse<bool> (a) { }

  explicit SparseBoolMatrix (const PermMatrix& a) : Sparse<bool> (a) { };

  SparseBoolMatrix (const Array<bool>& a, const idx_vector& r,
                    const idx_vector& c, octave_idx_type nr = -1,
                    octave_idx_type nc = -1, bool sum_terms = true,
                    octave_idx_type nzm = -1)
    : Sparse<bool> (a, r, c, nr, nc, sum_terms, nzm) { }

  SparseBoolMatrix (octave_idx_type r, octave_idx_type c,
                    octave_idx_type num_nz) : Sparse<bool> (r, c, num_nz) { }

  SparseBoolMatrix& operator = (const SparseBoolMatrix& a)
  {
    Sparse<bool>::operator = (a);
    return *this;
  }

  bool operator == (const SparseBoolMatrix& a) const;
  bool operator != (const SparseBoolMatrix& a) const;

  SparseBoolMatrix transpose (void) const
  { return Sparse<bool>::transpose (); }

  // destructive insert/delete/reorder operations

  SparseBoolMatrix& insert (const SparseBoolMatrix& a,
                            octave_idx_type r, octave_idx_type c);

  SparseBoolMatrix& insert (const SparseBoolMatrix& a,
                            const Array<octave_idx_type>& indx);

  SparseBoolMatrix concat (const SparseBoolMatrix& rb,
                           const Array<octave_idx_type>& ra_idx);

  SparseBoolMatrix diag (octave_idx_type k = 0) const;

  boolMatrix matrix_value (void) const;

  SparseBoolMatrix squeeze (void) const;

  SparseBoolMatrix index (const idx_vector& i, bool resize_ok) const;

  SparseBoolMatrix index (const idx_vector& i, const idx_vector& j,
                          bool resize_ok) const;

  SparseBoolMatrix reshape (const dim_vector& new_dims) const;

  SparseBoolMatrix permute (const Array<octave_idx_type>& vec,
                            bool inv = false) const;

  SparseBoolMatrix ipermute (const Array<octave_idx_type>& vec) const;

  // unary operations

  SparseBoolMatrix operator ! (void) const;

  // other operations

  SparseBoolMatrix all (int dim = -1) const;
  SparseBoolMatrix any (int dim = -1) const;
  SparseMatrix sum (int dim = -1) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const SparseBoolMatrix& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               SparseBoolMatrix& a);
};

SPARSE_SMS_EQNE_OP_DECLS (SparseBoolMatrix, bool, OCTAVE_API)
SPARSE_SMS_BOOL_OP_DECLS (SparseBoolMatrix, bool, OCTAVE_API)

SPARSE_SSM_EQNE_OP_DECLS (bool, SparseBoolMatrix, OCTAVE_API)
SPARSE_SSM_BOOL_OP_DECLS (bool, SparseBoolMatrix, OCTAVE_API)

SPARSE_SMSM_EQNE_OP_DECLS (SparseBoolMatrix, SparseBoolMatrix, OCTAVE_API)
SPARSE_SMSM_BOOL_OP_DECLS (SparseBoolMatrix, SparseBoolMatrix, OCTAVE_API)

#endif
