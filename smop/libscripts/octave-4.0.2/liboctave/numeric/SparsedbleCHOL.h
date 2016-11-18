/*

Copyright (C) 2005-2015 David Bateman
Copyright (C) 1998-2005 Andy Adler

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

#if !defined (octave_SparsedbleCHOL_h)
#define octave_SparsedbleCHOL_h 1

#include "sparse-base-chol.h"
#include "dSparse.h"

class
OCTAVE_API
SparseCHOL : public sparse_base_chol <SparseMatrix, double, SparseMatrix>
{
public:

  SparseCHOL (void) : sparse_base_chol<SparseMatrix, double, SparseMatrix> ()
  { }

  SparseCHOL (const SparseMatrix& a, bool natural = true, bool force = false)
    : sparse_base_chol<SparseMatrix, double, SparseMatrix> (a, natural, force)
  { }

  SparseCHOL (const SparseMatrix& a, octave_idx_type& info,
              bool natural = false, bool force = false)
    : sparse_base_chol<SparseMatrix, double, SparseMatrix> (a, info, natural,
                                                            force)
  { }

  SparseCHOL (const SparseCHOL& a) :
    sparse_base_chol<SparseMatrix, double, SparseMatrix> (a) { }

  ~SparseCHOL (void) { }

  SparseCHOL& operator = (const SparseCHOL& a)
  {
    if (this != &a)
      sparse_base_chol <SparseMatrix, double, SparseMatrix> :: operator = (a);

    return *this;
  }

  SparseMatrix chol_matrix (void) const { return R (); }

  SparseMatrix L (void) const
  { return sparse_base_chol<SparseMatrix, double, SparseMatrix>:: L (); }

  SparseMatrix R (void) const
  { return sparse_base_chol<SparseMatrix, double, SparseMatrix>:: R (); }

  octave_idx_type P (void) const
  { return sparse_base_chol<SparseMatrix, double, SparseMatrix>:: P (); }

  ColumnVector perm (void) const
  { return sparse_base_chol<SparseMatrix, double, SparseMatrix>:: perm (); }

  SparseMatrix Q (void) const
  { return sparse_base_chol<SparseMatrix, double, SparseMatrix>:: Q (); }

  double rcond (void) const
  { return sparse_base_chol<SparseMatrix, double, SparseMatrix>:: rcond (); }

  // Compute the inverse of a matrix using the Cholesky factorization.
  SparseMatrix inverse (void) const
  {
    return sparse_base_chol<SparseMatrix, double, SparseMatrix>:: inverse ();
  }
};

SparseMatrix OCTAVE_API chol2inv (const SparseMatrix& r);

#endif
