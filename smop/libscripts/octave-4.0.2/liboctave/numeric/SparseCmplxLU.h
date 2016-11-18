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

#if !defined (octave_SparseCmplxLU_h)
#define octave_SparseCmplxLU_h 1

#include "sparse-base-lu.h"
#include "dSparse.h"
#include "CSparse.h"

class
OCTAVE_API
SparseComplexLU
  : public sparse_base_lu <SparseComplexMatrix, Complex, SparseMatrix, double>
{
public:

  SparseComplexLU (void)
    : sparse_base_lu <SparseComplexMatrix, Complex, SparseMatrix, double> () { }

  SparseComplexLU (const SparseComplexMatrix& a,
                   const Matrix& piv_thres = Matrix (),
                   bool scale = false);

  SparseComplexLU (const SparseComplexMatrix& a, const ColumnVector& Qinit,
                   const Matrix& piv_thres = Matrix (),
                   bool scale = false, bool FixedQ = false,
                   double droptol = -1., bool milu = false,
                   bool udiag = false);

  SparseComplexLU (const SparseComplexLU& a)
    : sparse_base_lu <SparseComplexMatrix, Complex, SparseMatrix, double> (a)
  { }

  SparseComplexLU& operator = (const SparseComplexLU& a)
  {
    if (this != &a)
      sparse_base_lu <SparseComplexMatrix, Complex, SparseMatrix, double>
                     :: operator = (a);

    return *this;
  }

  ~SparseComplexLU (void) { }
};

#endif
