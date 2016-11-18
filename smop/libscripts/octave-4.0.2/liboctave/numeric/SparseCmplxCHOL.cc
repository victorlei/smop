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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "SparseCmplxCHOL.h"

// Instantiate the base CHOL class for the type we need
#define OCTAVE_CHOLMOD_TYPE CHOLMOD_COMPLEX
#include "sparse-base-chol.h"
#include "sparse-base-chol.cc"
template class sparse_base_chol <SparseComplexMatrix, Complex, SparseMatrix>;

// Compute the inverse of a matrix using the Cholesky factorization.
SparseComplexMatrix
chol2inv (const SparseComplexMatrix& r)
{
  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();
  SparseComplexMatrix retval;

  if (r_nr == r_nc)
    {
      MatrixType mattype (r);
      int typ = mattype.type (false);
      double rcond;
      octave_idx_type info;
      SparseComplexMatrix rinv;

      if (typ == MatrixType::Upper)
        {
          rinv = r.inverse (mattype, info, rcond, true, false);
          retval = rinv.transpose () * rinv;
        }
      else if (typ == MatrixType::Lower)
        {
          rinv = r.transpose ().inverse (mattype, info, rcond, true, false);
          retval = rinv.transpose () * rinv;
        }
      else
        (*current_liboctave_error_handler)
          ("U must be a triangular matrix");
    }
  else
    (*current_liboctave_error_handler) ("U must be a square matrix");

  return retval;
}
