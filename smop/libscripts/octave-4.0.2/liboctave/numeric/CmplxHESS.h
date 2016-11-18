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

#if !defined (octave_CmplxHESS_h)
#define octave_CmplxHESS_h 1

#include <iosfwd>

#include "CMatrix.h"

class
OCTAVE_API
ComplexHESS
{
public:

  ComplexHESS (void) : hess_mat (), unitary_hess_mat () { }

  ComplexHESS (const ComplexMatrix& a)
    : hess_mat (), unitary_hess_mat ()
  {
    init (a);
  }

  ComplexHESS (const ComplexMatrix& a, octave_idx_type& info)
    : hess_mat (), unitary_hess_mat ()
  {
    info = init (a);
  }

  ComplexHESS (const ComplexHESS& a)
    : hess_mat (a.hess_mat), unitary_hess_mat (a.unitary_hess_mat) { }

  ComplexHESS& operator = (const ComplexHESS& a)
  {
    if (this != &a)
      {
        hess_mat = a.hess_mat;
        unitary_hess_mat = a.unitary_hess_mat;
      }
    return *this;
  }

  ~ComplexHESS (void) { }

  ComplexMatrix hess_matrix (void) const { return hess_mat; }

  ComplexMatrix unitary_hess_matrix (void) const
  {
    return unitary_hess_mat;
  }

  friend std::ostream& operator << (std::ostream& os, const ComplexHESS& a);

private:

  ComplexMatrix hess_mat;
  ComplexMatrix unitary_hess_mat;

  octave_idx_type init (const ComplexMatrix& a);
};

#endif
