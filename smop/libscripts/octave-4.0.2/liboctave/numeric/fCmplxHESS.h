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

#if !defined (octave_fCmplxHESS_h)
#define octave_fCmplxHESS_h 1

#include <iosfwd>

#include "fCMatrix.h"

class
OCTAVE_API
FloatComplexHESS
{
public:

  FloatComplexHESS (void) : hess_mat (), unitary_hess_mat () { }

  FloatComplexHESS (const FloatComplexMatrix& a)
    : hess_mat (), unitary_hess_mat ()
  {
    init (a);
  }

  FloatComplexHESS (const FloatComplexMatrix& a, octave_idx_type& info)
    : hess_mat (), unitary_hess_mat ()
  {
    info = init (a);
  }

  FloatComplexHESS (const FloatComplexHESS& a)
    : hess_mat (a.hess_mat), unitary_hess_mat (a.unitary_hess_mat) { }

  FloatComplexHESS& operator = (const FloatComplexHESS& a)
  {
    if (this != &a)
      {
        hess_mat = a.hess_mat;
        unitary_hess_mat = a.unitary_hess_mat;
      }
    return *this;
  }

  ~FloatComplexHESS (void) { }

  FloatComplexMatrix hess_matrix (void) const { return hess_mat; }

  FloatComplexMatrix unitary_hess_matrix (void) const
  {
    return unitary_hess_mat;
  }

  friend std::ostream& operator << (std::ostream& os,
                                    const FloatComplexHESS& a);

private:

  FloatComplexMatrix hess_mat;
  FloatComplexMatrix unitary_hess_mat;

  octave_idx_type init (const FloatComplexMatrix& a);
};

#endif
