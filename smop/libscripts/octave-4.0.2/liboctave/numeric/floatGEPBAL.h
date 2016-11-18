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

#if !defined (octave_floatGEPBAL_h)
#define octave_floatGEPBAL_h 1

#include <iosfwd>
#include <string>

#include "fMatrix.h"

class
OCTAVE_API
FloatGEPBALANCE
{
public:

  FloatGEPBALANCE (void)
    : balanced_mat (), balanced_mat2 (), balancing_mat (), balancing_mat2 ()
  { }
  FloatGEPBALANCE (const FloatMatrix& a, const FloatMatrix& b,
                   const std::string& balance_job)
    : balanced_mat (), balanced_mat2 (), balancing_mat (), balancing_mat2 ()
  {
    init (a, b, balance_job);
  }

  FloatGEPBALANCE (const FloatGEPBALANCE& a)
    : balanced_mat (a.balanced_mat), balanced_mat2 (a.balanced_mat2),
      balancing_mat (a.balancing_mat), balancing_mat2 (a.balancing_mat2) { }

  FloatGEPBALANCE& operator = (const FloatGEPBALANCE& a)
  {
    if (this != &a)
      {
        balanced_mat = a.balanced_mat;
        balanced_mat2 = a.balanced_mat2;
        balancing_mat = a.balancing_mat;
        balancing_mat2 = a.balancing_mat2;
      }
    return *this;
  }

  ~FloatGEPBALANCE (void) { }

  FloatMatrix balanced_matrix (void) const { return balanced_mat; }

  FloatMatrix balanced_matrix2 (void) const { return balanced_mat2; }

  FloatMatrix balancing_matrix (void) const { return balancing_mat; }

  FloatMatrix balancing_matrix2 (void) const { return balancing_mat2; }

  friend std::ostream& operator << (std::ostream& os, const FloatGEPBALANCE& a);

private:

  FloatMatrix balanced_mat;
  FloatMatrix balanced_mat2;
  FloatMatrix balancing_mat;
  FloatMatrix balancing_mat2;

  octave_idx_type init (const FloatMatrix& a, const FloatMatrix& b,
                        const std::string& balance_job);
};

#endif
