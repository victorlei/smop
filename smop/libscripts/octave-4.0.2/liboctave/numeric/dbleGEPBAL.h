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

#if !defined (octave_dbleGEPBAL_h)
#define octave_dbleGEPBAL_h 1

#include <iosfwd>
#include <string>

#include "dMatrix.h"

class
OCTAVE_API
GEPBALANCE
{
public:

  GEPBALANCE (void)
    : balanced_mat (), balanced_mat2 (), balancing_mat (), balancing_mat2 ()
  { }

  GEPBALANCE (const Matrix& a, const Matrix& b, const std::string& balance_job)
    : balanced_mat (), balanced_mat2 (), balancing_mat (), balancing_mat2 ()
  {
    init (a, b, balance_job);
  }

  GEPBALANCE (const GEPBALANCE& a)
    : balanced_mat (a.balanced_mat), balanced_mat2 (a.balanced_mat2),
      balancing_mat (a.balancing_mat), balancing_mat2 (a.balancing_mat2)
  { }

  GEPBALANCE& operator = (const GEPBALANCE& a)
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

  ~GEPBALANCE (void) { }

  Matrix balanced_matrix (void) const { return balanced_mat; }

  Matrix balanced_matrix2 (void) const { return balanced_mat2; }

  Matrix balancing_matrix (void) const { return balancing_mat; }

  Matrix balancing_matrix2 (void) const { return balancing_mat2; }

  friend std::ostream& operator << (std::ostream& os, const GEPBALANCE& a);

private:

  Matrix balanced_mat;
  Matrix balanced_mat2;
  Matrix balancing_mat;
  Matrix balancing_mat2;

  octave_idx_type init (const Matrix& a, const Matrix& b,
                        const std::string& balance_job);
};

#endif
