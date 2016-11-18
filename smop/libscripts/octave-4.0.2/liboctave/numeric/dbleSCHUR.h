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

#if !defined (octave_dbleSCHUR_h)
#define octave_dbleSCHUR_h 1

#include <iosfwd>
#include <string>

#include "dMatrix.h"

class
OCTAVE_API
SCHUR
{
public:

  SCHUR (void) : schur_mat (), unitary_mat (), selector (0) { }

  SCHUR (const Matrix& a, const std::string& ord, bool calc_unitary = true)
    : schur_mat (), unitary_mat (), selector (0)
  {
    init (a, ord, calc_unitary);
  }

  SCHUR (const Matrix& a, const std::string& ord, int& info,
         bool calc_unitary = true)
    : schur_mat (), unitary_mat (), selector (0)
  {
    info = init (a, ord, calc_unitary);
  }

  SCHUR (const SCHUR& a)
    : schur_mat (a.schur_mat), unitary_mat (a.unitary_mat), selector (0)
  { }

  SCHUR (const Matrix& s, const Matrix& u);

  SCHUR& operator = (const SCHUR& a)
  {
    if (this != &a)
      {
        schur_mat = a.schur_mat;
        unitary_mat = a.unitary_mat;
      }
    return *this;
  }

  ~SCHUR (void) { }

  Matrix schur_matrix (void) const { return schur_mat; }

  Matrix unitary_matrix (void) const { return unitary_mat; }

  friend std::ostream& operator << (std::ostream& os, const SCHUR& a);

  typedef octave_idx_type (*select_function) (const double&, const double&);

private:

  Matrix schur_mat;
  Matrix unitary_mat;

  select_function selector;

  octave_idx_type init (const Matrix& a, const std::string& ord,
                        bool calc_unitary);
};

#endif
