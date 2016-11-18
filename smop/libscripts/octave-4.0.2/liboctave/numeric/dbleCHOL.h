/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek

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

#if !defined (octave_dbleCHOL_h)
#define octave_dbleCHOL_h 1

#include <iosfwd>

#include "dMatrix.h"
#include "dColVector.h"

class
OCTAVE_API
CHOL
{
public:

  CHOL (void) : chol_mat (), xrcond (0) { }

  CHOL (const Matrix& a, bool calc_cond = false)
    : chol_mat (), xrcond (0)
  {
    init (a, calc_cond);
  }

  CHOL (const Matrix& a, octave_idx_type& info, bool calc_cond = false)
    : chol_mat (), xrcond (0)
  {
    info = init (a, calc_cond);
  }

  CHOL (const CHOL& a) : chol_mat (a.chol_mat), xrcond (a.xrcond) { }

  CHOL& operator = (const CHOL& a)
  {
    if (this != &a)
      {
        chol_mat = a.chol_mat;
        xrcond = a.xrcond;
      }
    return *this;
  }

  Matrix chol_matrix (void) const { return chol_mat; }

  double rcond (void) const { return xrcond; }

  // Compute the inverse of a matrix using the Cholesky factorization.
  Matrix inverse (void) const;

  void set (const Matrix& R);

  void update (const ColumnVector& u);

  octave_idx_type downdate (const ColumnVector& u);

  octave_idx_type insert_sym (const ColumnVector& u, octave_idx_type j);

  void delete_sym (octave_idx_type j);

  void shift_sym (octave_idx_type i, octave_idx_type j);

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const CHOL& a);

private:

  Matrix chol_mat;

  double xrcond;

  octave_idx_type init (const Matrix& a, bool calc_cond);
};

Matrix OCTAVE_API chol2inv (const Matrix& r);

#endif
