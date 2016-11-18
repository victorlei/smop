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

#if !defined (octave_floatCHOL_h)
#define octave_floatCHOL_h 1

#include <iosfwd>

#include "fMatrix.h"
#include "fColVector.h"

class
OCTAVE_API
FloatCHOL
{
public:

  FloatCHOL (void) : chol_mat (), xrcond (0) { }

  FloatCHOL (const FloatMatrix& a, bool calc_cond = false)
    : chol_mat (), xrcond (0)
  {
    init (a, calc_cond);
  }

  FloatCHOL (const FloatMatrix& a, octave_idx_type& info,
             bool calc_cond = false)
    : chol_mat (), xrcond (0)
  {
    info = init (a, calc_cond);
  }

  FloatCHOL (const FloatCHOL& a) : chol_mat (a.chol_mat), xrcond (a.xrcond) { }

  FloatCHOL& operator = (const FloatCHOL& a)
  {
    if (this != &a)
      {
        chol_mat = a.chol_mat;
        xrcond = a.xrcond;
      }
    return *this;
  }

  FloatMatrix chol_matrix (void) const { return chol_mat; }

  float rcond (void) const { return xrcond; }

  // Compute the inverse of a matrix using the Cholesky factorization.
  FloatMatrix inverse (void) const;

  void set (const FloatMatrix& R);

  void update (const FloatColumnVector& u);

  octave_idx_type downdate (const FloatColumnVector& u);

  octave_idx_type insert_sym (const FloatColumnVector& u, octave_idx_type j);

  void delete_sym (octave_idx_type j);

  void shift_sym (octave_idx_type i, octave_idx_type j);

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const FloatCHOL& a);

private:

  FloatMatrix chol_mat;

  float xrcond;

  octave_idx_type init (const FloatMatrix& a, bool calc_cond);
};

FloatMatrix OCTAVE_API chol2inv (const FloatMatrix& r);

#endif
