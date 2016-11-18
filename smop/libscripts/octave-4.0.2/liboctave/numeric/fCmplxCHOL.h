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

#if !defined (octave_fCmplxCHOL_h)
#define octave_fCmplxCHOL_h 1

#include <iosfwd>

#include "fCMatrix.h"
#include "fCColVector.h"

class
OCTAVE_API
FloatComplexCHOL
{
public:

  FloatComplexCHOL (void) : chol_mat (), xrcond (0) { }

  FloatComplexCHOL (const FloatComplexMatrix& a, bool calc_cond = false)
    : chol_mat (), xrcond (0)
  {
    init (a, calc_cond);
  }

  FloatComplexCHOL (const FloatComplexMatrix& a, octave_idx_type& info,
                    bool calc_cond = false)
    : chol_mat (), xrcond (0)
  {
    info = init (a, calc_cond);
  }

  FloatComplexCHOL (const FloatComplexCHOL& a)
    : chol_mat (a.chol_mat), xrcond (a.xrcond) { }

  FloatComplexCHOL& operator = (const FloatComplexCHOL& a)
  {
    if (this != &a)
      {
        chol_mat = a.chol_mat;
        xrcond = a.xrcond;
      }

    return *this;
  }

  FloatComplexMatrix chol_matrix (void) const { return chol_mat; }

  float rcond (void) const { return xrcond; }

  FloatComplexMatrix inverse (void) const;

  void set (const FloatComplexMatrix& R);

  void update (const FloatComplexColumnVector& u);

  octave_idx_type downdate (const FloatComplexColumnVector& u);

  octave_idx_type insert_sym (const FloatComplexColumnVector& u,
                              octave_idx_type j);

  void delete_sym (octave_idx_type j);

  void shift_sym (octave_idx_type i, octave_idx_type j);

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const FloatComplexCHOL& a);

private:

  FloatComplexMatrix chol_mat;

  float xrcond;

  octave_idx_type init (const FloatComplexMatrix& a, bool calc_cond);
};

FloatComplexMatrix OCTAVE_API chol2inv (const FloatComplexMatrix& r);

#endif
