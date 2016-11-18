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

#if !defined (octave_fCmplxSCHUR_h)
#define octave_fCmplxSCHUR_h 1

#include <iosfwd>
#include <string>

#include "fCMatrix.h"
#include "floatSCHUR.h"

class
OCTAVE_API
FloatComplexSCHUR
{
public:

  FloatComplexSCHUR (void) : schur_mat (), unitary_mat (), selector (0) { }

  FloatComplexSCHUR (const FloatComplexMatrix& a, const std::string& ord,
                     bool calc_unitary = true)
    : schur_mat (), unitary_mat (), selector (0)
  {
    init (a, ord, calc_unitary);
  }

  FloatComplexSCHUR (const FloatComplexMatrix& a, const std::string& ord,
                     octave_idx_type& info, bool calc_unitary = true)
    : schur_mat (), unitary_mat (), selector (0)
  {
    info = init (a, ord, calc_unitary);
  }

  FloatComplexSCHUR (const FloatComplexSCHUR& a)
    : schur_mat (a.schur_mat), unitary_mat (a.unitary_mat), selector (0)
  { }

  FloatComplexSCHUR (const FloatComplexMatrix& s, const FloatComplexMatrix& u);

  FloatComplexSCHUR (const FloatSCHUR& s);

  FloatComplexSCHUR& operator = (const FloatComplexSCHUR& a)
  {
    if (this != &a)
      {
        schur_mat = a.schur_mat;
        unitary_mat = a.unitary_mat;
      }
    return *this;
  }

  ~FloatComplexSCHUR (void) { }

  FloatComplexMatrix schur_matrix (void) const { return schur_mat; }

  FloatComplexMatrix unitary_matrix (void) const { return unitary_mat; }

  friend std::ostream& operator << (std::ostream& os,
                                    const FloatComplexSCHUR& a);

  typedef octave_idx_type (*select_function) (const FloatComplex&);

private:

  FloatComplexMatrix schur_mat;
  FloatComplexMatrix unitary_mat;

  select_function selector;

  octave_idx_type init (const FloatComplexMatrix& a, const std::string& ord,
                        bool calc_unitary);
};

#endif
