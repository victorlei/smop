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

#if !defined (octave_fCmplxSVD_h)
#define octave_fCmplxSVD_h 1

#include <iosfwd>

#include "fDiagMatrix.h"
#include "fCMatrix.h"
#include "dbleSVD.h"

class
OCTAVE_API
FloatComplexSVD
{
public:

  FloatComplexSVD (void)
    : type_computed (), sigma (), left_sm (), right_sm ()
  { }

  FloatComplexSVD (const FloatComplexMatrix& a,
                   SVD::type svd_type = SVD::std,
                   SVD::driver svd_driver = SVD::GESVD)
    : type_computed (), sigma (), left_sm (), right_sm ()
  {
    init (a, svd_type, svd_driver);
  }

  FloatComplexSVD (const FloatComplexMatrix& a, octave_idx_type& info,
                   SVD::type svd_type = SVD::std,
                   SVD::driver svd_driver = SVD::GESVD)
    : type_computed (), sigma (), left_sm (), right_sm ()
  {
    info = init (a, svd_type, svd_driver);
  }

  FloatComplexSVD (const FloatComplexSVD& a)
    : type_computed (a.type_computed), sigma (a.sigma),
      left_sm (a.left_sm), right_sm (a.right_sm)
  { }

  FloatComplexSVD& operator = (const FloatComplexSVD& a)
  {
    if (this != &a)
      {
        type_computed = a.type_computed;
        sigma = a.sigma;
        left_sm = a.left_sm;
        right_sm = a.right_sm;
      }
    return *this;
  }

  ~FloatComplexSVD (void) { }

  FloatDiagMatrix singular_values (void) const { return sigma; }

  FloatComplexMatrix left_singular_matrix (void) const;

  FloatComplexMatrix right_singular_matrix (void) const;

  friend std::ostream&  operator << (std::ostream& os,
                                     const FloatComplexSVD& a);

private:

  SVD::type type_computed;

  FloatDiagMatrix sigma;
  FloatComplexMatrix left_sm;
  FloatComplexMatrix right_sm;

  octave_idx_type init (const FloatComplexMatrix& a,
                        SVD::type svd_type = SVD::std,
                        SVD::driver svd_driver = SVD::GESVD);
};

#endif
