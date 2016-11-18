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

#if !defined (octave_fEIG_h)
#define octave_fEIG_h 1

#include <iosfwd>

#include "fMatrix.h"
#include "fCMatrix.h"
#include "fCColVector.h"

class
OCTAVE_API
FloatEIG
{
  friend class FloatMatrix;
  friend class FloatComplexMatrix;

public:

  FloatEIG (void)
    : lambda (), v () { }

  FloatEIG (const FloatMatrix& a, bool calc_eigenvectors = true)
    : lambda (), v ()
  {
    init (a, calc_eigenvectors);
  }

  FloatEIG (const FloatMatrix& a, octave_idx_type& info,
            bool calc_eigenvectors = true)
    : lambda (), v ()
  {
    info = init (a, calc_eigenvectors);
  }

  FloatEIG (const FloatMatrix& a, const FloatMatrix& b,
            bool calc_eigenvectors = true)
    : lambda (), v ()
  {
    init (a, b, calc_eigenvectors);
  }

  FloatEIG (const FloatMatrix& a, const FloatMatrix& b, octave_idx_type& info,
            bool calc_eigenvectors = true)
    : lambda (), v ()
  {
    info = init (a, b, calc_eigenvectors);
  }

  FloatEIG (const FloatComplexMatrix& a, bool calc_eigenvectors = true)
    : lambda (), v ()
  {
    init (a, calc_eigenvectors);
  }

  FloatEIG (const FloatComplexMatrix& a, octave_idx_type& info,
            bool calc_eigenvectors = true)
    : lambda (), v ()
  {
    info = init (a, calc_eigenvectors);
  }

  FloatEIG (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
            bool calc_eigenvectors = true)
    : lambda (), v ()
  {
    init (a, b, calc_eigenvectors);
  }

  FloatEIG (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
            octave_idx_type& info, bool calc_eigenvectors = true)
    : lambda (), v ()
  {
    info = init (a, b, calc_eigenvectors);
  }

  FloatEIG (const FloatEIG& a) : lambda (a.lambda), v (a.v) { }

  FloatEIG& operator = (const FloatEIG& a)
  {
    if (this != &a)
      {
        lambda = a.lambda;
        v = a.v;
      }
    return *this;
  }

  ~FloatEIG (void) { }

  FloatComplexColumnVector eigenvalues (void) const { return lambda; }

  FloatComplexMatrix eigenvectors (void) const { return v; }

  friend std::ostream&  operator << (std::ostream& os, const FloatEIG& a);

private:

  FloatComplexColumnVector lambda;
  FloatComplexMatrix v;

  octave_idx_type init (const FloatMatrix& a, bool calc_eigenvectors);
  octave_idx_type init (const FloatMatrix& a, const FloatMatrix& b,
                        bool calc_eigenvectors);
  octave_idx_type init (const FloatComplexMatrix& a, bool calc_eigenvectors);
  octave_idx_type init (const FloatComplexMatrix& a,
                        const FloatComplexMatrix& b, bool calc_eigenvectors);

  octave_idx_type symmetric_init (const FloatMatrix& a, bool calc_eigenvectors);
  octave_idx_type symmetric_init (const FloatMatrix& a, const FloatMatrix& b,
                                  bool calc_eigenvectors);
  octave_idx_type hermitian_init (const FloatComplexMatrix& a,
                                  bool calc_eigenvectors);
  octave_idx_type hermitian_init (const FloatComplexMatrix& a,
                                  const FloatComplexMatrix& b,
                                  bool calc_eigenvectors);
};

#endif
