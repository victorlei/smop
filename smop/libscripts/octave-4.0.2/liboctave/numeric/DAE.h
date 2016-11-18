/*

Copyright (C) 1993-2015 John W. Eaton

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

#if !defined (octave_DAE_h)
#define octave_DAE_h 1

#include "DAEFunc.h"
#include "base-dae.h"

class
OCTAVE_API
DAE : public base_diff_alg_eqn, public DAEFunc
{
public:

  DAE (void)
    : base_diff_alg_eqn (), DAEFunc () { }

  DAE (const ColumnVector& xx, double tt, DAEFunc& f)
    : base_diff_alg_eqn (xx, tt), DAEFunc (f) { }

  DAE (const ColumnVector& xx, const ColumnVector& xxdot,
       double tt, DAEFunc& f)
    : base_diff_alg_eqn (xx, xxdot, tt), DAEFunc (f) { }

  DAE (const DAE& a)
    : base_diff_alg_eqn (a), DAEFunc (a) { }

  DAE& operator = (const DAE& a)
  {
    if (this != &a)
      {
        base_diff_alg_eqn::operator = (a);
        DAEFunc::operator = (a);
      }
    return *this;
  }

  virtual ~DAE (void) { }
};

#endif
