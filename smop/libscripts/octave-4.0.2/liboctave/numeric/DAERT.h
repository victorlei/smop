/*

Copyright (C) 2002-2015 John W. Eaton

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

#if !defined (octave_DAERT_h)
#define octave_DAERT_h 1

#include "DAE.h"
#include "DAERTFunc.h"
#include "base-dae.h"

class
DAERT : public base_diff_alg_eqn, public DAERTFunc
{
public:

  DAERT (void)
    : base_diff_alg_eqn (), DAERTFunc () { }

  DAERT (const ColumnVector& xx, double tt, DAERTFunc& f)
    : base_diff_alg_eqn (xx, tt), DAERTFunc (f) { }

  DAERT (const ColumnVector& xx, const ColumnVector& xxdot, double tt,
         DAERTFunc& f)
    : base_diff_alg_eqn (xx, xxdot, tt), DAERTFunc (f) { }

  DAERT (const DAERT& a)
    : base_diff_alg_eqn (a), DAERTFunc (a) { }

  DAERT& operator = (const DAERT& a)
  {
    if (this != &a)
      {
        base_diff_alg_eqn::operator = (a);
        DAERTFunc::operator = (a);

      }
    return *this;
  }

  virtual ~DAERT (void) { }

  void initialize (const ColumnVector& xx, const ColumnVector& xxdot,
                   double tt)
  {
    base_diff_alg_eqn::initialize (xx, xxdot, tt);
  }
};

#endif
