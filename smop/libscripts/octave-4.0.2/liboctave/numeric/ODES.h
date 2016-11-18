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

#if !defined (octave_ODES_h)
#define octave_ODES_h 1

#include "ODESFunc.h"
#include "base-de.h"

class
ODES : public base_diff_eqn, public ODESFunc
{
public:

  ODES (void)
    : base_diff_eqn (), ODESFunc (), xdot (), theta () { }

  ODES (const ColumnVector& s, double tm, ODESFunc& f)
    : base_diff_eqn (s, tm), ODESFunc (f), xdot (s.length (), 0.0), theta () { }

  ODES (const ColumnVector& s, const ColumnVector& xtheta, double tm,
        ODESFunc& f)
    : base_diff_eqn (s, tm), ODESFunc (f), xdot (s.length (), 0.0),
      theta (xtheta) { }

  ODES (const ODES& a)
    : base_diff_eqn (a), ODESFunc (a), xdot (a.xdot), theta (a.theta) { }

  ODES& operator = (const ODES& a)
  {
    if (this != &a)
      {
        base_diff_eqn::operator = (a);
        ODESFunc::operator = (a);

        xdot = a.xdot;
        theta = a.theta;
      }
    return *this;
  }

  ~ODES (void) { }

  ColumnVector parameter_vector (void) { return theta; }

  void initialize (const ColumnVector& x, double t);

  void initialize (const ColumnVector& x, double t,
                   const ColumnVector& theta);

protected:

  // State vector time derivatives.
  ColumnVector xdot;

  // Parameter vector.
  ColumnVector theta;
};

#endif
