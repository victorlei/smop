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

#if !defined (octave_ODE_h)
#define octave_ODE_h 1

#include "ODEFunc.h"
#include "base-de.h"

class
ODE : public base_diff_eqn, public ODEFunc
{
public:

  ODE (void)
    : base_diff_eqn (), ODEFunc () { }

  ODE (const ColumnVector& s, double tm, const ODEFunc& f)
    : base_diff_eqn (s, tm), ODEFunc (f) { }

  ODE (const ODE& a)
    : base_diff_eqn (a), ODEFunc (a) { }

  ODE& operator = (const ODE& a)
  {
    if (this != &a)
      {
        base_diff_eqn::operator = (a);
        ODEFunc::operator = (a);
      }
    return *this;
  }

  virtual ~ODE (void) { }

  // Derived classes must provide functions to actually do the
  // integration.

  // Return the vector of states at output time t.
  virtual ColumnVector do_integrate (double tt) = 0;

  // Return a matrix of states at each output time specified by t.
  // The rows of the result matrix should each correspond to a new
  // output time.
  virtual Matrix do_integrate (const ColumnVector& tt) = 0;

  virtual Matrix do_integrate (const ColumnVector& tt,
                               const ColumnVector& ttcrit) = 0;

  // Lots of ways to call the single function and optionally set and
  // get additional information.

  // Integrate to t from current point.
  virtual ColumnVector integrate (double tt)
  { return do_integrate (tt); }

  // Set new x0, t0 and integrate to t.
  virtual ColumnVector integrate (const ColumnVector& x0, double t0, double tt)
  {
    initialize (x0, t0);
    return do_integrate (tt);
  }

  // Integrate from current point and return output at all points
  // specified by t.
  virtual Matrix integrate (const ColumnVector& tt)
  { return do_integrate (tt); }

  // Set new x0, t0 and integrate to return output at all points
  // specified by t.
  virtual Matrix integrate (const ColumnVector& x0, double t0,
                            const ColumnVector& tt)
  {
    initialize (x0, t0);
    return do_integrate (tt);
  }

  // Integrate from current point and return output at all points
  // specified by t.
  virtual Matrix integrate (const ColumnVector& tt,
                            const ColumnVector& ttcrit)
  { return do_integrate (tt, ttcrit); }

  // Set new x0, t0 and integrate to return output at all points
  // specified by t.
  virtual Matrix integrate (const ColumnVector& x0, double t0,
                            const ColumnVector& tt,
                            const ColumnVector& ttcrit)
  {
    initialize (x0, t0);
    return do_integrate (tt, ttcrit);
  }
};

#endif
