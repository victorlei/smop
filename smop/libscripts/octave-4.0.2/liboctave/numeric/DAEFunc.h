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

#if !defined (octave_DAEFunc_h)
#define octave_DAEFunc_h 1

class Matrix;
class ColumnVector;

class
DAEFunc
{
public:

  typedef ColumnVector (*DAERHSFunc) (const ColumnVector& x,
                                      const ColumnVector& xdot,
                                      double t, octave_idx_type& ires);

  // This is really the form used by DASSL:
  //
  //   PD = DG/DY + CJ * DG/DYPRIME

  typedef Matrix (*DAEJacFunc) (const ColumnVector& x,
                                const ColumnVector& xdot,
                                double t, double cj);

  DAEFunc (void)
    : fun (0), jac (0), reset (true) { }

  DAEFunc (DAERHSFunc f)
    : fun (f), jac (0), reset (true) { }

  DAEFunc (DAERHSFunc f, DAEJacFunc j)
    : fun (f), jac (j), reset (true) { }

  DAEFunc (const DAEFunc& a)
    : fun (a.fun), jac (a.jac), reset (a.reset) { }

  DAEFunc& operator = (const DAEFunc& a)
  {
    if (this != &a)
      {
        fun = a.fun;
        jac = a.jac;
        reset = a.reset;
      }
    return *this;
  }

  virtual ~DAEFunc (void) { }

  DAERHSFunc function (void) const { return fun; }

  DAEFunc& set_function (DAERHSFunc f)
  {
    fun = f;
    reset = true;
    return *this;
  }

  DAEJacFunc jacobian_function (void) const { return jac; }

  DAEFunc& set_jacobian_function (DAEJacFunc j)
  {
    jac = j;
    reset = true;
    return *this;
  }

protected:

  DAERHSFunc fun;
  DAEJacFunc jac;

  // This variable is TRUE when this object is constructed, and also
  // after any internal data has changed.  Derived classes may use
  // this information (and change it) to know when to (re)initialize
  // their own internal data related to this object.

  bool reset;
};

#endif
