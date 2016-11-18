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

#if !defined (octave_ODEFunc_h)
#define octave_ODEFunc_h 1

class Matrix;
class ColumnVector;

class
ODEFunc
{
public:

  typedef ColumnVector (*ODERHSFunc) (const ColumnVector&, double);
  typedef Matrix (*ODEJacFunc) (const ColumnVector&, double);

  ODEFunc (void)
    : fun (0), jac (0), reset (true) { }

  ODEFunc (ODERHSFunc f)
    : fun (f), jac (0), reset (true) { }

  ODEFunc (ODERHSFunc f, ODEJacFunc j)
    : fun (f), jac (j), reset (true) { }

  ODEFunc (const ODEFunc& a)
    : fun (a.fun), jac (a.jac), reset (true) { }

  ODEFunc& operator = (const ODEFunc& a)
  {
    if (this != &a)
      {
        fun = a.fun;
        jac = a.jac;
        reset = a.reset;
      }
    return *this;
  }

  virtual ~ODEFunc (void) { }

  ODERHSFunc function (void) const { return fun; }

  ODEFunc& set_function (ODERHSFunc f)
  {
    fun = f;
    reset = true;
    return *this;
  }

  ODEJacFunc jacobian_function (void) const { return jac; }

  ODEFunc& set_jacobian_function (ODEJacFunc j)
  {
    jac = j;
    reset = true;
    return *this;
  }

protected:

  ODERHSFunc fun;
  ODEJacFunc jac;

  // This variable is TRUE when this object is constructed, and also
  // after any internal data has changed.  Derived classes may use
  // this information (and change it) to know when to (re)initialize
  // their own internal data related to this object.

  bool reset;
};

#endif
