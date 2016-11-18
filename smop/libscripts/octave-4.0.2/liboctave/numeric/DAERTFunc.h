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

#if !defined (octave_DAERTFunc_h)
#define octave_DAERTFunc_h 1

#include "dMatrix.h"

class
DAERTFunc : public DAEFunc
{
public:

  typedef ColumnVector (*DAERTConstrFunc) (const ColumnVector& x, double t);

  DAERTFunc (void)
    : DAEFunc (), constr (0), reset (true) { }

  DAERTFunc (DAERHSFunc f)
    : DAEFunc (f), constr (0), reset (true) { }

  DAERTFunc (DAERHSFunc f, DAEJacFunc j)
    : DAEFunc (f, j), constr (0), reset (true) { }

  DAERTFunc (DAERHSFunc f, DAERTConstrFunc cf)
    : DAEFunc (f), constr (cf), reset (true) { }

  DAERTFunc (DAERHSFunc f, DAERTConstrFunc cf, DAEJacFunc j)
    : DAEFunc (f, j), constr (cf), reset (true) { }

  DAERTFunc (const DAERTFunc& a)
    : DAEFunc (a), constr (a.constr), reset (a.reset) { }

  DAERTFunc& operator = (const DAERTFunc& a)
  {
    if (this != &a)
      {
        DAEFunc::operator = (a);
        constr = a.constr;
        reset = a.reset;
      }
    return *this;
  }

  virtual ~DAERTFunc (void) { }

  DAERTConstrFunc constraint_function (void) const { return constr; }

  DAERTFunc& set_constraint_function (DAERTConstrFunc cf)
  {
    constr = cf;
    reset = true;
    return *this;
  }

protected:

  DAERTConstrFunc constr;

  // This variable is TRUE when this object is constructed, and also
  // after any internal data has changed.  Derived classes may use
  // this information (and change it) to know when to (re)initialize
  // their own internal data related to this object.

  bool reset;
};

#endif
