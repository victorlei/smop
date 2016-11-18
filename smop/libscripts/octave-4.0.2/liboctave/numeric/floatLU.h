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

#if !defined (octave_floatLU_h)
#define octave_floatLU_h 1

#include "base-lu.h"
#include "dMatrix.h"
#include "fMatrix.h"

class
OCTAVE_API
FloatLU : public base_lu <FloatMatrix>
{
public:

  FloatLU (void) : base_lu <FloatMatrix> () { }

  FloatLU (const FloatMatrix& a);

  FloatLU (const FloatLU& a) : base_lu <FloatMatrix> (a) { }

  FloatLU (const FloatMatrix& l, const FloatMatrix& u,
           const PermMatrix& p)
    : base_lu <FloatMatrix> (l, u, p) { }

  FloatLU& operator = (const FloatLU& a)
  {
    if (this != &a)
      base_lu <FloatMatrix> :: operator = (a);

    return *this;
  }

  ~FloatLU (void) { }

  void update (const FloatColumnVector& u, const FloatColumnVector& v);

  void update (const FloatMatrix& u, const FloatMatrix& v);

  void update_piv (const FloatColumnVector& u, const FloatColumnVector& v);

  void update_piv (const FloatMatrix& u, const FloatMatrix& v);
};

#endif
