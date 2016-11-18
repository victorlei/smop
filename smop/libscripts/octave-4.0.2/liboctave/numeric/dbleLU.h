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

#if !defined (octave_dbleLU_h)
#define octave_dbleLU_h 1

#include "base-lu.h"
#include "dMatrix.h"

class
OCTAVE_API
LU : public base_lu <Matrix>
{
public:

  LU (void) : base_lu <Matrix> () { }

  LU (const Matrix& a);

  LU (const LU& a) : base_lu <Matrix> (a) { }

  LU (const Matrix& l, const Matrix& u, const PermMatrix& p)
    : base_lu <Matrix> (l, u, p) { }

  LU& operator = (const LU& a)
  {
    if (this != &a)
      base_lu <Matrix> :: operator = (a);

    return *this;
  }

  ~LU (void) { }

  void update (const ColumnVector& u, const ColumnVector& v);

  void update (const Matrix& u, const Matrix& v);

  void update_piv (const ColumnVector& u, const ColumnVector& v);

  void update_piv (const Matrix& u, const Matrix& v);
};

#endif
