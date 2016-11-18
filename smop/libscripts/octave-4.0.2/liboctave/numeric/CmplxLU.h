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

#if !defined (octave_CmplxLU_h)
#define octave_CmplxLU_h 1

#include "base-lu.h"
#include "dMatrix.h"
#include "CMatrix.h"

class
OCTAVE_API
ComplexLU : public base_lu <ComplexMatrix>
{
public:

  ComplexLU (void)
    : base_lu <ComplexMatrix> () { }

  ComplexLU (const ComplexMatrix& a);

  ComplexLU (const ComplexLU& a)
    : base_lu <ComplexMatrix> (a) { }

  ComplexLU (const ComplexMatrix& l, const ComplexMatrix& u,
             const PermMatrix& p)
    : base_lu <ComplexMatrix> (l, u, p) { }

  ComplexLU& operator = (const ComplexLU& a)
  {
    if (this != &a)
      base_lu <ComplexMatrix> :: operator = (a);

    return *this;
  }

  ~ComplexLU (void) { }

  void update (const ComplexColumnVector& u, const ComplexColumnVector& v);

  void update (const ComplexMatrix& u, const ComplexMatrix& v);

  void update_piv (const ComplexColumnVector& u, const ComplexColumnVector& v);

  void update_piv (const ComplexMatrix& u, const ComplexMatrix& v);
};

#endif
