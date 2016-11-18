/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek

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

#if !defined (octave_CmplxAEPBAL_h)
#define octave_CmplxAEPBAL_h 1

#include <iosfwd>
#include <string>

#include "base-aepbal.h"
#include "CMatrix.h"
#include "dColVector.h"

class
OCTAVE_API
ComplexAEPBALANCE : public base_aepbal<ComplexMatrix, ColumnVector>
{
public:

  ComplexAEPBALANCE (void) : base_aepbal<ComplexMatrix, ColumnVector> () { }

  ComplexAEPBALANCE (const ComplexMatrix& a, bool noperm = false,
                     bool noscal = false);

  ComplexAEPBALANCE (const ComplexAEPBALANCE& a)
    : base_aepbal<ComplexMatrix, ColumnVector> (a) { }

  ComplexMatrix balancing_matrix (void) const;
};

#endif
