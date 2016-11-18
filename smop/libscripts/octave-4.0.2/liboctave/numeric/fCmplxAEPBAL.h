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

#if !defined (octave_fCmplxAEPBAL_h)
#define octave_fCmplxAEPBAL_h 1

#include <iosfwd>
#include <string>

#include "base-aepbal.h"
#include "fCMatrix.h"
#include "fColVector.h"

class
OCTAVE_API
FloatComplexAEPBALANCE
  : public base_aepbal<FloatComplexMatrix, FloatColumnVector>
{
public:

  FloatComplexAEPBALANCE (void)
    : base_aepbal<FloatComplexMatrix, FloatColumnVector> () { }

  FloatComplexAEPBALANCE (const FloatComplexMatrix& a, bool noperm = false,
                          bool noscal = false);

  FloatComplexAEPBALANCE (const FloatComplexAEPBALANCE& a)
    : base_aepbal<FloatComplexMatrix, FloatColumnVector> (a) { }

  FloatComplexMatrix balancing_matrix (void) const;
};

#endif
