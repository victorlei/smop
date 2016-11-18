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

#if !defined (octave_floatAEPBAL_h)
#define octave_floatAEPBAL_h 1

#include <iosfwd>
#include <string>

#include "base-aepbal.h"
#include "fMatrix.h"
#include "fColVector.h"

class
OCTAVE_API
FloatAEPBALANCE : public base_aepbal<FloatMatrix, FloatColumnVector>
{
public:

  FloatAEPBALANCE (void) : base_aepbal<FloatMatrix, FloatColumnVector> () { }

  FloatAEPBALANCE (const FloatMatrix& a, bool noperm = false,
                   bool noscal = false);

  FloatAEPBALANCE (const FloatAEPBALANCE& a)
    : base_aepbal<FloatMatrix, FloatColumnVector> (a) { }

  FloatMatrix balancing_matrix (void) const;
};

#endif
