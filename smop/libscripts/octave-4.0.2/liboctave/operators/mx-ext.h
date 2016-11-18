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

#if !defined (octave_mx_ext_h)
#define octave_mx_ext_h 1

// Result of a AEP Balance operation.

#include "dbleAEPBAL.h"
#include "CmplxAEPBAL.h"

// Result of a Determinant calculation.

#include "DET.h"

// Result of a Cholesky Factorization

#include "dbleCHOL.h"
#include "CmplxCHOL.h"
#include "floatCHOL.h"
#include "fCmplxCHOL.h"

// Result of a Hessenberg Decomposition

#include "dbleHESS.h"
#include "CmplxHESS.h"

// Result of a Schur Decomposition

#include "dbleSCHUR.h"
#include "CmplxSCHUR.h"
#include "floatSCHUR.h"
#include "fCmplxSCHUR.h"

// Result of a Singular Value Decomposition.

#include "dbleSVD.h"
#include "CmplxSVD.h"
#include "floatSVD.h"
#include "fCmplxSVD.h"

// Result of an Eigenvalue computation.

#include "EIG.h"

// Result of an LU decomposition.

#include "dbleLU.h"
#include "CmplxLU.h"
#include "floatLU.h"
#include "fCmplxLU.h"

// Result of a QR decomposition.

#include "dbleQR.h"
#include "CmplxQR.h"

#include "dbleQRP.h"
#include "CmplxQRP.h"

#endif
