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

#if !defined (octave_mx_base_h)
#define octave_mx_base_h 1

// Matrix Type class

#include "MatrixType.h"

// Matrix classes.

#include "boolMatrix.h"
#include "chMatrix.h"
#include "dMatrix.h"
#include "CMatrix.h"
#include "fMatrix.h"
#include "fCMatrix.h"

// Column Vector classes.

#include "dColVector.h"
#include "CColVector.h"
#include "fColVector.h"
#include "fCColVector.h"

// Row Vector classes.

#include "dRowVector.h"
#include "CRowVector.h"
#include "fRowVector.h"
#include "fCRowVector.h"

// Diagonal Matrix classes.

#include "dDiagMatrix.h"
#include "CDiagMatrix.h"
#include "fDiagMatrix.h"
#include "fCDiagMatrix.h"

// Permutation matrix class
#include "PermMatrix.h"

// Sparse Matrix classes.

#include "boolSparse.h"
#include "dSparse.h"
#include "CSparse.h"

// N-d Array classes.

#include "boolNDArray.h"
#include "chNDArray.h"
#include "dNDArray.h"
#include "CNDArray.h"
#include "fNDArray.h"
#include "fCNDArray.h"

#include "int8NDArray.h"
#include "int16NDArray.h"
#include "int32NDArray.h"
#include "int64NDArray.h"

#include "uint8NDArray.h"
#include "uint16NDArray.h"
#include "uint32NDArray.h"
#include "uint64NDArray.h"

#endif
