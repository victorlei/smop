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

#if !defined (octave_mx_defs_h)
#define octave_mx_defs_h 1

// Classes we declare.

class Matrix;
class ComplexMatrix;
class FloatMatrix;
class FloatComplexMatrix;
class boolMatrix;
class charMatrix;

class NDArray;
class ComplexNDArray;
class FloatNDArray;
class FloatComplexNDArray;
class boolNDArray;
class charNDArray;

class ColumnVector;
class ComplexColumnVector;
class FloatColumnVector;
class FloatComplexColumnVector;

class RowVector;
class ComplexRowVector;
class FloatRowVector;
class FloatComplexRowVector;

class DiagMatrix;
class ComplexDiagMatrix;
class FloatDiagMatrix;
class FloatComplexDiagMatrix;

class PermMatrix;

class AEPBALANCE;
class ComplexAEPBALANCE;
class FloatAEPBALANCE;
class FloatComplexAEPBALANCE;

class GEPBALANCE;
class ComplexGEPBALANCE;
class FloatGEPBALANCE;
class FloatComplexGEPBALANCE;

class CHOL;
class ComplexCHOL;
class FloatCHOL;
class FloatComplexCHOL;

class EIG;

class HESS;
class ComplexHESS;
class FloatHESS;
class FloatComplexHESS;

class SCHUR;
class ComplexSCHUR;
class FloatSCHUR;
class FloatComplexSCHUR;

class SVD;
class ComplexSVD;
class FloatSVD;
class FloatComplexSVD;

class LU;
class ComplexLU;
class FloatLU;
class FloatComplexLU;

class QR;
class ComplexQR;
class FloatQR;
class FloatComplexQR;

class QRP;
class ComplexQRP;
class FloatQRP;
class FloatComplexQRP;

// Other data types we use but that don't always need to have full
// declarations.

#include "oct-cmplx.h"

#ifndef MAPPER_FCN_TYPEDEFS
#define MAPPER_FCN_TYPEDEFS 1

typedef bool (*b_d_Mapper)(double);
typedef bool (*b_c_Mapper)(const Complex&);

typedef double (*d_d_Mapper)(double);
typedef double (*d_c_Mapper)(const Complex&);
typedef Complex (*c_c_Mapper)(const Complex&);

typedef bool (*b_f_Mapper)(float);
typedef bool (*b_fc_Mapper)(const FloatComplex&);

typedef float (*f_f_Mapper)(float);
typedef float (*f_fc_Mapper)(const FloatComplex&);
typedef FloatComplex (*fc_fc_Mapper)(const FloatComplex&);

enum blas_trans_type
{
  blas_no_trans = 'N',
  blas_trans = 'T',
  blas_conj_trans = 'C'
};

inline char
get_blas_char (blas_trans_type transt)
{
  return static_cast<char> (transt);
}


#endif

#endif
