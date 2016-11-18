/*

Copyright (C) 1993-2015 John W. Eaton

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

#if !defined (octave_xpow_h)
#define octave_xpow_h 1

#include "oct-cmplx.h"

class Matrix;
class ComplexMatrix;
class FloatMatrix;
class FloatComplexMatrix;
class DiagMatrix;
class ComplexDiagMatrix;
class FloatDiagMatrix;
class FloatComplexDiagMatrix;
class PermMatrix;
class NDArray;
class FloatNDArray;
class ComplexNDArray;
class FloatComplexNDArray;
class octave_value;
class Range;

extern OCTINTERP_API octave_value xpow (double a, double b);
extern OCTINTERP_API octave_value xpow (double a, const Matrix& b);
extern OCTINTERP_API octave_value xpow (double a, const Complex& b);
extern OCTINTERP_API octave_value xpow (double a, const ComplexMatrix& b);

extern OCTINTERP_API octave_value xpow (const Matrix& a, double b);
extern OCTINTERP_API octave_value xpow (const Matrix& a, const Complex& b);

extern OCTINTERP_API octave_value xpow (const DiagMatrix& a, double b);
extern OCTINTERP_API octave_value xpow (const DiagMatrix& a, const Complex& b);

extern OCTINTERP_API octave_value xpow (const PermMatrix& a, double b);

extern OCTINTERP_API octave_value xpow (const Complex& a, double b);
extern OCTINTERP_API octave_value xpow (const Complex& a, const Matrix& b);
extern OCTINTERP_API octave_value xpow (const Complex& a, const Complex& b);
extern OCTINTERP_API octave_value xpow (const Complex& a,
                                        const ComplexMatrix& b);

extern OCTINTERP_API octave_value xpow (const ComplexMatrix& a, double b);
extern OCTINTERP_API octave_value xpow (const ComplexMatrix& a,
                                        const Complex& b);

extern OCTINTERP_API octave_value xpow (const ComplexDiagMatrix& a, double b);
extern OCTINTERP_API octave_value xpow (const ComplexDiagMatrix& a,
                                        const Complex& b);

extern OCTINTERP_API octave_value elem_xpow (double a, const Matrix& b);
extern OCTINTERP_API octave_value elem_xpow (double a, const ComplexMatrix& b);
extern OCTINTERP_API octave_value elem_xpow (double a, const Range& r);

extern OCTINTERP_API octave_value elem_xpow (const Matrix& a, double b);
extern OCTINTERP_API octave_value elem_xpow (const Matrix& a, const Matrix& b);
extern OCTINTERP_API octave_value elem_xpow (const Matrix& a, const Complex& b);
extern OCTINTERP_API octave_value elem_xpow (const Matrix& a,
                                             const ComplexMatrix& b);

extern OCTINTERP_API octave_value elem_xpow (const Complex& a, const Matrix& b);
extern OCTINTERP_API octave_value elem_xpow (const Complex& a,
                                             const ComplexMatrix& b);
extern OCTINTERP_API octave_value elem_xpow (const Complex& a, const Range& r);

extern OCTINTERP_API octave_value elem_xpow (const ComplexMatrix& a, double b);
extern OCTINTERP_API octave_value elem_xpow (const ComplexMatrix& a,
                                             const Matrix& b);
extern OCTINTERP_API octave_value elem_xpow (const ComplexMatrix& a,
                                             const Complex& b);
extern OCTINTERP_API octave_value elem_xpow (const ComplexMatrix& a,
                                             const ComplexMatrix& b);


extern OCTINTERP_API octave_value elem_xpow (double a, const NDArray& b);
extern OCTINTERP_API octave_value elem_xpow (double a, const ComplexNDArray& b);

extern OCTINTERP_API octave_value elem_xpow (const NDArray& a, double b);
extern OCTINTERP_API octave_value elem_xpow (const NDArray& a,
                                             const NDArray& b);
extern OCTINTERP_API octave_value elem_xpow (const NDArray& a,
                                             const Complex& b);
extern OCTINTERP_API octave_value elem_xpow (const NDArray& a,
                                             const ComplexNDArray& b);

extern OCTINTERP_API octave_value elem_xpow (const Complex& a,
                                             const NDArray& b);
extern OCTINTERP_API octave_value elem_xpow (const Complex& a,
                                             const ComplexNDArray& b);

extern OCTINTERP_API octave_value elem_xpow (const ComplexNDArray& a,
                                             double b);
extern OCTINTERP_API octave_value elem_xpow (const ComplexNDArray& a,
                                             const NDArray& b);
extern OCTINTERP_API octave_value elem_xpow (const ComplexNDArray& a,
                                             const Complex& b);
extern OCTINTERP_API octave_value elem_xpow (const ComplexNDArray& a,
                                             const ComplexNDArray& b);

extern OCTINTERP_API octave_value xpow (float a, float b);
extern OCTINTERP_API octave_value xpow (float a, const FloatMatrix& b);
extern OCTINTERP_API octave_value xpow (float a, const FloatComplex& b);
extern OCTINTERP_API octave_value xpow (float a, const FloatComplexMatrix& b);

extern OCTINTERP_API octave_value xpow (const FloatMatrix& a, float b);
extern OCTINTERP_API octave_value xpow (const FloatMatrix& a,
                                        const FloatComplex& b);

extern OCTINTERP_API octave_value xpow (const FloatDiagMatrix& a, float b);
extern OCTINTERP_API octave_value xpow (const FloatDiagMatrix& a,
                                        const FloatComplex& b);

extern OCTINTERP_API octave_value xpow (const FloatComplex& a, float b);
extern OCTINTERP_API octave_value xpow (const FloatComplex& a,
                                        const FloatMatrix& b);
extern OCTINTERP_API octave_value xpow (const FloatComplex& a,
                                        const FloatComplex& b);
extern OCTINTERP_API octave_value xpow (const FloatComplex& a,
                                        const FloatComplexMatrix& b);

extern OCTINTERP_API octave_value xpow (const FloatComplexMatrix& a, float b);
extern OCTINTERP_API octave_value xpow (const FloatComplexMatrix& a,
                                        const FloatComplex& b);

extern OCTINTERP_API octave_value xpow (const FloatComplexDiagMatrix& a,
                                        float b);
extern OCTINTERP_API octave_value xpow (const FloatComplexDiagMatrix& a,
                                        const FloatComplex& b);

extern OCTINTERP_API octave_value elem_xpow (float a, const FloatMatrix& b);
extern OCTINTERP_API octave_value elem_xpow (float a,
                                             const FloatComplexMatrix& b);

extern OCTINTERP_API octave_value elem_xpow (const FloatMatrix& a, float b);
extern OCTINTERP_API octave_value elem_xpow (const FloatMatrix& a,
                                             const FloatMatrix& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatMatrix& a,
                                             const FloatComplex& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatMatrix& a,
                                             const FloatComplexMatrix& b);

extern OCTINTERP_API octave_value elem_xpow (const FloatComplex& a,
                                             const FloatMatrix& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatComplex& a,
                                             const FloatComplexMatrix& b);

extern OCTINTERP_API octave_value elem_xpow (const FloatComplexMatrix& a,
                                             float b);
extern OCTINTERP_API octave_value elem_xpow (const FloatComplexMatrix& a,
                                             const FloatMatrix& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatComplexMatrix& a,
                                             const FloatComplex& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatComplexMatrix& a,
                                             const FloatComplexMatrix& b);


extern OCTINTERP_API octave_value elem_xpow (float a, const FloatNDArray& b);
extern OCTINTERP_API octave_value elem_xpow (float a,
                                             const FloatComplexNDArray& b);

extern OCTINTERP_API octave_value elem_xpow (const FloatNDArray& a, float b);
extern OCTINTERP_API octave_value elem_xpow (const FloatNDArray& a,
                                             const FloatNDArray& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatNDArray& a,
                                             const FloatComplex& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatNDArray& a,
                                             const FloatComplexNDArray& b);

extern OCTINTERP_API octave_value elem_xpow (const FloatComplex& a,
                                             const FloatNDArray& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatComplex& a,
                                             const FloatComplexNDArray& b);

extern OCTINTERP_API octave_value elem_xpow (const FloatComplexNDArray& a,
                                             float b);
extern OCTINTERP_API octave_value elem_xpow (const FloatComplexNDArray& a,
                                             const FloatNDArray& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatComplexNDArray& a,
                                             const FloatComplex& b);
extern OCTINTERP_API octave_value elem_xpow (const FloatComplexNDArray& a,
                                             const FloatComplexNDArray& b);

#endif
