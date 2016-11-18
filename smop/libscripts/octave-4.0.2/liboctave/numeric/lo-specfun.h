/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#if !defined (octave_lo_specfun_h)
#define octave_lo_specfun_h 1

#include "oct-cmplx.h"
#include "Array.h"

class Matrix;
class ComplexMatrix;
class NDArray;
class ComplexNDArray;
class RowVector;
class ComplexColumnVector;
class FloatMatrix;
class FloatComplexMatrix;
class FloatNDArray;
class FloatComplexNDArray;
class FloatRowVector;
class FloatComplexColumnVector;
class Range;

#if !defined (HAVE_ACOSH)
extern OCTAVE_API double acosh (double);
#endif

#if !defined (HAVE_ASINH)
extern OCTAVE_API double asinh (double);
#endif

#if !defined (HAVE_ATANH)
extern OCTAVE_API double atanh (double);
#endif

#if !defined (HAVE_ERF)
extern OCTAVE_API double erf (double);
#endif
extern OCTAVE_API Complex erf (const Complex& x);
extern OCTAVE_API FloatComplex erf (const FloatComplex& x);

#if !defined (HAVE_ERFC)
extern OCTAVE_API double erfc (double);
#endif
extern OCTAVE_API Complex erfc (const Complex& x);
extern OCTAVE_API FloatComplex erfc (const FloatComplex& x);

#if !defined (HAVE_ACOSHF)
extern OCTAVE_API float acoshf (float);
#endif

#if !defined (HAVE_ASINHF)
extern OCTAVE_API float asinhf (float);
#endif

#if !defined (HAVE_ATANHF)
extern OCTAVE_API float atanhf (float);
#endif

#if !defined (HAVE_ERFF)
extern OCTAVE_API float erff (float);
#endif

#if !defined (HAVE_ERFCF)
extern OCTAVE_API float erfcf (float);
#endif

#if !defined (HAVE_EXPM1)
extern OCTAVE_API double expm1 (double x);
#endif
extern OCTAVE_API Complex expm1 (const Complex& x);

#if !defined (HAVE_EXPM1F)
extern OCTAVE_API float expm1f (float x);
#endif
extern OCTAVE_API FloatComplex expm1 (const FloatComplex& x);

#if !defined (HAVE_LOG1P)
extern OCTAVE_API double log1p (double x);
#endif
extern OCTAVE_API Complex log1p (const Complex& x);

#if !defined (HAVE_LOG1PF)
extern OCTAVE_API float log1pf (float x);
#endif
extern OCTAVE_API FloatComplex log1p (const FloatComplex& x);

#if !defined (HAVE_CBRT)
extern OCTAVE_API double cbrt (double x);
#endif

#if !defined (HAVE_CBRTF)
extern OCTAVE_API float cbrtf (float x);
#endif

extern OCTAVE_API double xgamma (double x);
extern OCTAVE_API double xlgamma (double x);
extern OCTAVE_API Complex rc_lgamma (double x);

extern OCTAVE_API float xgamma (float x);
extern OCTAVE_API float xlgamma (float x);
extern OCTAVE_API FloatComplex rc_lgamma (float x);

extern OCTAVE_API Complex
besselj (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
bessely (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
besseli (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
besselk (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
besselh1 (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
besselh2 (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API ComplexMatrix
besselj (double alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (double alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (double alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (double alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (double alpha, const ComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (double alpha, const ComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselj (const Matrix& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (const Matrix& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (const Matrix& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (const Matrix& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (const Matrix& alpha, const Complex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (const Matrix& alpha, const Complex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselj (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (const Matrix& alpha, const ComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselj (double alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
bessely (double alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besseli (double alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselk (double alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh1 (double alpha, const ComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh2 (double alpha, const ComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselj (const NDArray& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
bessely (const NDArray& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besseli (const NDArray& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselk (const NDArray& alpha, const Complex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh1 (const NDArray& alpha, const Complex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh2 (const NDArray& alpha, const Complex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselj (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
bessely (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besseli (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselk (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh1 (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
besselh2 (const NDArray& alpha, const ComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselj (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
bessely (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besseli (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselk (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh1 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
besselh2 (const RowVector& alpha, const ComplexColumnVector& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplex
besselj (float alpha, const FloatComplex& x, bool scaled,
         octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
bessely (float alpha, const FloatComplex& x, bool scaled,
         octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besseli (float alpha, const FloatComplex& x, bool scaled,
         octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besselk (float alpha, const FloatComplex& x, bool scaled,
         octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besselh1 (float alpha, const FloatComplex& x, bool scaled,
          octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
besselh2 (float alpha, const FloatComplex& x, bool scaled,
          octave_idx_type& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (float alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (float alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (float alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (float alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (float alpha, const FloatComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (float alpha, const FloatComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (const FloatMatrix& alpha, const FloatComplex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (const FloatMatrix& alpha, const FloatComplexMatrix& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselj (float alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
bessely (float alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besseli (float alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselk (float alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh1 (float alpha, const FloatComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh2 (float alpha, const FloatComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselj (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
bessely (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besseli (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselk (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh1 (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh2 (const FloatNDArray& alpha, const FloatComplex& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselj (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
bessely (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besseli (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselk (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
         Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh1 (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
besselh2 (const FloatNDArray& alpha, const FloatComplexNDArray& x, bool scaled,
          Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselj (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
         bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
bessely (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
         bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besseli (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
         bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselk (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
         bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh1 (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
          bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
besselh2 (const FloatRowVector& alpha, const FloatComplexColumnVector& x,
          bool scaled, Array<octave_idx_type>& ierr);

extern OCTAVE_API Complex
airy (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API Complex
biry (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API ComplexMatrix
airy (const ComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexMatrix
biry (const ComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
airy (const ComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API ComplexNDArray
biry (const ComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplex
airy (const FloatComplex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplex
biry (const FloatComplex& z, bool deriv, bool scaled, octave_idx_type& ierr);

extern OCTAVE_API FloatComplexMatrix
airy (const FloatComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexMatrix
biry (const FloatComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
airy (const FloatComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API FloatComplexNDArray
biry (const FloatComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr);

extern OCTAVE_API double
betainc (double x, double a, double b);
extern OCTAVE_API Array<double>
betainc (double x, double a, const Array<double>& b);
extern OCTAVE_API Array<double>
betainc (double x, const Array<double>& a, double b);
extern OCTAVE_API Array<double>
betainc (double x, const Array<double>& a, const Array<double>& b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, double a, double b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, double a, double b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, double a, const Array<double>& b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, const Array<double>& a, double b);
extern OCTAVE_API Array<double>
betainc (const Array<double>& x, const Array<double>& a,
         const Array<double>& b);

extern OCTAVE_API float
betainc (float x, float a, float b);
extern OCTAVE_API Array<float>
betainc (float x, float a, const Array<float>& b);
extern OCTAVE_API Array<float>
betainc (float x, const Array<float>& a, float b);
extern OCTAVE_API Array<float>
betainc (float x, const Array<float>& a, const Array<float>& b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, float a, float b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, float a, float b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, float a, const Array<float>& b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, const Array<float>& a, float b);
extern OCTAVE_API Array<float>
betainc (const Array<float>& x, const Array<float>& a, const Array<float>& b);

extern OCTAVE_API double gammainc (double x, double a, bool& err);
extern OCTAVE_API Matrix gammainc (double x, const Matrix& a);
extern OCTAVE_API Matrix gammainc (const Matrix& x, double a);
extern OCTAVE_API Matrix gammainc (const Matrix& x, const Matrix& a);

extern OCTAVE_API NDArray gammainc (double x, const NDArray& a);
extern OCTAVE_API NDArray gammainc (const NDArray& x, double a);
extern OCTAVE_API NDArray gammainc (const NDArray& x, const NDArray& a);

inline double gammainc (double x, double a)
{
  bool err;
  return gammainc (x, a, err);
}

extern OCTAVE_API float gammainc (float x, float a, bool& err);
extern OCTAVE_API FloatMatrix gammainc (float x, const FloatMatrix& a);
extern OCTAVE_API FloatMatrix gammainc (const FloatMatrix& x, float a);
extern OCTAVE_API FloatMatrix
gammainc (const FloatMatrix& x, const FloatMatrix& a);

extern OCTAVE_API FloatNDArray gammainc (float x, const FloatNDArray& a);
extern OCTAVE_API FloatNDArray gammainc (const FloatNDArray& x, float a);
extern OCTAVE_API FloatNDArray
gammainc (const FloatNDArray& x, const FloatNDArray& a);

inline float gammainc (float x, float a)
{
  bool err;
  return gammainc (x, a, err);
}

extern OCTAVE_API Complex rc_log1p (double);
extern OCTAVE_API FloatComplex rc_log1p (float);

extern OCTAVE_API double erfinv (double x);
extern OCTAVE_API float erfinv (float x);

extern OCTAVE_API double erfcinv (double x);
extern OCTAVE_API float erfcinv (float x);

extern OCTAVE_API float erfcx (float x);
extern OCTAVE_API double erfcx (double x);
extern OCTAVE_API Complex erfcx (const Complex& x);
extern OCTAVE_API FloatComplex erfcx (const FloatComplex& x);

extern OCTAVE_API float erfi (float x);
extern OCTAVE_API double erfi (double x);
extern OCTAVE_API Complex erfi (const Complex& x);
extern OCTAVE_API FloatComplex erfi (const FloatComplex& x);

extern OCTAVE_API float dawson (float x);
extern OCTAVE_API double dawson (double x);
extern OCTAVE_API Complex dawson (const Complex& x);
extern OCTAVE_API FloatComplex dawson (const FloatComplex& x);

extern OCTAVE_API double betaincinv (double x, double a, double b);
extern OCTAVE_API Array<double>
betaincinv (double x, double a, const Array<double>& b);
extern OCTAVE_API Array<double>
betaincinv (double x, const Array<double>& a, double b);
extern OCTAVE_API Array<double>
betaincinv (double x, const Array<double>& a, const Array<double>& b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, double a, double b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, double a, double b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, double a, const Array<double>& b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, const Array<double>& a, double b);
extern OCTAVE_API Array<double>
betaincinv (const Array<double>& x, const Array<double>& a,
            const Array<double>& b);

extern OCTAVE_API void
ellipj (double u, double m, double& sn, double& cn, double& dn, double& err);
extern OCTAVE_API void
ellipj (const Complex& u, double m, Complex& sn, Complex& cn, Complex& dn,
        double& err);

#endif
