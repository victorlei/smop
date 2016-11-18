/*

Copyright (C) 2008-2015 VZLU Prague, a.s.

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

// author: Jaroslav Hajek <highegg@gmail.com>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cfloat>
#include <cmath>

#include <iostream>
#include <vector>

#include "oct-cmplx.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "mx-cm-s.h"
#include "mx-s-cm.h"
#include "mx-fcm-fs.h"
#include "mx-fs-fcm.h"
#include "Array.h"
#include "Array-util.h"
#include "CMatrix.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "CColVector.h"
#include "dColVector.h"
#include "CRowVector.h"
#include "dRowVector.h"
#include "fCColVector.h"
#include "fColVector.h"
#include "fCRowVector.h"
#include "fRowVector.h"
#include "CSparse.h"
#include "dSparse.h"
#include "dbleSVD.h"
#include "CmplxSVD.h"
#include "floatSVD.h"
#include "fCmplxSVD.h"

// Theory: norm accumulator is an object that has an accum method able
// to handle both real and complex element, and a cast operator
// returning the intermediate norm. Reference: Higham, N. "Estimating
// the Matrix p-Norm." Numer. Math. 62, 539-555, 1992.

// norm accumulator for the p-norm
template <class R>
class norm_accumulator_p
{
  R p,scl,sum;
public:
  norm_accumulator_p () {} // we need this one for Array
  norm_accumulator_p (R pp) : p(pp), scl(0), sum(1) {}

  template<class U>
  void accum (U val)
  {
    octave_quit ();
    R t = std::abs (val);
    if (scl == t) // we need this to handle Infs properly
      sum += 1;
    else if (scl < t)
      {
        sum *= std::pow (scl/t, p);
        sum += 1;
        scl = t;
      }
    else if (t != 0)
      sum += std::pow (t/scl, p);
  }
  operator R () { return scl * std::pow (sum, 1/p); }
};

// norm accumulator for the minus p-pseudonorm
template <class R>
class norm_accumulator_mp
{
  R p,scl,sum;
public:
  norm_accumulator_mp () {} // we need this one for Array
  norm_accumulator_mp (R pp) : p(pp), scl(0), sum(1) {}

  template<class U>
  void accum (U val)
  {
    octave_quit ();
    R t = 1 / std::abs (val);
    if (scl == t)
      sum += 1;
    else if (scl < t)
      {
        sum *= std::pow (scl/t, p);
        sum += 1;
        scl = t;
      }
    else if (t != 0)
      sum += std::pow (t/scl, p);
  }
  operator R () { return scl * std::pow (sum, -1/p); }
};

// norm accumulator for the 2-norm (euclidean)
template <class R>
class norm_accumulator_2
{
  R scl,sum;
  static R pow2 (R x) { return x*x; }
public:
  norm_accumulator_2 () : scl(0), sum(1) {}

  void accum (R val)
  {
    R t = std::abs (val);
    if (scl == t)
      sum += 1;
    else if (scl < t)
      {
        sum *= pow2 (scl/t);
        sum += 1;
        scl = t;
      }
    else if (t != 0)
      sum += pow2 (t/scl);
  }

  void accum (std::complex<R> val)
  {
    accum (val.real ());
    accum (val.imag ());
  }

  operator R () { return scl * std::sqrt (sum); }
};

// norm accumulator for the 1-norm (city metric)
template <class R>
class norm_accumulator_1
{
  R sum;
public:
  norm_accumulator_1 () : sum (0) {}
  template<class U>
  void accum (U val)
  {
    sum += std::abs (val);
  }
  operator R () { return sum; }
};

// norm accumulator for the inf-norm (max metric)
template <class R>
class norm_accumulator_inf
{
  R max;
public:
  norm_accumulator_inf () : max (0) {}
  template<class U>
  void accum (U val)
  {
    max = std::max (max, std::abs (val));
  }
  operator R () { return max; }
};

// norm accumulator for the -inf pseudonorm (min abs value)
template <class R>
class norm_accumulator_minf
{
  R min;
public:
  norm_accumulator_minf () : min (octave_Inf) {}
  template<class U>
  void accum (U val)
  {
    min = std::min (min, std::abs (val));
  }
  operator R () { return min; }
};

// norm accumulator for the 0-pseudonorm (hamming distance)
template <class R>
class norm_accumulator_0
{
  unsigned int num;
public:
  norm_accumulator_0 () : num (0) {}
  template<class U>
  void accum (U val)
  {
    if (val != static_cast<U> (0)) ++num;
  }
  operator R () { return num; }
};


// OK, we're armed :) Now let's go for the fun

template <class T, class R, class ACC>
inline void vector_norm (const Array<T>& v, R& res, ACC acc)
{
  for (octave_idx_type i = 0; i < v.numel (); i++)
    acc.accum (v(i));

  res = acc;
}

// dense versions
template <class T, class R, class ACC>
void column_norms (const MArray<T>& m, MArray<R>& res, ACC acc)
{
  res = MArray<R> (dim_vector (1, m.columns ()));
  for (octave_idx_type j = 0; j < m.columns (); j++)
    {
      ACC accj = acc;
      for (octave_idx_type i = 0; i < m.rows (); i++)
        accj.accum (m(i, j));

      res.xelem (j) = accj;
    }
}

template <class T, class R, class ACC>
void row_norms (const MArray<T>& m, MArray<R>& res, ACC acc)
{
  res = MArray<R> (dim_vector (m.rows (), 1));
  std::vector<ACC> acci (m.rows (), acc);
  for (octave_idx_type j = 0; j < m.columns (); j++)
    {
      for (octave_idx_type i = 0; i < m.rows (); i++)
        acci[i].accum (m(i, j));
    }

  for (octave_idx_type i = 0; i < m.rows (); i++)
    res.xelem (i) = acci[i];
}

// sparse versions
template <class T, class R, class ACC>
void column_norms (const MSparse<T>& m, MArray<R>& res, ACC acc)
{
  res = MArray<R> (dim_vector (1, m.columns ()));
  for (octave_idx_type j = 0; j < m.columns (); j++)
    {
      ACC accj = acc;
      for (octave_idx_type k = m.cidx (j); k < m.cidx (j+1); k++)
        accj.accum (m.data (k));

      res.xelem (j) = accj;
    }
}

template <class T, class R, class ACC>
void row_norms (const MSparse<T>& m, MArray<R>& res, ACC acc)
{
  res = MArray<R> (dim_vector (m.rows (), 1));
  std::vector<ACC> acci (m.rows (), acc);
  for (octave_idx_type j = 0; j < m.columns (); j++)
    {
      for (octave_idx_type k = m.cidx (j); k < m.cidx (j+1); k++)
        acci[m.ridx (k)].accum (m.data (k));
    }

  for (octave_idx_type i = 0; i < m.rows (); i++)
    res.xelem (i) = acci[i];
}

// now the dispatchers
#define DEFINE_DISPATCHER(FUNC_NAME, ARG_TYPE, RES_TYPE) \
template <class T, class R> \
RES_TYPE FUNC_NAME (const ARG_TYPE& v, R p) \
{ \
  RES_TYPE res; \
  if (p == 2) \
    FUNC_NAME (v, res, norm_accumulator_2<R> ()); \
  else if (p == 1) \
    FUNC_NAME (v, res, norm_accumulator_1<R> ()); \
  else if (lo_ieee_isinf (p)) \
    { \
      if (p > 0) \
        FUNC_NAME (v, res, norm_accumulator_inf<R> ()); \
      else \
        FUNC_NAME (v, res, norm_accumulator_minf<R> ()); \
    } \
  else if (p == 0) \
    FUNC_NAME (v, res, norm_accumulator_0<R> ()); \
  else if (p > 0) \
    FUNC_NAME (v, res, norm_accumulator_p<R> (p)); \
  else \
    FUNC_NAME (v, res, norm_accumulator_mp<R> (p)); \
  return res; \
}

DEFINE_DISPATCHER (vector_norm, MArray<T>, R)
DEFINE_DISPATCHER (column_norms, MArray<T>, MArray<R>)
DEFINE_DISPATCHER (row_norms, MArray<T>, MArray<R>)
DEFINE_DISPATCHER (column_norms, MSparse<T>, MArray<R>)
DEFINE_DISPATCHER (row_norms, MSparse<T>, MArray<R>)

// The approximate subproblem in Higham's method. Find lambda and mu such that
// norm ([lambda, mu], p) == 1 and norm (y*lambda + col*mu, p) is maximized.
// Real version. As in Higham's paper.
template <class ColVectorT, class R>
static void
higham_subp (const ColVectorT& y, const ColVectorT& col,
             octave_idx_type nsamp, R p, R& lambda, R& mu)
{
  R nrm = 0;
  for (octave_idx_type i = 0; i < nsamp; i++)
    {
      octave_quit ();
      R fi = i * static_cast<R> (M_PI) / nsamp;
      R lambda1 = cos (fi);
      R mu1 = sin (fi);
      R lmnr = std::pow (std::pow (std::abs (lambda1), p) +
                         std::pow (std::abs (mu1), p), 1/p);
      lambda1 /= lmnr; mu1 /= lmnr;
      R nrm1 = vector_norm (lambda1 * y + mu1 * col, p);
      if (nrm1 > nrm)
        {
          lambda = lambda1;
          mu = mu1;
          nrm = nrm1;
        }
    }
}

// Complex version. Higham's paper does not deal with complex case, so we use a
// simple extension. First, guess the magnitudes as in real version, then try
// to rotate lambda to improve further.
template <class ColVectorT, class R>
static void
higham_subp (const ColVectorT& y, const ColVectorT& col,
             octave_idx_type nsamp, R p,
             std::complex<R>& lambda, std::complex<R>& mu)
{
  typedef std::complex<R> CR;
  R nrm = 0;
  lambda = 1.0;
  CR lamcu = lambda / std::abs (lambda);
  // Probe magnitudes
  for (octave_idx_type i = 0; i < nsamp; i++)
    {
      octave_quit ();
      R fi = i * static_cast<R> (M_PI) / nsamp;
      R lambda1 = cos (fi);
      R mu1 = sin (fi);
      R lmnr = std::pow (std::pow (std::abs (lambda1), p) +
                         std::pow (std::abs (mu1), p), 1/p);
      lambda1 /= lmnr; mu1 /= lmnr;
      R nrm1 = vector_norm (lambda1 * lamcu * y + mu1 * col, p);
      if (nrm1 > nrm)
        {
          lambda = lambda1 * lamcu;
          mu = mu1;
          nrm = nrm1;
        }
    }
  R lama = std::abs (lambda);
  // Probe orientation
  for (octave_idx_type i = 0; i < nsamp; i++)
    {
      octave_quit ();
      R fi = i * static_cast<R> (M_PI) / nsamp;
      lamcu = CR (cos (fi), sin (fi));
      R nrm1 = vector_norm (lama * lamcu * y + mu * col, p);
      if (nrm1 > nrm)
        {
          lambda = lama * lamcu;
          nrm = nrm1;
        }
    }
}

// the p-dual element (should work for both real and complex)
template <class T, class R>
inline T elem_dual_p (T x, R p)
{
  return signum (x) * std::pow (std::abs (x), p-1);
}

// the VectorT is used for vectors, but actually it has to be
// a Matrix type to allow all the operations. For instance SparseMatrix
// does not support multiplication with column/row vectors.
// the dual vector
template <class VectorT, class R>
VectorT dual_p (const VectorT& x, R p, R q)
{
  VectorT res (x.dims ());
  for (octave_idx_type i = 0; i < x.numel (); i++)
    res.xelem (i) = elem_dual_p (x(i), p);
  return res / vector_norm (res, q);
}

// Higham's hybrid method
template <class MatrixT, class VectorT, class R>
R higham (const MatrixT& m, R p, R tol, int maxiter,
          VectorT& x)
{
  x.resize (m.columns (), 1);
  // the OSE part
  VectorT y(m.rows (), 1, 0), z(m.rows (), 1);
  typedef typename VectorT::element_type RR;
  RR lambda = 0;
  RR mu = 1;
  for (octave_idx_type k = 0; k < m.columns (); k++)
    {
      octave_quit ();
      VectorT col (m.column (k));
      if (k > 0)
        higham_subp (y, col, 4*k, p, lambda, mu);
      for (octave_idx_type i = 0; i < k; i++)
        x(i) *= lambda;
      x(k) = mu;
      y = lambda * y + mu * col;
    }

  // the PM part
  x = x / vector_norm (x, p);
  R q = p/(p-1);

  R gamma = 0, gamma1;
  int iter = 0;
  while (iter < maxiter)
    {
      octave_quit ();
      y = m*x;
      gamma1 = gamma;
      gamma = vector_norm (y, p);
      z = dual_p (y, p, q);
      z = z.hermitian ();
      z = z * m;

      if (iter > 0 && (vector_norm (z, q) <= gamma
                       || (gamma - gamma1) <= tol*gamma))
        break;

      z = z.hermitian ();
      x = dual_p (z, q, p);
      iter ++;
    }

  return gamma;
}

// derive column vector and SVD types

static const char *p_less1_gripe = "xnorm: p must be at least 1";

// Static constant to control the maximum number of iterations.  100 seems to
// be a good value.  Eventually, we can provide a means to change this
// constant from Octave.
static int max_norm_iter = 100;

// version with SVD for dense matrices
template <class MatrixT, class VectorT, class SVDT, class R>
R matrix_norm (const MatrixT& m, R p, VectorT, SVDT)
{
  R res = 0;
  if (p == 2)
    {
      SVDT svd (m, SVD::sigma_only);
      res = svd.singular_values () (0,0);
    }
  else if (p == 1)
    res = xcolnorms (m, 1).max ();
  else if (lo_ieee_isinf (p))
    res = xrownorms (m, 1).max ();
  else if (p > 1)
    {
      VectorT x;
      const R sqrteps = std::sqrt (std::numeric_limits<R>::epsilon ());
      res = higham (m, p, sqrteps, max_norm_iter, x);
    }
  else
    (*current_liboctave_error_handler) (p_less1_gripe);

  return res;
}

// SVD-free version for sparse matrices
template <class MatrixT, class VectorT, class R>
R matrix_norm (const MatrixT& m, R p, VectorT)
{
  R res = 0;
  if (p == 1)
    res = xcolnorms (m, 1).max ();
  else if (lo_ieee_isinf (p))
    res = xrownorms (m, 1).max ();
  else if (p > 1)
    {
      VectorT x;
      const R sqrteps = std::sqrt (std::numeric_limits<R>::epsilon ());
      res = higham (m, p, sqrteps, max_norm_iter, x);
    }
  else
    (*current_liboctave_error_handler) (p_less1_gripe);

  return res;
}

// and finally, here's what we've promised in the header file

#define DEFINE_XNORM_FUNCS(PREFIX, RTYPE) \
  OCTAVE_API RTYPE xnorm (const PREFIX##ColumnVector& x, RTYPE p) \
  { return vector_norm (x, p); } \
  OCTAVE_API RTYPE xnorm (const PREFIX##RowVector& x, RTYPE p) \
  { return vector_norm (x, p); } \
  OCTAVE_API RTYPE xnorm (const PREFIX##Matrix& x, RTYPE p) \
  { return matrix_norm (x, p, PREFIX##Matrix (), PREFIX##SVD ()); } \
  OCTAVE_API RTYPE xfrobnorm (const PREFIX##Matrix& x) \
  { return vector_norm (x, static_cast<RTYPE> (2)); }

DEFINE_XNORM_FUNCS(, double)
DEFINE_XNORM_FUNCS(Complex, double)
DEFINE_XNORM_FUNCS(Float, float)
DEFINE_XNORM_FUNCS(FloatComplex, float)

// this is needed to avoid copying the sparse matrix for xfrobnorm
template <class T, class R>
inline void array_norm_2 (const T* v, octave_idx_type n, R& res)
{
  norm_accumulator_2<R> acc;
  for (octave_idx_type i = 0; i < n; i++)
    acc.accum (v[i]);

  res = acc;
}

#define DEFINE_XNORM_SPARSE_FUNCS(PREFIX, RTYPE) \
  OCTAVE_API RTYPE xnorm (const Sparse##PREFIX##Matrix& x, RTYPE p) \
  { return matrix_norm (x, p, PREFIX##Matrix ()); } \
  OCTAVE_API RTYPE xfrobnorm (const Sparse##PREFIX##Matrix& x) \
  { \
    RTYPE res; \
    array_norm_2 (x.data (), x.nnz (), res); \
    return res; \
  }

DEFINE_XNORM_SPARSE_FUNCS(, double)
DEFINE_XNORM_SPARSE_FUNCS(Complex, double)

#define DEFINE_COLROW_NORM_FUNCS(PREFIX, RPREFIX, RTYPE) \
  extern OCTAVE_API RPREFIX##RowVector xcolnorms (const PREFIX##Matrix& m, RTYPE p) \
  { return column_norms (m, p); } \
  extern OCTAVE_API RPREFIX##ColumnVector xrownorms (const PREFIX##Matrix& m, RTYPE p) \
  { return row_norms (m, p); } \

DEFINE_COLROW_NORM_FUNCS(, , double)
DEFINE_COLROW_NORM_FUNCS(Complex, , double)
DEFINE_COLROW_NORM_FUNCS(Float, Float, float)
DEFINE_COLROW_NORM_FUNCS(FloatComplex, Float, float)

DEFINE_COLROW_NORM_FUNCS(Sparse, , double)
DEFINE_COLROW_NORM_FUNCS(SparseComplex, , double)

