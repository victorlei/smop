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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include <cfloat>

#include "CollocWt.h"
#include "f77-fcn.h"
#include "lo-error.h"

// The following routines jcobi, dif, and dfopr are based on the code
// found in Villadsen, J. and M. L. Michelsen, Solution of Differential
// Equation Models by Polynomial Approximation, Prentice-Hall (1978)
// pages 418-420.
//
// Translated to C++ by jwe.

// Compute the first three derivatives of the node polynomial.
//
//                 n0     (alpha,beta)           n1
//   p  (x)  =  (x)   *  p (x)         *  (1 - x)
//    nt                   n
//
// at the interpolation points.  Each of the parameters n0 and n1
// may be given the value 0 or 1.  The total number of points
// nt = n + n0 + n1
//
// The values of root must be known before a call to dif is possible.
// They may be computed using jcobi.

static void
dif (octave_idx_type nt, double *root, double *dif1, double *dif2,
     double *dif3)
{
  // Evaluate derivatives of node polynomial using recursion formulas.

  for (octave_idx_type i = 0; i < nt; i++)
    {
      double x = root[i];

      dif1[i] = 1.0;
      dif2[i] = 0.0;
      dif3[i] = 0.0;

      for (octave_idx_type j = 0; j < nt; j++)
        {
          if (j != i)
            {
              double y = x - root[j];

              dif3[i] = y * dif3[i] + 3.0 * dif2[i];
              dif2[i] = y * dif2[i] + 2.0 * dif1[i];
              dif1[i] = y * dif1[i];
            }
        }
    }
}

// Compute the zeros of the Jacobi polynomial.
//
//    (alpha,beta)
//   p  (x)
//    n
//
// Use dif to compute the derivatives of the node
// polynomial
//
//                 n0     (alpha,beta)           n1
//   p  (x)  =  (x)   *  p (x)         *  (1 - x)
//    nt                   n
//
// at the interpolation points.
//
// See Villadsen and Michelsen, pages 131-132 and 418.
//
// Input parameters:
//
//   nd     : the dimension of the vectors dif1, dif2, dif3, and root
//
//   n      : the degree of the jacobi polynomial, (i.e. the number
//            of interior interpolation points)
//
//   n0     : determines whether x = 0 is included as an
//            interpolation point
//
//              n0 = 0  ==>  x = 0 is not included
//              n0 = 1  ==>  x = 0 is included
//
//   n1     : determines whether x = 1 is included as an
//            interpolation point
//
//              n1 = 0  ==>  x = 1 is not included
//              n1 = 1  ==>  x = 1 is included
//
//   alpha  : the value of alpha in the description of the jacobi
//            polynomial
//
//   beta   : the value of beta in the description of the jacobi
//            polynomial
//
//   For a more complete explanation of alpha an beta, see Villadsen
//   and Michelsen, pages 57 to 59.
//
// Output parameters:
//
//   root   : one dimensional vector containing on exit the
//            n + n0 + n1 zeros of the node polynomial used in the
//            interpolation routine
//
//   dif1   : one dimensional vector containing the first derivative
//            of the node polynomial at the zeros
//
//   dif2   : one dimensional vector containing the second derivative
//            of the node polynomial at the zeros
//
//   dif3   : one dimensional vector containing the third derivative
//            of the node polynomial at the zeros

static bool
jcobi (octave_idx_type n, octave_idx_type n0, octave_idx_type n1,
       double alpha, double beta, double *dif1, double *dif2,
       double *dif3, double *root)
{
  assert (n0 == 0 || n0 == 1);
  assert (n1 == 0 || n1 == 1);

  octave_idx_type nt = n + n0 + n1;

  assert (nt > 1);

// -- first evaluation of coefficients in recursion formulas.
// -- recursion coefficients are stored in dif1 and dif2.

  double ab = alpha + beta;
  double ad = beta - alpha;
  double ap = beta * alpha;

  dif1[0] = (ad / (ab + 2.0) + 1.0) / 2.0;
  dif2[0] = 0.0;

  if (n >= 2)
    {
      for (octave_idx_type i = 1; i < n; i++)
        {
          double z1 = i;
          double z = ab + 2 * z1;

          dif1[i] = (ab * ad / z / (z + 2.0) + 1.0) / 2.0;

          if (i == 1)
            dif2[i] = (ab + ap + z1) / z / z / (z + 1.0);
          else
            {
              z = z * z;
              double y = z1 * (ab + z1);
              y = y * (ap + y);
              dif2[i] = y / z / (z - 1.0);
            }
        }
    }

  // Root determination by Newton method with suppression of previously
  // determined roots.

  double x = 0.0;

  for (octave_idx_type i = 0; i < n; i++)
    {
      bool done = false;

      int k = 0;

      while (! done)
        {
          double xd = 0.0;
          double xn = 1.0;
          double xd1 = 0.0;
          double xn1 = 0.0;

          for (octave_idx_type j = 0; j < n; j++)
            {
              double xp  = (dif1[j] - x) * xn  - dif2[j] * xd;
              double xp1 = (dif1[j] - x) * xn1 - dif2[j] * xd1 - xn;

              xd  = xn;
              xd1 = xn1;
              xn  = xp;
              xn1 = xp1;
            }

          double zc = 1.0;
          double z = xn / xn1;

          if (i != 0)
            {
              for (octave_idx_type j = 1; j <= i; j++)
                zc = zc - z / (x - root[j-1]);
            }

          z = z / zc;
          x = x - z;

          // Famous last words:  100 iterations should be more than
          // enough in all cases.

          if (++k > 100 || xisnan (z))
            return false;

          if (std::abs (z) <= 100 * std::numeric_limits<double>::epsilon ())
            done = true;
        }

      root[i] = x;
      x = x + sqrt (std::numeric_limits<double>::epsilon ());
    }

  // Add interpolation points at x = 0 and/or x = 1.

  if (n0 != 0)
    {
      for (octave_idx_type i = n; i > 0; i--)
        root[i] = root[i-1];

      root[0] = 0.0;
    }

  if (n1 != 0)
    root[nt-1] = 1.0;

  dif (nt, root, dif1, dif2, dif3);

  return true;
}

// Compute derivative weights for orthogonal collocation.
//
// See Villadsen and Michelsen, pages 133-134, 419.
//
// Input parameters:
//
//   nd     : the dimension of the vectors dif1, dif2, dif3, and root
//
//   n      : the degree of the jacobi polynomial, (i.e. the number
//            of interior interpolation points)
//
//   n0     : determines whether x = 0 is included as an
//            interpolation point
//
//              n0 = 0  ==>  x = 0 is not included
//              n0 = 1  ==>  x = 0 is included
//
//   n1     : determines whether x = 1 is included as an
//            interpolation point
//
//              n1 = 0  ==>  x = 1 is not included
//              n1 = 1  ==>  x = 1 is included
//
//   i      : the index of the node for which the weights are to be
//            calculated
//
//   id     : indicator
//
//              id = 1  ==>  first derivative weights are computed
//              id = 2  ==>  second derivative weights are computed
//              id = 3  ==>  gaussian weights are computed (in this
//                           case, the value of i is irrelevant)
//
// Output parameters:
//
//   dif1   : one dimensional vector containing the first derivative
//            of the node polynomial at the zeros
//
//   dif2   : one dimensional vector containing the second derivative
//            of the node polynomial at the zeros
//
//   dif3   : one dimensional vector containing the third derivative
//            of the node polynomial at the zeros
//
//   vect   : one dimensional vector of computed weights

static void
dfopr (octave_idx_type n, octave_idx_type n0, octave_idx_type n1,
       octave_idx_type i, octave_idx_type id, double *dif1,
       double *dif2, double *dif3, double *root, double *vect)
{
  assert (n0 == 0 || n0 == 1);
  assert (n1 == 0 || n1 == 1);

  octave_idx_type nt = n + n0 + n1;

  assert (nt > 1);

  assert (id == 1 || id == 2 || id == 3);

  if (id != 3)
    assert (i >= 0 && i < nt);

  // Evaluate discretization matrices and Gaussian quadrature weights.
  // Quadrature weights are normalized to sum to one.

  if (id != 3)
    {
      for (octave_idx_type j = 0; j < nt; j++)
        {
          if (j == i)
            {
              if (id == 1)
                vect[i] = dif2[i] / dif1[i] / 2.0;
              else
                vect[i] = dif3[i] / dif1[i] / 3.0;
            }
          else
            {
              double y = root[i] - root[j];

              vect[j] = dif1[i] / dif1[j] / y;

              if (id == 2)
                vect[j] = vect[j] * (dif2[i] / dif1[i] - 2.0 / y);
            }
        }
    }
  else
    {
      double y = 0.0;

      for (octave_idx_type j = 0; j < nt; j++)
        {
          double x  = root[j];

          double ax = x * (1.0 - x);

          if (n0 == 0)
            ax = ax / x / x;

          if (n1 == 0)
            ax = ax / (1.0 - x) / (1.0 - x);

          vect[j] = ax / (dif1[j] * dif1[j]);

          y = y + vect[j];
        }

      for (octave_idx_type j = 0; j < nt; j++)
        vect[j] = vect[j] / y;
    }
}

// Error handling.

void
CollocWt::error (const char* msg)
{
  (*current_liboctave_error_handler) ("fatal CollocWt error: %s", msg);
}

CollocWt&
CollocWt::set_left (double val)
{
  if (val >= rb)
    {
      error ("left bound greater than right bound");
      return *this;
    }

  lb = val;
  initialized = 0;
  return *this;
}

CollocWt&
CollocWt::set_right (double val)
{
  if (val <= lb)
    {
      error ("right bound less than left bound");
      return *this;
    }

  rb = val;
  initialized = 0;
  return *this;
}

void
CollocWt::init (void)
{
  // Check for possible errors.

  double wid = rb - lb;
  if (wid <= 0.0)
    {
      error ("width less than or equal to zero");
      return;
    }

  octave_idx_type nt = n + inc_left + inc_right;

  if (nt < 0)
    {
      error ("total number of collocation points less than zero");
      return;
    }
  else if (nt == 0)
    return;

  Array<double> dif1 (dim_vector (nt, 1));
  double *pdif1 = dif1.fortran_vec ();

  Array<double> dif2 (dim_vector (nt, 1));
  double *pdif2 = dif2.fortran_vec ();

  Array<double> dif3 (dim_vector (nt, 1));
  double *pdif3 = dif3.fortran_vec ();

  Array<double> vect (dim_vector (nt, 1));
  double *pvect = vect.fortran_vec ();

  r.resize (nt, 1);
  q.resize (nt, 1);
  A.resize (nt, nt);
  B.resize (nt, nt);

  double *pr = r.fortran_vec ();

  // Compute roots.

  if (! jcobi (n, inc_left, inc_right, Alpha, Beta, pdif1, pdif2, pdif3, pr))
    {
      error ("jcobi: newton iteration failed");
      return;
    }

  octave_idx_type id;

  // First derivative weights.

  id = 1;
  for (octave_idx_type i = 0; i < nt; i++)
    {
      dfopr (n, inc_left, inc_right, i, id, pdif1, pdif2, pdif3, pr, pvect);

      for (octave_idx_type j = 0; j < nt; j++)
        A(i,j) = vect(j);
    }

  // Second derivative weights.

  id = 2;
  for (octave_idx_type i = 0; i < nt; i++)
    {
      dfopr (n, inc_left, inc_right, i, id, pdif1, pdif2, pdif3, pr, pvect);

      for (octave_idx_type j = 0; j < nt; j++)
        B(i,j) = vect(j);
    }

  // Gaussian quadrature weights.

  id = 3;
  double *pq = q.fortran_vec ();
  dfopr (n, inc_left, inc_right, id, id, pdif1, pdif2, pdif3, pr, pq);

  initialized = 1;
}

std::ostream&
operator << (std::ostream& os, const CollocWt& a)
{
  if (a.left_included ())
    os << "left  boundary is included\n";
  else
    os << "left  boundary is not included\n";

  if (a.right_included ())
    os << "right boundary is included\n";
  else
    os << "right boundary is not included\n";

  os << "\n";

  os << a.Alpha << " " << a.Beta << "\n\n"
     << a.r << "\n\n"
     << a.q << "\n\n"
     << a.A << "\n"
     << a.B << "\n";

  return os;
}
