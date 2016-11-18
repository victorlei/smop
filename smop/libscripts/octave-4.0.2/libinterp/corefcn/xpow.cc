/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

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

#include <cassert>

#include <limits>

#include "Array-util.h"
#include "CColVector.h"
#include "CDiagMatrix.h"
#include "fCDiagMatrix.h"
#include "fCMatrix.h"
#include "CMatrix.h"
#include "EIG.h"
#include "fEIG.h"
#include "dDiagMatrix.h"
#include "fDiagMatrix.h"
#include "dMatrix.h"
#include "PermMatrix.h"
#include "mx-cm-cdm.h"
#include "mx-fcm-fcdm.h"
#include "oct-cmplx.h"
#include "Range.h"
#include "quit.h"

#include "error.h"
#include "oct-obj.h"
#include "utils.h"
#include "xpow.h"

#include "bsxfun.h"

static inline int
xisint (double x)
{
  return (D_NINT (x) == x
          && ((x >= 0 && x < std::numeric_limits<int>::max ())
              || (x <= 0 && x > std::numeric_limits<int>::min ())));
}

// Safer pow functions.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | 1 | 5 |  7 | 11 |
//                  +---+---+----+----+
//   matrix         | 2 | * |  8 |  * |
//                  +---+---+----+----+
//   complex_scalar | 3 | 6 |  9 | 12 |
//                  +---+---+----+----+
//   complex_matrix | 4 | * | 10 |  * |
//                  +---+---+----+----+

// -*- 1 -*-
octave_value
xpow (double a, double b)
{
  double retval;

  if (a < 0.0 && ! xisint (b))
    {
      Complex atmp (a);

      return std::pow (atmp, b);
    }
  else
    retval = std::pow (a, b);

  return retval;
}

// -*- 2 -*-
octave_value
xpow (double a, const Matrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for x^A, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      EIG b_eig (b);

      if (! error_state)
        {
          ComplexColumnVector lambda (b_eig.eigenvalues ());
          ComplexMatrix Q (b_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            {
              Complex elt = lambda(i);
              if (std::imag (elt) == 0.0)
                lambda(i) = std::pow (a, std::real (elt));
              else
                lambda(i) = std::pow (a, elt);
            }
          ComplexDiagMatrix D (lambda);

          ComplexMatrix C = Q * D * Q.inverse ();
          if (a > 0)
            retval = real (C);
          else
            retval = C;
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 3 -*-
octave_value
xpow (double a, const Complex& b)
{
  Complex result = std::pow (a, b);
  return result;
}

// -*- 4 -*-
octave_value
xpow (double a, const ComplexMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for x^A, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      EIG b_eig (b);

      if (! error_state)
        {
          ComplexColumnVector lambda (b_eig.eigenvalues ());
          ComplexMatrix Q (b_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            {
              Complex elt = lambda(i);
              if (std::imag (elt) == 0.0)
                lambda(i) = std::pow (a, std::real (elt));
              else
                lambda(i) = std::pow (a, elt);
            }
          ComplexDiagMatrix D (lambda);

          retval = ComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 5 -*-
octave_value
xpow (const Matrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      if (static_cast<int> (b) == b)
        {
          int btmp = static_cast<int> (b);
          if (btmp == 0)
            {
              retval = DiagMatrix (nr, nr, 1.0);
            }
          else
            {
              // Too much copying?
              // FIXME: we shouldn't do this if the exponent is large...

              Matrix atmp;
              if (btmp < 0)
                {
                  btmp = -btmp;

                  octave_idx_type info;
                  double rcond = 0.0;
                  MatrixType mattype (a);

                  atmp = a.inverse (mattype, info, rcond, 1);

                  if (info == -1)
                    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
                }
              else
                atmp = a;

              Matrix result (atmp);

              btmp--;

              while (btmp > 0)
                {
                  if (btmp & 1)
                    result = result * atmp;

                  btmp >>= 1;

                  if (btmp > 0)
                    atmp = atmp * atmp;
                }

              retval = result;
            }
        }
      else
        {
          EIG a_eig (a);

          if (! error_state)
            {
              ComplexColumnVector lambda (a_eig.eigenvalues ());
              ComplexMatrix Q (a_eig.eigenvectors ());

              for (octave_idx_type i = 0; i < nr; i++)
                lambda(i) = std::pow (lambda(i), b);

              ComplexDiagMatrix D (lambda);

              retval = ComplexMatrix (Q * D * Q.inverse ());
            }
          else
            error ("xpow: matrix diagonalization failed");
        }
    }

  return retval;
}

// -*- 5d -*-
octave_value
xpow (const DiagMatrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      if (static_cast<int> (b) == b)
        {
          DiagMatrix r (nr, nc);
          for (octave_idx_type i = 0; i < nc; i++)
            r.dgelem (i) = std::pow (a.dgelem (i), b);
          retval = r;
        }
      else
        {
          ComplexDiagMatrix r (nr, nc);
          for (octave_idx_type i = 0; i < nc; i++)
            r.dgelem (i) = std::pow (static_cast<Complex> (a.dgelem (i)), b);
          retval = r;
        }
    }

  return retval;
}

// -*- 5p -*-
octave_value
xpow (const PermMatrix& a, double b)
{
  octave_value retval;
  int btmp = static_cast<int> (b);
  if (btmp == b)
    return a.power (btmp);
  else
    return xpow (Matrix (a), b);
}

// -*- 6 -*-
octave_value
xpow (const Matrix& a, const Complex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      EIG a_eig (a);

      if (! error_state)
        {
          ComplexColumnVector lambda (a_eig.eigenvalues ());
          ComplexMatrix Q (a_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            lambda(i) = std::pow (lambda(i), b);

          ComplexDiagMatrix D (lambda);

          retval = ComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 7 -*-
octave_value
xpow (const Complex& a, double b)
{
  Complex result;

  if (xisint (b))
    result = std::pow (a, static_cast<int> (b));
  else
    result = std::pow (a, b);

  return result;
}

// -*- 8 -*-
octave_value
xpow (const Complex& a, const Matrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for x^A, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      EIG b_eig (b);

      if (! error_state)
        {
          ComplexColumnVector lambda (b_eig.eigenvalues ());
          ComplexMatrix Q (b_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            {
              Complex elt = lambda(i);
              if (std::imag (elt) == 0.0)
                lambda(i) = std::pow (a, std::real (elt));
              else
                lambda(i) = std::pow (a, elt);
            }
          ComplexDiagMatrix D (lambda);

          retval = ComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 9 -*-
octave_value
xpow (const Complex& a, const Complex& b)
{
  Complex result;
  result = std::pow (a, b);
  return result;
}

// -*- 10 -*-
octave_value
xpow (const Complex& a, const ComplexMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for x^A, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      EIG b_eig (b);

      if (! error_state)
        {
          ComplexColumnVector lambda (b_eig.eigenvalues ());
          ComplexMatrix Q (b_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            {
              Complex elt = lambda(i);
              if (std::imag (elt) == 0.0)
                lambda(i) = std::pow (a, std::real (elt));
              else
                lambda(i) = std::pow (a, elt);
            }
          ComplexDiagMatrix D (lambda);

          retval = ComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 11 -*-
octave_value
xpow (const ComplexMatrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      if (static_cast<int> (b) == b)
        {
          int btmp = static_cast<int> (b);
          if (btmp == 0)
            {
              retval = DiagMatrix (nr, nr, 1.0);
            }
          else
            {
              // Too much copying?
              // FIXME: we shouldn't do this if the exponent is large...

              ComplexMatrix atmp;
              if (btmp < 0)
                {
                  btmp = -btmp;

                  octave_idx_type info;
                  double rcond = 0.0;
                  MatrixType mattype (a);

                  atmp = a.inverse (mattype, info, rcond, 1);

                  if (info == -1)
                    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
                }
              else
                atmp = a;

              ComplexMatrix result (atmp);

              btmp--;

              while (btmp > 0)
                {
                  if (btmp & 1)
                    result = result * atmp;

                  btmp >>= 1;

                  if (btmp > 0)
                    atmp = atmp * atmp;
                }

              retval = result;
            }
        }
      else
        {
          EIG a_eig (a);

          if (! error_state)
            {
              ComplexColumnVector lambda (a_eig.eigenvalues ());
              ComplexMatrix Q (a_eig.eigenvectors ());

              for (octave_idx_type i = 0; i < nr; i++)
                lambda(i) = std::pow (lambda(i), b);

              ComplexDiagMatrix D (lambda);

              retval = ComplexMatrix (Q * D * Q.inverse ());
            }
          else
            error ("xpow: matrix diagonalization failed");
        }
    }

  return retval;
}

// -*- 12 -*-
octave_value
xpow (const ComplexMatrix& a, const Complex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      EIG a_eig (a);

      if (! error_state)
        {
          ComplexColumnVector lambda (a_eig.eigenvalues ());
          ComplexMatrix Q (a_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            lambda(i) = std::pow (lambda(i), b);

          ComplexDiagMatrix D (lambda);

          retval = ComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 12d -*-
octave_value
xpow (const ComplexDiagMatrix& a, const Complex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      ComplexDiagMatrix r (nr, nc);
      for (octave_idx_type i = 0; i < nc; i++)
        r(i, i) = std::pow (a(i, i), b);
      retval = r;
    }

  return retval;
}

// mixed
octave_value
xpow (const ComplexDiagMatrix& a, double b)
{
  return xpow (a, static_cast<Complex> (b));
}

octave_value
xpow (const DiagMatrix& a, const Complex& b)
{
  return xpow (ComplexDiagMatrix (a), b);
}


// Safer pow functions that work elementwise for matrices.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   matrix         | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_matrix | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// FIXME: these functions need to be fixed so that things like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// -*- 1 -*-
octave_value
elem_xpow (double a, const Matrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  double d1, d2;

  if (a < 0.0 && ! b.all_integers (d1, d2))
    {
      Complex atmp (a);
      ComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (atmp, b (i, j));
          }

      retval = result;
    }
  else
    {
      Matrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a, b (i, j));
          }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (double a, const ComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);
  Complex atmp (a);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (atmp, b (i, j));
      }

  return result;
}

static inline bool
same_sign (double a, double b)
{
  return (a >= 0 && b >= 0) || (a <= 0 && b <= 0);
}

octave_value
elem_xpow (double a, const Range& r)
{
  octave_value retval;

  // Only optimize powers with ranges that are integer and monotonic in
  // magnitude.
  if (r.nelem () > 1 && r.all_elements_are_ints ()
      && same_sign (r.base (), r.limit ()))
    {
      octave_idx_type n = r.nelem ();
      Matrix result (1, n);
      if (same_sign (r.base (), r.inc ()))
        {
          double base = std::pow (a, r.base ());
          double inc = std::pow (a, r.inc ());
          result(0) = base;
          for (octave_idx_type i = 1; i < n; i++)
            result(i) = (base *= inc);
        }
      else
        {
          // Don't use Range::limit () here.
          double limit = std::pow (a, r.base () + (n-1) * r.inc ());
          double inc = std::pow (a, -r.inc ());
          result(n-1) = limit;
          for (octave_idx_type i = n-2; i >= 0; i--)
            result(i) = (limit *= inc);
        }

      retval = result;
    }
  else
    retval = elem_xpow (a, r.matrix_value ());

  return retval;
}

// -*- 3 -*-
octave_value
elem_xpow (const Matrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (! xisint (b) && a.any_element_is_negative ())
    {
      ComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();

            Complex atmp (a (i, j));

            result (i, j) = std::pow (atmp, b);
          }

      retval = result;
    }
  else
    {
      Matrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a (i, j), b);
          }

      retval = result;
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const Matrix& a, const Matrix& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  int convert_to_complex = 0;
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        double atmp = a (i, j);
        double btmp = b (i, j);
        if (atmp < 0.0 && static_cast<int> (btmp) != btmp)
          {
            convert_to_complex = 1;
            goto done;
          }
      }

done:

  if (convert_to_complex)
    {
      ComplexMatrix complex_result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            Complex atmp (a (i, j));
            Complex btmp (b (i, j));
            complex_result (i, j) = std::pow (atmp, btmp);
          }

      retval = complex_result;
    }
  else
    {
      Matrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a (i, j), b (i, j));
          }

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const Matrix& a, const Complex& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (Complex (a (i, j)), b);
      }

  return result;
}

// -*- 6 -*-
octave_value
elem_xpow (const Matrix& a, const ComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (Complex (a (i, j)), b (i, j));
      }

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const Complex& a, const Matrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        double btmp = b (i, j);
        if (xisint (btmp))
          result (i, j) = std::pow (a, static_cast<int> (btmp));
        else
          result (i, j) = std::pow (a, btmp);
      }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const Complex& a, const ComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (a, b (i, j));
      }

  return result;
}

octave_value
elem_xpow (const Complex& a, const Range& r)
{
  octave_value retval;

  // Only optimize powers with ranges that are integer and monotonic in
  // magnitude.
  if (r.nelem () > 1 && r.all_elements_are_ints ()
      && same_sign (r.base (), r.limit ()))
    {
      octave_idx_type n = r.nelem ();
      ComplexMatrix result (1, n);

      if (same_sign (r.base (), r.inc ()))
        {
          Complex base = std::pow (a, r.base ());
          Complex inc = std::pow (a, r.inc ());
          result(0) = base;
          for (octave_idx_type i = 1; i < n; i++)
            result(i) = (base *= inc);
        }
      else
        {
          // Don't use Range::limit () here.
          Complex limit = std::pow (a, r.base () + (n-1) * r.inc ());
          Complex inc = std::pow (a, -r.inc ());
          result(n-1) = limit;
          for (octave_idx_type i = n-2; i >= 0; i--)
            result(i) = (limit *= inc);
        }

      retval = result;
    }
  else
    retval = elem_xpow (a, r.matrix_value ());


  return retval;
}

// -*- 9 -*-
octave_value
elem_xpow (const ComplexMatrix& a, double b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  ComplexMatrix result (nr, nc);

  if (xisint (b))
    {
      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a (i, j), static_cast<int> (b));
          }
    }
  else
    {
      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a (i, j), b);
          }
    }

  return result;
}

// -*- 10 -*-
octave_value
elem_xpow (const ComplexMatrix& a, const Matrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        double btmp = b (i, j);
        if (xisint (btmp))
          result (i, j) = std::pow (a (i, j), static_cast<int> (btmp));
        else
          result (i, j) = std::pow (a (i, j), btmp);
      }

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const ComplexMatrix& a, const Complex& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (a (i, j), b);
      }

  return result;
}

// -*- 12 -*-
octave_value
elem_xpow (const ComplexMatrix& a, const ComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (a (i, j), b (i, j));
      }

  return result;
}

// Safer pow functions that work elementwise for N-d arrays.
//
//       op2 \ op1:   s   nd  cs   cnd
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   N_d            | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_N_d    | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// FIXME: these functions need to be fixed so that things like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// -*- 1 -*-
octave_value
elem_xpow (double a, const NDArray& b)
{
  octave_value retval;

  if (a < 0.0 && ! b.all_integers ())
    {
      Complex atmp (a);
      ComplexNDArray result (b.dims ());
      for (octave_idx_type i = 0; i < b.length (); i++)
        {
          octave_quit ();
          result(i) = std::pow (atmp, b(i));
        }

      retval = result;
    }
  else
    {
      NDArray result (b.dims ());
      for (octave_idx_type i = 0; i < b.length (); i++)
        {
          octave_quit ();
          result (i) = std::pow (a, b(i));
        }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (double a, const ComplexNDArray& b)
{
  ComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a, b(i));
    }

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const NDArray& a, double b)
{
  octave_value retval;

  if (! xisint (b))
    {
      if (a.any_element_is_negative ())
        {
          ComplexNDArray result (a.dims ());

          for (octave_idx_type i = 0; i < a.length (); i++)
            {
              octave_quit ();

              Complex atmp (a (i));

              result(i) = std::pow (atmp, b);
            }

          retval = result;
        }
      else
        {
          NDArray result (a.dims ());
          for (octave_idx_type i = 0; i < a.length (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), b);
            }

          retval = result;
        }
    }
  else
    {
      NoAlias<NDArray> result (a.dims ());

      int ib = static_cast<int> (b);
      if (ib == 2)
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            result(i) = a(i) * a(i);
        }
      else if (ib == 3)
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            result(i) = a(i) * a(i) * a(i);
        }
      else if (ib == -1)
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            result(i) = 1.0 / a(i);
        }
      else
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), ib);
            }
        }

      retval = result;
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const NDArray& a, const NDArray& b)
{
  octave_value retval;

  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (is_valid_bsxfun ("operator .^", a_dims, b_dims))
        {
          //Potentially complex results
          NDArray xa = octave_value_extract<NDArray> (a);
          NDArray xb = octave_value_extract<NDArray> (b);
          if (! xb.all_integers () && xa.any_element_is_negative ())
            return octave_value (bsxfun_pow (ComplexNDArray (xa), xb));
          else
            return octave_value (bsxfun_pow (xa, xb));
        }
      else
        {
          gripe_nonconformant ("operator .^", a_dims, b_dims);
          return octave_value ();
        }
    }

  int len = a.length ();

  bool convert_to_complex = false;

  for (octave_idx_type i = 0; i < len; i++)
    {
      octave_quit ();
      double atmp = a(i);
      double btmp = b(i);
      if (atmp < 0.0 && static_cast<int> (btmp) != btmp)
        {
          convert_to_complex = true;
          goto done;
        }
    }

done:

  if (convert_to_complex)
    {
      ComplexNDArray complex_result (a_dims);

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_quit ();
          Complex atmp (a(i));
          complex_result(i) = std::pow (atmp, b(i));
        }

      retval = complex_result;
    }
  else
    {
      NDArray result (a_dims);

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_quit ();
          result(i) = std::pow (a(i), b(i));
        }

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const NDArray& a, const Complex& b)
{
  ComplexNDArray result (a.dims ());

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b);
    }

  return result;
}

// -*- 6 -*-
octave_value
elem_xpow (const NDArray& a, const ComplexNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (is_valid_bsxfun ("operator .^", a_dims, b_dims))
        {
          return bsxfun_pow (a, b);
        }
      else
        {
          gripe_nonconformant ("operator .^", a_dims, b_dims);
          return octave_value ();
        }
    }

  ComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b(i));
    }

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const Complex& a, const NDArray& b)
{
  ComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      octave_quit ();
      double btmp = b(i);
      if (xisint (btmp))
        result(i) = std::pow (a, static_cast<int> (btmp));
      else
        result(i) = std::pow (a, btmp);
    }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const Complex& a, const ComplexNDArray& b)
{
  ComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a, b(i));
    }

  return result;
}

// -*- 9 -*-
octave_value
elem_xpow (const ComplexNDArray& a, double b)
{
  ComplexNDArray result (a.dims ());

  if (xisint (b))
    {
      if (b == -1)
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            result.xelem (i) = 1.0 / a(i);
        }
      else
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), static_cast<int> (b));
            }
        }
    }
  else
    {
      for (octave_idx_type i = 0; i < a.length (); i++)
        {
          octave_quit ();
          result(i) = std::pow (a(i), b);
        }
    }

  return result;
}

// -*- 10 -*-
octave_value
elem_xpow (const ComplexNDArray& a, const NDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (is_valid_bsxfun ("operator .^", a_dims, b_dims))
        {
          return bsxfun_pow (a, b);
        }
      else
        {
          gripe_nonconformant ("operator .^", a_dims, b_dims);
          return octave_value ();
        }
    }

  ComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      double btmp = b(i);
      if (xisint (btmp))
        result(i) = std::pow (a(i), static_cast<int> (btmp));
      else
        result(i) = std::pow (a(i), btmp);
    }

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const ComplexNDArray& a, const Complex& b)
{
  ComplexNDArray result (a.dims ());

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b);
    }

  return result;
}

// -*- 12 -*-
octave_value
elem_xpow (const ComplexNDArray& a, const ComplexNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (is_valid_bsxfun ("operator .^", a_dims, b_dims))
        {
          return bsxfun_pow (a, b);
        }
      else
        {
          gripe_nonconformant ("operator .^", a_dims, b_dims);
          return octave_value ();
        }
    }

  ComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b(i));
    }

  return result;
}

static inline int
xisint (float x)
{
  return (D_NINT (x) == x
          && ((x >= 0 && x < std::numeric_limits<int>::max ())
              || (x <= 0 && x > std::numeric_limits<int>::min ())));
}

// Safer pow functions.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | 1 | 5 |  7 | 11 |
//                  +---+---+----+----+
//   matrix         | 2 | * |  8 |  * |
//                  +---+---+----+----+
//   complex_scalar | 3 | 6 |  9 | 12 |
//                  +---+---+----+----+
//   complex_matrix | 4 | * | 10 |  * |
//                  +---+---+----+----+

// -*- 1 -*-
octave_value
xpow (float a, float b)
{
  float retval;

  if (a < 0.0 && ! xisint (b))
    {
      FloatComplex atmp (a);

      return std::pow (atmp, b);
    }
  else
    retval = std::pow (a, b);

  return retval;
}

// -*- 2 -*-
octave_value
xpow (float a, const FloatMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for x^A, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      FloatEIG b_eig (b);

      if (! error_state)
        {
          FloatComplexColumnVector lambda (b_eig.eigenvalues ());
          FloatComplexMatrix Q (b_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            {
              FloatComplex elt = lambda(i);
              if (std::imag (elt) == 0.0)
                lambda(i) = std::pow (a, std::real (elt));
              else
                lambda(i) = std::pow (a, elt);
            }
          FloatComplexDiagMatrix D (lambda);

          FloatComplexMatrix C = Q * D * Q.inverse ();

          if (a > 0)
            retval = real (C);
          else
            retval = C;
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 3 -*-
octave_value
xpow (float a, const FloatComplex& b)
{
  FloatComplex result = std::pow (a, b);
  return result;
}

// -*- 4 -*-
octave_value
xpow (float a, const FloatComplexMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for x^A, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      FloatEIG b_eig (b);

      if (! error_state)
        {
          FloatComplexColumnVector lambda (b_eig.eigenvalues ());
          FloatComplexMatrix Q (b_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            {
              FloatComplex elt = lambda(i);
              if (std::imag (elt) == 0.0)
                lambda(i) = std::pow (a, std::real (elt));
              else
                lambda(i) = std::pow (a, elt);
            }
          FloatComplexDiagMatrix D (lambda);

          retval = FloatComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 5 -*-
octave_value
xpow (const FloatMatrix& a, float b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      if (static_cast<int> (b) == b)
        {
          int btmp = static_cast<int> (b);
          if (btmp == 0)
            {
              retval = FloatDiagMatrix (nr, nr, 1.0);
            }
          else
            {
              // Too much copying?
              // FIXME: we shouldn't do this if the exponent is large...

              FloatMatrix atmp;
              if (btmp < 0)
                {
                  btmp = -btmp;

                  octave_idx_type info;
                  float rcond = 0.0;
                  MatrixType mattype (a);

                  atmp = a.inverse (mattype, info, rcond, 1);

                  if (info == -1)
                    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
                }
              else
                atmp = a;

              FloatMatrix result (atmp);

              btmp--;

              while (btmp > 0)
                {
                  if (btmp & 1)
                    result = result * atmp;

                  btmp >>= 1;

                  if (btmp > 0)
                    atmp = atmp * atmp;
                }

              retval = result;
            }
        }
      else
        {
          FloatEIG a_eig (a);

          if (! error_state)
            {
              FloatComplexColumnVector lambda (a_eig.eigenvalues ());
              FloatComplexMatrix Q (a_eig.eigenvectors ());

              for (octave_idx_type i = 0; i < nr; i++)
                lambda(i) = std::pow (lambda(i), b);

              FloatComplexDiagMatrix D (lambda);

              retval = FloatComplexMatrix (Q * D * Q.inverse ());
            }
          else
            error ("xpow: matrix diagonalization failed");
        }
    }

  return retval;
}

// -*- 5d -*-
octave_value
xpow (const FloatDiagMatrix& a, float b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      if (static_cast<int> (b) == b)
        {
          FloatDiagMatrix r (nr, nc);
          for (octave_idx_type i = 0; i < nc; i++)
            r.dgelem (i) = std::pow (a.dgelem (i), b);
          retval = r;
        }
      else
        {
          FloatComplexDiagMatrix r (nr, nc);
          for (octave_idx_type i = 0; i < nc; i++)
            r.dgelem (i) = std::pow (static_cast<FloatComplex> (a.dgelem (i)),
                                                                b);
          retval = r;
        }
    }

  return retval;
}

// -*- 6 -*-
octave_value
xpow (const FloatMatrix& a, const FloatComplex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      FloatEIG a_eig (a);

      if (! error_state)
        {
          FloatComplexColumnVector lambda (a_eig.eigenvalues ());
          FloatComplexMatrix Q (a_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            lambda(i) = std::pow (lambda(i), b);

          FloatComplexDiagMatrix D (lambda);

          retval = FloatComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 7 -*-
octave_value
xpow (const FloatComplex& a, float b)
{
  FloatComplex result;

  if (xisint (b))
    result = std::pow (a, static_cast<int> (b));
  else
    result = std::pow (a, b);

  return result;
}

// -*- 8 -*-
octave_value
xpow (const FloatComplex& a, const FloatMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for x^A, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      FloatEIG b_eig (b);

      if (! error_state)
        {
          FloatComplexColumnVector lambda (b_eig.eigenvalues ());
          FloatComplexMatrix Q (b_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            {
              FloatComplex elt = lambda(i);
              if (std::imag (elt) == 0.0)
                lambda(i) = std::pow (a, std::real (elt));
              else
                lambda(i) = std::pow (a, elt);
            }
          FloatComplexDiagMatrix D (lambda);

          retval = FloatComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 9 -*-
octave_value
xpow (const FloatComplex& a, const FloatComplex& b)
{
  FloatComplex result;
  result = std::pow (a, b);
  return result;
}

// -*- 10 -*-
octave_value
xpow (const FloatComplex& a, const FloatComplexMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for x^A, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      FloatEIG b_eig (b);

      if (! error_state)
        {
          FloatComplexColumnVector lambda (b_eig.eigenvalues ());
          FloatComplexMatrix Q (b_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            {
              FloatComplex elt = lambda(i);
              if (std::imag (elt) == 0.0)
                lambda(i) = std::pow (a, std::real (elt));
              else
                lambda(i) = std::pow (a, elt);
            }
          FloatComplexDiagMatrix D (lambda);

          retval = FloatComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 11 -*-
octave_value
xpow (const FloatComplexMatrix& a, float b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      if (static_cast<int> (b) == b)
        {
          int btmp = static_cast<int> (b);
          if (btmp == 0)
            {
              retval = FloatDiagMatrix (nr, nr, 1.0);
            }
          else
            {
              // Too much copying?
              // FIXME: we shouldn't do this if the exponent is large...

              FloatComplexMatrix atmp;
              if (btmp < 0)
                {
                  btmp = -btmp;

                  octave_idx_type info;
                  float rcond = 0.0;
                  MatrixType mattype (a);

                  atmp = a.inverse (mattype, info, rcond, 1);

                  if (info == -1)
                    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
                }
              else
                atmp = a;

              FloatComplexMatrix result (atmp);

              btmp--;

              while (btmp > 0)
                {
                  if (btmp & 1)
                    result = result * atmp;

                  btmp >>= 1;

                  if (btmp > 0)
                    atmp = atmp * atmp;
                }

              retval = result;
            }
        }
      else
        {
          FloatEIG a_eig (a);

          if (! error_state)
            {
              FloatComplexColumnVector lambda (a_eig.eigenvalues ());
              FloatComplexMatrix Q (a_eig.eigenvectors ());

              for (octave_idx_type i = 0; i < nr; i++)
                lambda(i) = std::pow (lambda(i), b);

              FloatComplexDiagMatrix D (lambda);

              retval = FloatComplexMatrix (Q * D * Q.inverse ());
            }
          else
            error ("xpow: matrix diagonalization failed");
        }
    }

  return retval;
}

// -*- 12 -*-
octave_value
xpow (const FloatComplexMatrix& a, const FloatComplex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      FloatEIG a_eig (a);

      if (! error_state)
        {
          FloatComplexColumnVector lambda (a_eig.eigenvalues ());
          FloatComplexMatrix Q (a_eig.eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            lambda(i) = std::pow (lambda(i), b);

          FloatComplexDiagMatrix D (lambda);

          retval = FloatComplexMatrix (Q * D * Q.inverse ());
        }
      else
        error ("xpow: matrix diagonalization failed");
    }

  return retval;
}

// -*- 12d -*-
octave_value
xpow (const FloatComplexDiagMatrix& a, const FloatComplex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0 || nr != nc)
    error ("for A^b, A must be a square matrix. Use .^ for elementwise power.");
  else
    {
      FloatComplexDiagMatrix r (nr, nc);
      for (octave_idx_type i = 0; i < nc; i++)
        r(i, i) = std::pow (a(i, i), b);
      retval = r;
    }

  return retval;
}

// mixed
octave_value
xpow (const FloatComplexDiagMatrix& a, float b)
{
  return xpow (a, static_cast<FloatComplex> (b));
}

octave_value
xpow (const FloatDiagMatrix& a, const FloatComplex& b)
{
  return xpow (FloatComplexDiagMatrix (a), b);
}

// Safer pow functions that work elementwise for matrices.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   matrix         | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_matrix | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// FIXME: these functions need to be fixed so that things like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// -*- 1 -*-
octave_value
elem_xpow (float a, const FloatMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  float d1, d2;

  if (a < 0.0 && ! b.all_integers (d1, d2))
    {
      FloatComplex atmp (a);
      FloatComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (atmp, b (i, j));
          }

      retval = result;
    }
  else
    {
      FloatMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a, b (i, j));
          }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (float a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  FloatComplexMatrix result (nr, nc);
  FloatComplex atmp (a);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (atmp, b (i, j));
      }

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const FloatMatrix& a, float b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (! xisint (b) && a.any_element_is_negative ())
    {
      FloatComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();

            FloatComplex atmp (a (i, j));

            result (i, j) = std::pow (atmp, b);
          }

      retval = result;
    }
  else
    {
      FloatMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a (i, j), b);
          }

      retval = result;
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const FloatMatrix& a, const FloatMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  int convert_to_complex = 0;
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        float atmp = a (i, j);
        float btmp = b (i, j);
        if (atmp < 0.0 && static_cast<int> (btmp) != btmp)
          {
            convert_to_complex = 1;
            goto done;
          }
      }

done:

  if (convert_to_complex)
    {
      FloatComplexMatrix complex_result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            FloatComplex atmp (a (i, j));
            FloatComplex btmp (b (i, j));
            complex_result (i, j) = std::pow (atmp, btmp);
          }

      retval = complex_result;
    }
  else
    {
      FloatMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a (i, j), b (i, j));
          }

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const FloatMatrix& a, const FloatComplex& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (FloatComplex (a (i, j)), b);
      }

  return result;
}

// -*- 6 -*-
octave_value
elem_xpow (const FloatMatrix& a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (FloatComplex (a (i, j)), b (i, j));
      }

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const FloatComplex& a, const FloatMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        float btmp = b (i, j);
        if (xisint (btmp))
          result (i, j) = std::pow (a, static_cast<int> (btmp));
        else
          result (i, j) = std::pow (a, btmp);
      }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const FloatComplex& a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (a, b (i, j));
      }

  return result;
}

// -*- 9 -*-
octave_value
elem_xpow (const FloatComplexMatrix& a, float b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  FloatComplexMatrix result (nr, nc);

  if (xisint (b))
    {
      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a (i, j), static_cast<int> (b));
          }
    }
  else
    {
      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result (i, j) = std::pow (a (i, j), b);
          }
    }

  return result;
}

// -*- 10 -*-
octave_value
elem_xpow (const FloatComplexMatrix& a, const FloatMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        float btmp = b (i, j);
        if (xisint (btmp))
          result (i, j) = std::pow (a (i, j), static_cast<int> (btmp));
        else
          result (i, j) = std::pow (a (i, j), btmp);
      }

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const FloatComplexMatrix& a, const FloatComplex& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (a (i, j), b);
      }

  return result;
}

// -*- 12 -*-
octave_value
elem_xpow (const FloatComplexMatrix& a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result (i, j) = std::pow (a (i, j), b (i, j));
      }

  return result;
}

// Safer pow functions that work elementwise for N-d arrays.
//
//       op2 \ op1:   s   nd  cs   cnd
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   N_d            | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_N_d    | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// FIXME: these functions need to be fixed so that things like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// -*- 1 -*-
octave_value
elem_xpow (float a, const FloatNDArray& b)
{
  octave_value retval;

  if (a < 0.0 && ! b.all_integers ())
    {
      FloatComplex atmp (a);
      FloatComplexNDArray result (b.dims ());
      for (octave_idx_type i = 0; i < b.length (); i++)
        {
          octave_quit ();
          result(i) = std::pow (atmp, b(i));
        }

      retval = result;
    }
  else
    {
      FloatNDArray result (b.dims ());
      for (octave_idx_type i = 0; i < b.length (); i++)
        {
          octave_quit ();
          result (i) = std::pow (a, b(i));
        }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (float a, const FloatComplexNDArray& b)
{
  FloatComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a, b(i));
    }

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const FloatNDArray& a, float b)
{
  octave_value retval;

  if (! xisint (b))
    {
      if (a.any_element_is_negative ())
        {
          FloatComplexNDArray result (a.dims ());

          for (octave_idx_type i = 0; i < a.length (); i++)
            {
              octave_quit ();

              FloatComplex atmp (a (i));

              result(i) = std::pow (atmp, b);
            }

          retval = result;
        }
      else
        {
          FloatNDArray result (a.dims ());
          for (octave_idx_type i = 0; i < a.length (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), b);
            }

          retval = result;
        }
    }
  else
    {
      NoAlias<FloatNDArray> result (a.dims ());

      int ib = static_cast<int> (b);
      if (ib == 2)
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            result(i) = a(i) * a(i);
        }
      else if (ib == 3)
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            result(i) = a(i) * a(i) * a(i);
        }
      else if (ib == -1)
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            result(i) = 1.0f / a(i);
        }
      else
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), ib);
            }
        }

      retval = result;
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const FloatNDArray& a, const FloatNDArray& b)
{
  octave_value retval;

  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (is_valid_bsxfun ("operator .^", a_dims, b_dims))
        {
          //Potentially complex results
          FloatNDArray xa = octave_value_extract<FloatNDArray> (a);
          FloatNDArray xb = octave_value_extract<FloatNDArray> (b);
          if (! xb.all_integers () && xa.any_element_is_negative ())
            return octave_value (bsxfun_pow (FloatComplexNDArray (xa), xb));
          else
            return octave_value (bsxfun_pow (xa, xb));
        }
      else
        {
          gripe_nonconformant ("operator .^", a_dims, b_dims);
          return octave_value ();
        }
    }

  int len = a.length ();

  bool convert_to_complex = false;

  for (octave_idx_type i = 0; i < len; i++)
    {
      octave_quit ();
      float atmp = a(i);
      float btmp = b(i);
      if (atmp < 0.0 && static_cast<int> (btmp) != btmp)
        {
          convert_to_complex = true;
          goto done;
        }
    }

done:

  if (convert_to_complex)
    {
      FloatComplexNDArray complex_result (a_dims);

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_quit ();
          FloatComplex atmp (a(i));
          complex_result(i) = std::pow (atmp, b(i));
        }

      retval = complex_result;
    }
  else
    {
      FloatNDArray result (a_dims);

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_quit ();
          result(i) = std::pow (a(i), b(i));
        }

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const FloatNDArray& a, const FloatComplex& b)
{
  FloatComplexNDArray result (a.dims ());

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b);
    }

  return result;
}

// -*- 6 -*-
octave_value
elem_xpow (const FloatNDArray& a, const FloatComplexNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (is_valid_bsxfun ("operator .^", a_dims, b_dims))
        {
          return bsxfun_pow (a, b);
        }
      else
        {
          gripe_nonconformant ("operator .^", a_dims, b_dims);
          return octave_value ();
        }
    }

  FloatComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b(i));
    }

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const FloatComplex& a, const FloatNDArray& b)
{
  FloatComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      octave_quit ();
      float btmp = b(i);
      if (xisint (btmp))
        result(i) = std::pow (a, static_cast<int> (btmp));
      else
        result(i) = std::pow (a, btmp);
    }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const FloatComplex& a, const FloatComplexNDArray& b)
{
  FloatComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a, b(i));
    }

  return result;
}

// -*- 9 -*-
octave_value
elem_xpow (const FloatComplexNDArray& a, float b)
{
  FloatComplexNDArray result (a.dims ());

  if (xisint (b))
    {
      if (b == -1)
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            result.xelem (i) = 1.0f / a(i);
        }
      else
        {
          for (octave_idx_type i = 0; i < a.length (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), static_cast<int> (b));
            }
        }
    }
  else
    {
      for (octave_idx_type i = 0; i < a.length (); i++)
        {
          octave_quit ();
          result(i) = std::pow (a(i), b);
        }
    }

  return result;
}

// -*- 10 -*-
octave_value
elem_xpow (const FloatComplexNDArray& a, const FloatNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (is_valid_bsxfun ("operator .^", a_dims, b_dims))
        {
          return bsxfun_pow (a, b);
        }
      else
        {
          gripe_nonconformant ("operator .^", a_dims, b_dims);
          return octave_value ();
        }
    }

  FloatComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      float btmp = b(i);
      if (xisint (btmp))
        result(i) = std::pow (a(i), static_cast<int> (btmp));
      else
        result(i) = std::pow (a(i), btmp);
    }

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const FloatComplexNDArray& a, const FloatComplex& b)
{
  FloatComplexNDArray result (a.dims ());

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b);
    }

  return result;
}

// -*- 12 -*-
octave_value
elem_xpow (const FloatComplexNDArray& a, const FloatComplexNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (is_valid_bsxfun ("operator .^", a_dims, b_dims))
        {
          return bsxfun_pow (a, b);
        }
      else
        {
          gripe_nonconformant ("operator .^", a_dims, b_dims);
          return octave_value ();
        }
    }

  FloatComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.length (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b(i));
    }

  return result;
}
