/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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
#include "oct-cmplx.h"
#include "quit.h"

#include "error.h"
#include "oct-obj.h"
#include "utils.h"

#include "dSparse.h"
#include "CSparse.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "sparse-xpow.h"

static inline int
xisint (double x)
{
  return (D_NINT (x) == x
          && ((x >= 0 && x < std::numeric_limits<int>::max ())
              || (x <= 0 && x > std::numeric_limits<int>::min ())));
}


// Safer pow functions. Only two make sense for sparse matrices, the
// others should all promote to full matrices.

octave_value
xpow (const SparseMatrix& a, double b)
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
              SparseMatrix tmp = SparseMatrix (nr, nr, nr);
              for (octave_idx_type i = 0; i < nr; i++)
                {
                  tmp.data (i) = 1.0;
                  tmp.ridx (i) = i;
                }
              for (octave_idx_type i = 0; i < nr + 1; i++)
                tmp.cidx (i) = i;

              retval = tmp;
            }
          else
            {
              SparseMatrix atmp;
              if (btmp < 0)
                {
                  btmp = -btmp;

                  octave_idx_type info;
                  double rcond = 0.0;
                  MatrixType mattyp (a);

                  atmp = a.inverse (mattyp, info, rcond, 1);

                  if (info == -1)
                    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
                }
              else
                atmp = a;

              SparseMatrix result (atmp);

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
        error ("use full(a) ^ full(b)");
    }

  return retval;
}

octave_value
xpow (const SparseComplexMatrix& a, double b)
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
              SparseMatrix tmp = SparseMatrix (nr, nr, nr);
              for (octave_idx_type i = 0; i < nr; i++)
                {
                  tmp.data (i) = 1.0;
                  tmp.ridx (i) = i;
                }
              for (octave_idx_type i = 0; i < nr + 1; i++)
                tmp.cidx (i) = i;

              retval = tmp;
            }
          else
            {
              SparseComplexMatrix atmp;
              if (btmp < 0)
                {
                  btmp = -btmp;

                  octave_idx_type info;
                  double rcond = 0.0;
                  MatrixType mattyp (a);

                  atmp = a.inverse (mattyp, info, rcond, 1);

                  if (info == -1)
                    warning ("inverse: matrix singular to machine\
 precision, rcond = %g", rcond);
                }
              else
                atmp = a;

              SparseComplexMatrix result (atmp);

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
        error ("use full(a) ^ full(b)");
    }

  return retval;
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

// FIXME: these functions need to be fixed so that things
// like
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

// Handle special case of scalar-sparse-matrix .^ sparse-matrix.
// Forwarding to the scalar elem_xpow function and then converting the
// result back to a sparse matrix is a bit wasteful but it does not
// seem worth the effort to optimize -- how often does this case come up
// in practice?

template <class S, class SM>
inline octave_value
scalar_xpow (const S& a, const SM& b)
{
  octave_value val = elem_xpow (a, b);

  if (val.is_complex_type ())
    return SparseComplexMatrix (val.complex_matrix_value ());
  else
    return SparseMatrix (val.matrix_value ());
}

/*
%!assert (sparse (2) .^ [3, 4], sparse ([8, 16]));
%!assert (sparse (2i) .^ [3, 4], sparse ([-0-8i, 16]));
*/

// -*- 1 -*-
octave_value
elem_xpow (double a, const SparseMatrix& b)
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
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = std::pow (atmp, b(i,j));
            }
        }

      retval = result;
    }
  else
    {
      Matrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = std::pow (a, b(i,j));
            }
        }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (double a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  Complex atmp (a);
  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_quit ();
          result(i, j) = std::pow (atmp, b(i,j));
        }
    }

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const SparseMatrix& a, double b)
{
  // FIXME: What should a .^ 0 give?  Matlab gives a
  // sparse matrix with same structure as a, which is strictly
  // incorrect. Keep compatibility.

  octave_value retval;

  octave_idx_type nz = a.nnz ();

  if (b <= 0.0)
    {
      octave_idx_type nr = a.rows ();
      octave_idx_type nc = a.cols ();

      if (static_cast<int> (b) != b && a.any_element_is_negative ())
        {
          ComplexMatrix result (nr, nc, Complex (std::pow (0.0, b)));

          // FIXME: avoid apparent GNU libm bug by
          // converting A and B to complex instead of just A.
          Complex btmp (b);

          for (octave_idx_type j = 0; j < nc; j++)
            for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
              {
                octave_quit ();

                Complex atmp (a.data (i));

                result(a.ridx (i), j) = std::pow (atmp, btmp);
              }

          retval = octave_value (result);
        }
      else
        {
          Matrix result (nr, nc, (std::pow (0.0, b)));

          for (octave_idx_type j = 0; j < nc; j++)
            for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
              {
                octave_quit ();
                result(a.ridx (i), j) = std::pow (a.data (i), b);
              }

          retval = octave_value (result);
        }
    }
  else if (static_cast<int> (b) != b && a.any_element_is_negative ())
    {
      SparseComplexMatrix result (a);

      for (octave_idx_type i = 0; i < nz; i++)
        {
          octave_quit ();

          // FIXME: avoid apparent GNU libm bug by
          // converting A and B to complex instead of just A.

          Complex atmp (a.data (i));
          Complex btmp (b);

          result.data (i) = std::pow (atmp, btmp);
        }

      result.maybe_compress (true);

      retval = result;
    }
  else
    {
      SparseMatrix result (a);

      for (octave_idx_type i = 0; i < nz; i++)
        {
          octave_quit ();
          result.data (i) = std::pow (a.data (i), b);
        }

      result.maybe_compress (true);

      retval = result;
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const SparseMatrix& a, const SparseMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a.numel () == 1 && b.numel () > 1)
    return scalar_xpow (a(0), b);

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  int convert_to_complex = 0;
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
      {
        if (a.data(i) < 0.0)
          {
            double btmp = b (a.ridx (i), j);
            if (static_cast<int> (btmp) != btmp)
              {
                convert_to_complex = 1;
                goto done;
              }
          }
      }

done:

  // This is a dumb operator for sparse matrices anyway, and there is
  // no sensible way to handle the 0.^0 versus the 0.^x cases. Therefore
  // allocate a full matrix filled for the 0.^0 case and shrink it later
  // as needed

  if (convert_to_complex)
    {
      SparseComplexMatrix complex_result (nr, nc, Complex (1.0, 0.0));

      for (octave_idx_type j = 0; j < nc; j++)
        {
          for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
            {
              octave_quit ();
              complex_result.xelem (a.ridx (i), j) =
                std::pow (Complex (a.data (i)), Complex (b(a.ridx (i), j)));
            }
        }
      complex_result.maybe_compress (true);
      retval = complex_result;
    }
  else
    {
      SparseMatrix result (nr, nc, 1.0);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
            {
              octave_quit ();
              result.xelem (a.ridx (i), j) = std::pow (a.data (i),
                                                       b(a.ridx (i), j));
            }
        }
      result.maybe_compress (true);
      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const SparseMatrix& a, const Complex& b)
{
  octave_value retval;

  if (b == 0.0)
    // Can this case ever happen, due to automatic retyping with maybe_mutate?
    retval = octave_value (NDArray (a.dims (), 1));
  else
    {
      octave_idx_type nz = a.nnz ();
      SparseComplexMatrix result (a);

      for (octave_idx_type i = 0; i < nz; i++)
        {
          octave_quit ();
          result.data (i) = std::pow (Complex (a.data (i)), b);
        }

      result.maybe_compress (true);

      retval = result;
    }

  return retval;
}

// -*- 6 -*-
octave_value
elem_xpow (const SparseMatrix& a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a.numel () == 1 && b.numel () > 1)
    return scalar_xpow (a(0), b);

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  SparseComplexMatrix result (nr, nc, Complex (1.0, 0.0));
  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
        {
          octave_quit ();
          result.xelem (a.ridx(i), j) = std::pow (a.data (i), b(a.ridx (i), j));
        }
    }

  result.maybe_compress (true);

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const Complex& a, const SparseMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_quit ();
          double btmp = b (i, j);
          if (xisint (btmp))
            result (i, j) = std::pow (a, static_cast<int> (btmp));
          else
            result (i, j) = std::pow (a, btmp);
        }
    }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const Complex& a, const SparseComplexMatrix& b)
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

// -*- 9 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, double b)
{
  octave_value retval;

  if (b <= 0)
    {
      octave_idx_type nr = a.rows ();
      octave_idx_type nc = a.cols ();

      ComplexMatrix result (nr, nc, Complex (std::pow (0.0, b)));

      if (xisint (b))
        {
          for (octave_idx_type j = 0; j < nc; j++)
            for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
              {
                octave_quit ();
                result (a.ridx (i), j) =
                  std::pow (a.data (i), static_cast<int> (b));
              }
        }
      else
        {
          for (octave_idx_type j = 0; j < nc; j++)
            for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
              {
                octave_quit ();
                result (a.ridx (i), j) = std::pow (a.data (i), b);
              }
        }

      retval = result;
    }
  else
    {
      octave_idx_type nz = a.nnz ();

      SparseComplexMatrix result (a);

      if (xisint (b))
        {
          for (octave_idx_type i = 0; i < nz; i++)
            {
              octave_quit ();
              result.data (i) = std::pow (a.data (i), static_cast<int> (b));
            }
        }
      else
        {
          for (octave_idx_type i = 0; i < nz; i++)
            {
              octave_quit ();
              result.data (i) = std::pow (a.data (i), b);
            }
        }

      result.maybe_compress (true);

      retval = result;
    }

  return retval;
}

// -*- 10 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, const SparseMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a.numel () == 1 && b.numel () > 1)
    return scalar_xpow (a(0), b);

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  SparseComplexMatrix result (nr, nc, Complex (1.0, 0.0));
  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
        {
          octave_quit ();
          double btmp = b(a.ridx (i), j);
          Complex tmp;

          if (xisint (btmp))
            result.xelem (a.ridx (i), j) = std::pow (a.data (i),
                                              static_cast<int> (btmp));
          else
            result.xelem (a.ridx (i), j) = std::pow (a.data (i), btmp);
        }
    }

  result.maybe_compress (true);

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, const Complex& b)
{
  octave_value retval;

  if (b == 0.0)
    // Can this case ever happen, due to automatic retyping with maybe_mutate?
    retval = octave_value (NDArray (a.dims (), 1));
  else
    {

      octave_idx_type nz = a.nnz ();

      SparseComplexMatrix result (a);

      for (octave_idx_type i = 0; i < nz; i++)
        {
          octave_quit ();
          result.data (i) = std::pow (a.data (i), b);
        }

      result.maybe_compress (true);

      retval = result;
    }

  return retval;
}

// -*- 12 -*-
octave_value
elem_xpow (const SparseComplexMatrix& a, const SparseComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a.numel () == 1 && b.numel () > 1)
    return scalar_xpow (a(0), b);

  if (nr != b_nr || nc != b_nc)
    {
      gripe_nonconformant ("operator .^", nr, nc, b_nr, b_nc);
      return octave_value ();
    }

  SparseComplexMatrix result (nr, nc, Complex (1.0, 0.0));
  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
        {
          octave_quit ();
          result.xelem (a.ridx (i), j) = std::pow (a.data (i),
                                                   b(a.ridx (i), j));
        }
    }
  result.maybe_compress (true);

  return result;
}
