/*

Copyright (C) 2002-2015 John W. Eaton

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

// Author: Paul Kienzle <pkienzle@users.sf.net>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dMatrix.h"
#include "fMatrix.h"
#include "CMatrix.h"
#include "fCMatrix.h"

#include "dSparse.h"
#include "CSparse.h"

#include "dDiagMatrix.h"
#include "fDiagMatrix.h"
#include "CDiagMatrix.h"
#include "fCDiagMatrix.h"

#include "PermMatrix.h"

#include "mx-inlines.cc"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "oct-obj.h"

template <class R, class T>
static MArray<T>
kron (const MArray<R>& a, const MArray<T>& b)
{
  assert (a.ndims () == 2);
  assert (b.ndims () == 2);

  octave_idx_type nra = a.rows ();
  octave_idx_type nrb = b.rows ();
  octave_idx_type nca = a.cols ();
  octave_idx_type ncb = b.cols ();

  MArray<T> c (dim_vector (nra*nrb, nca*ncb));
  T *cv = c.fortran_vec ();

  for (octave_idx_type ja = 0; ja < nca; ja++)
    for (octave_idx_type jb = 0; jb < ncb; jb++)
      for (octave_idx_type ia = 0; ia < nra; ia++)
        {
          octave_quit ();
          mx_inline_mul (nrb, cv, a(ia, ja), b.data () + nrb*jb);
          cv += nrb;
        }

  return c;
}

template <class R, class T>
static MArray<T>
kron (const MDiagArray2<R>& a, const MArray<T>& b)
{
  assert (b.ndims () == 2);

  octave_idx_type nra = a.rows ();
  octave_idx_type nrb = b.rows ();
  octave_idx_type dla = a.diag_length ();
  octave_idx_type nca = a.cols ();
  octave_idx_type ncb = b.cols ();

  MArray<T> c (dim_vector (nra*nrb, nca*ncb), T ());

  for (octave_idx_type ja = 0; ja < dla; ja++)
    for (octave_idx_type jb = 0; jb < ncb; jb++)
      {
        octave_quit ();
        mx_inline_mul (nrb, &c.xelem (ja*nrb, ja*ncb + jb), a.dgelem (ja),
                       b.data () + nrb*jb);
      }

  return c;
}

template <class T>
static MSparse<T>
kron (const MSparse<T>& A, const MSparse<T>& B)
{
  octave_idx_type idx = 0;
  MSparse<T> C (A.rows () * B.rows (), A.columns () * B.columns (),
                A.nnz () * B.nnz ());

  C.cidx (0) = 0;

  for (octave_idx_type Aj = 0; Aj < A.columns (); Aj++)
    for (octave_idx_type Bj = 0; Bj < B.columns (); Bj++)
      {
        octave_quit ();
        for (octave_idx_type Ai = A.cidx (Aj); Ai < A.cidx (Aj+1); Ai++)
          {
            octave_idx_type Ci = A.ridx (Ai) * B.rows ();
            const T v = A.data (Ai);

            for (octave_idx_type Bi = B.cidx (Bj); Bi < B.cidx (Bj+1); Bi++)
              {
                C.data (idx) = v * B.data (Bi);
                C.ridx (idx++) = Ci + B.ridx (Bi);
              }
          }
        C.cidx (Aj * B.columns () + Bj + 1) = idx;
      }

  return C;
}

static PermMatrix
kron (const PermMatrix& a, const PermMatrix& b)
{
  octave_idx_type na = a.rows ();
  octave_idx_type nb = b.rows ();
  const Array<octave_idx_type>& pa = a.col_perm_vec ();
  const Array<octave_idx_type>& pb = b.col_perm_vec ();
  Array<octave_idx_type> res_perm (dim_vector (na * nb, 1));
  octave_idx_type rescol = 0;
  for (octave_idx_type i = 0; i < na; i++)
    {
      octave_idx_type a_add = pa(i) * nb;
      for (octave_idx_type j = 0; j < nb; j++)
        res_perm.xelem (rescol++) = a_add + pb(j);
    }

  return PermMatrix (res_perm, true);
}

template <class MTA, class MTB>
octave_value
do_kron (const octave_value& a, const octave_value& b)
{
  MTA am = octave_value_extract<MTA> (a);
  MTB bm = octave_value_extract<MTB> (b);
  return octave_value (kron (am, bm));
}

octave_value
dispatch_kron (const octave_value& a, const octave_value& b)
{
  octave_value retval;
  if (a.is_perm_matrix () && b.is_perm_matrix ())
    retval = do_kron<PermMatrix, PermMatrix> (a, b);
  else if (a.is_sparse_type () || b.is_sparse_type ())
    {
      if (a.is_complex_type () || b.is_complex_type ())
        retval = do_kron<SparseComplexMatrix, SparseComplexMatrix> (a, b);
      else
        retval = do_kron<SparseMatrix, SparseMatrix> (a, b);
    }
  else if (a.is_diag_matrix ())
    {
      if (b.is_diag_matrix () && a.rows () == a.columns ()
          && b.rows () == b.columns ())
        {
          // We have two diagonal matrices, the product of those will be
          // another diagonal matrix.  To do that efficiently, extract
          // the diagonals as vectors and compute the product.  That
          // will be another vector, which we then use to construct a
          // diagonal matrix object.  Note that this will fail if our
          // digaonal matrix object is modified to allow the nonzero
          // values to be stored off of the principal diagonal (i.e., if
          // diag ([1,2], 3) is modified to return a diagonal matrix
          // object instead of a full matrix object).

          octave_value tmp = dispatch_kron (a.diag (), b.diag ());
          retval = tmp.diag ();
        }
      else if (a.is_single_type () || b.is_single_type ())
        {
          if (a.is_complex_type ())
            retval = do_kron<FloatComplexDiagMatrix, FloatComplexMatrix> (a, b);
          else if (b.is_complex_type ())
            retval = do_kron<FloatDiagMatrix, FloatComplexMatrix> (a, b);
          else
            retval = do_kron<FloatDiagMatrix, FloatMatrix> (a, b);
        }
      else
        {
          if (a.is_complex_type ())
            retval = do_kron<ComplexDiagMatrix, ComplexMatrix> (a, b);
          else if (b.is_complex_type ())
            retval = do_kron<DiagMatrix, ComplexMatrix> (a, b);
          else
            retval = do_kron<DiagMatrix, Matrix> (a, b);
        }
    }
  else if (a.is_single_type () || b.is_single_type ())
    {
      if (a.is_complex_type ())
        retval = do_kron<FloatComplexMatrix, FloatComplexMatrix> (a, b);
      else if (b.is_complex_type ())
        retval = do_kron<FloatMatrix, FloatComplexMatrix> (a, b);
      else
        retval = do_kron<FloatMatrix, FloatMatrix> (a, b);
    }
  else
    {
      if (a.is_complex_type ())
        retval = do_kron<ComplexMatrix, ComplexMatrix> (a, b);
      else if (b.is_complex_type ())
        retval = do_kron<Matrix, ComplexMatrix> (a, b);
      else
        retval = do_kron<Matrix, Matrix> (a, b);
    }
  return retval;
}


DEFUN (kron, args, , "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} kron (@var{A}, @var{B})\n\
@deftypefnx {Built-in Function} {} kron (@var{A1}, @var{A2}, @dots{})\n\
Form the Kronecker product of two or more matrices.\n\
\n\
This is defined block by block as\n\
\n\
@example\n\
x = [ a(i,j)*b ]\n\
@end example\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
kron (1:4, ones (3, 1))\n\
     @result{}  1  2  3  4\n\
         1  2  3  4\n\
         1  2  3  4\n\
@end group\n\
@end example\n\
\n\
If there are more than two input arguments @var{A1}, @var{A2}, @dots{},\n\
@var{An} the Kronecker product is computed as\n\
\n\
@example\n\
kron (kron (@var{A1}, @var{A2}), @dots{}, @var{An})\n\
@end example\n\
\n\
@noindent\n\
Since the Kronecker product is associative, this is well-defined.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin >= 2)
    {
      octave_value a = args(0);
      octave_value b = args(1);
      retval = dispatch_kron (a, b);
      for (octave_idx_type i = 2; i < nargin; i++)
        retval = dispatch_kron (retval, args(i));
    }
  else
    print_usage ();

  return retval;
}


/*
%!test
%! x = ones (2);
%! assert (kron (x, x), ones (4));

%!shared x, y, z, p1, p2, d1, d2
%! x =  [1, 2];
%! y =  [-1, -2];
%! z =  [1,  2,  3,  4; 1,  2,  3,  4; 1,  2,  3,  4];
%! p1 = eye (3)([2, 3, 1], :);  ## Permutation matrix
%! p2 = [0 1 0; 0 0 1; 1 0 0];  ## Non-permutation equivalent
%! d1 = diag ([1 2 3]);         ## Diag type matrix
%! d2 = [1 0 0; 0 2 0; 0 0 3];  ## Non-diag equivalent
%!assert (kron (1:4, ones (3, 1)), z)
%!assert (kron (single (1:4), ones (3, 1)), single (z))
%!assert (kron (sparse (1:4), ones (3, 1)), sparse (z))
%!assert (kron (complex (1:4), ones (3, 1)), z)
%!assert (kron (complex (single(1:4)), ones (3, 1)), single(z))
%!assert (kron (x, y, z), kron (kron (x, y), z))
%!assert (kron (x, y, z), kron (x, kron (y, z)))
%!assert (kron (p1, p1), kron (p2, p2))
%!assert (kron (p1, p2), kron (p2, p1))
%!assert (kron (d1, d1), kron (d2, d2))
%!assert (kron (d1, d2), kron (d2, d1))


%!assert (kron (diag ([1, 2]), diag ([3, 4])), diag ([3, 4, 6, 8]))

%% Test for two diag matrices.  See the comments above in
%% dispatch_kron for this case.
%%
%!test
%! expected = zeros (16, 16);
%! expected (1, 11) = 3;
%! expected (2, 12) = 4;
%! expected (5, 15) = 6;
%! expected (6, 16) = 8;
%! assert (kron (diag ([1, 2], 2), diag ([3, 4], 2)), expected)
*/
