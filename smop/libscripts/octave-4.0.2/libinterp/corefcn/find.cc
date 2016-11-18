/*

Copyright (C) 1996-2015 John W. Eaton

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

#include "quit.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"

// Find at most N_TO_FIND nonzero elements in NDA.  Search forward if
// DIRECTION is 1, backward if it is -1.  NARGOUT is the number of
// output arguments.  If N_TO_FIND is -1, find all nonzero elements.

template <typename T>
octave_value_list
find_nonzero_elem_idx (const Array<T>& nda, int nargout,
                       octave_idx_type n_to_find, int direction)
{
  octave_value_list retval ((nargout == 0 ? 1 : nargout), Matrix ());

  Array<octave_idx_type> idx;
  if (n_to_find >= 0)
    idx = nda.find (n_to_find, direction == -1);
  else
    idx = nda.find ();

  // The maximum element is always at the end.
  octave_idx_type iext = idx.is_empty () ? 0 : idx.xelem (idx.numel () - 1) + 1;

  switch (nargout)
    {
    default:
    case 3:
      retval(2) = Array<T> (nda.index (idx_vector (idx)));
      // Fall through!

    case 2:
      {
        Array<octave_idx_type> jdx (idx.dims ());
        octave_idx_type n = idx.length ();
        octave_idx_type nr = nda.rows ();
        for (octave_idx_type i = 0; i < n; i++)
          {
            jdx.xelem (i) = idx.xelem (i) / nr;
            idx.xelem (i) %= nr;
          }
        iext = -1;
        retval(1) = idx_vector (jdx, -1);
      }
      // Fall through!

    case 1:
    case 0:
      retval(0) = idx_vector (idx, iext);
      break;
    }

  return retval;
}

template <typename T>
octave_value_list
find_nonzero_elem_idx (const Sparse<T>& v, int nargout,
                       octave_idx_type n_to_find, int direction)
{
  nargout = std::min (nargout, 5);
  octave_value_list retval ((nargout == 0 ? 1 : nargout), Matrix ());

  octave_idx_type nr = v.rows ();
  octave_idx_type nc = v.cols ();
  octave_idx_type nz = v.nnz ();

  // Search in the default range.
  octave_idx_type start_nc = -1;
  octave_idx_type end_nc = -1;
  octave_idx_type count;

  // Search for the range to search
  if (n_to_find < 0)
    {
      start_nc = 0;
      end_nc = nc;
      n_to_find = nz;
      count = nz;
    }
  else if (direction > 0)
    {
      for (octave_idx_type j = 0; j < nc; j++)
        {
          OCTAVE_QUIT;
          if (v.cidx (j) == 0 && v.cidx (j+1) != 0)
            start_nc = j;
          if (v.cidx (j+1) >= n_to_find)
            {
              end_nc = j + 1;
              break;
            }
        }
    }
  else
    {
      for (octave_idx_type j = nc; j > 0; j--)
        {
          OCTAVE_QUIT;
          if (v.cidx (j) == nz && v.cidx (j-1) != nz)
            end_nc = j;
          if (nz - v.cidx (j-1) >= n_to_find)
            {
              start_nc = j - 1;
              break;
            }
        }
    }

  count = (n_to_find > v.cidx (end_nc) - v.cidx (start_nc) ?
           v.cidx (end_nc) - v.cidx (start_nc) : n_to_find);

  octave_idx_type result_nr;
  octave_idx_type result_nc;

  // Default case is to return a column vector, however, if the original
  // argument was a row vector, then force return of a row vector.
  if (nr == 1)
    {
      result_nr = 1;
      result_nc = count;
    }
  else
    {
      result_nr = count;
      result_nc = 1;
    }

  Matrix idx (result_nr, result_nc);

  Matrix i_idx (result_nr, result_nc);
  Matrix j_idx (result_nr, result_nc);

  Array<T> val (dim_vector (result_nr, result_nc));

  if (count > 0)
    {
      // Search for elements to return.  Only search the region where there
      // are elements to be found using the count that we want to find.
      for (octave_idx_type j = start_nc, cx = 0; j < end_nc; j++)
        for (octave_idx_type i = v.cidx (j); i < v.cidx (j+1); i++)
          {
            OCTAVE_QUIT;
            if (direction < 0 && i < nz - count)
              continue;
            i_idx(cx) = static_cast<double> (v.ridx (i) + 1);
            j_idx(cx) = static_cast<double> (j + 1);
            idx(cx) = j * nr + v.ridx (i) + 1;
            val(cx) = v.data(i);
            cx++;
            if (cx == count)
              break;
          }
    }
  else
    {
      // No items found.  Fixup return dimensions for Matlab compatibility.
      // The behavior to match is documented in Array.cc (Array<T>::find).
      if ((nr == 0 && nc == 0) || (nr == 1 && nc == 1))
        {
          idx.resize (0, 0);

          i_idx.resize (0, 0);
          j_idx.resize (0, 0);

          val.resize (dim_vector (0, 0));
        }
    }

  switch (nargout)
    {
    case 0:
    case 1:
      retval(0) = idx;
      break;

    case 5:
      retval(4) = nc;
      // Fall through

    case 4:
      retval(3) = nr;
      // Fall through

    case 3:
      retval(2) = val;
      // Fall through!

    case 2:
      retval(1) = j_idx;
      retval(0) = i_idx;
    }

  return retval;
}

octave_value_list
find_nonzero_elem_idx (const PermMatrix& v, int nargout,
                       octave_idx_type n_to_find, int direction)
{
  // There are far fewer special cases to handle for a PermMatrix.
  nargout = std::min (nargout, 5);
  octave_value_list retval ((nargout == 0 ? 1 : nargout), Matrix ());

  octave_idx_type nr = v.rows ();
  octave_idx_type nc = v.cols ();
  octave_idx_type start_nc, count;

  // Determine the range to search.
  if (n_to_find < 0 || n_to_find >= nc)
    {
      start_nc = 0;
      count = nc;
    }
  else if (direction > 0)
    {
      start_nc = 0;
      count = n_to_find;
    }
  else
    {
      start_nc = nc - n_to_find;
      count = n_to_find;
    }

  Matrix idx (count, 1);
  Matrix i_idx (count, 1);
  Matrix j_idx (count, 1);
  // Every value is 1.
  Array<double> val (dim_vector (count, 1), 1.0);

  if (count > 0)
    {
      const Array<octave_idx_type>& p = v.col_perm_vec ();
      for (octave_idx_type k = 0; k < count; k++)
        {
          OCTAVE_QUIT;
          const octave_idx_type j = start_nc + k;
          const octave_idx_type i = p(j);
          i_idx(k) = static_cast<double> (1+i);
          j_idx(k) = static_cast<double> (1+j);
          idx(k) = j * nc + i + 1;
        }
    }
  else
    {
      // FIXME: Is this case even possible?  A scalar permutation matrix seems
      // to devolve to a scalar full matrix, at least from the Octave command
      // line.  Perhaps this function could be called internally from C++ with
      // such a matrix.
      // No items found.  Fixup return dimensions for Matlab compatibility.
      // The behavior to match is documented in Array.cc (Array<T>::find).
      if ((nr == 0 && nc == 0) || (nr == 1 && nc == 1))
        {
          idx.resize (0, 0);

          i_idx.resize (0, 0);
          j_idx.resize (0, 0);

          val.resize (dim_vector (0, 0));
        }
    }

  switch (nargout)
    {
    case 0:
    case 1:
      retval(0) = idx;
      break;

    case 5:
      retval(4) = nc;
      // Fall through

    case 4:
      retval(3) = nc;
      // Fall through

    case 3:
      retval(2) = val;
      // Fall through!

    case 2:
      retval(1) = j_idx;
      retval(0) = i_idx;
    }

  return retval;
}

DEFUN (find, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{idx} =} find (@var{x})\n\
@deftypefnx {Built-in Function} {@var{idx} =} find (@var{x}, @var{n})\n\
@deftypefnx {Built-in Function} {@var{idx} =} find (@var{x}, @var{n}, @var{direction})\n\
@deftypefnx {Built-in Function} {[i, j] =} find (@dots{})\n\
@deftypefnx {Built-in Function} {[i, j, v] =} find (@dots{})\n\
Return a vector of indices of nonzero elements of a matrix, as a row if\n\
@var{x} is a row vector or as a column otherwise.\n\
\n\
To obtain a single index for each matrix element, Octave pretends that the\n\
columns of a matrix form one long vector (like Fortran arrays are stored).\n\
For example:\n\
\n\
@example\n\
@group\n\
find (eye (2))\n\
  @result{} [ 1; 4 ]\n\
@end group\n\
@end example\n\
\n\
If two inputs are given, @var{n} indicates the maximum number of elements to\n\
find from the beginning of the matrix or vector.\n\
\n\
If three inputs are given, @var{direction} should be one of\n\
@qcode{\"first\"} or @qcode{\"last\"}, requesting only the first or last\n\
@var{n} indices, respectively.  However, the indices are always returned in\n\
ascending order.\n\
\n\
If two outputs are requested, @code{find} returns the row and column\n\
indices of nonzero elements of a matrix.  For example:\n\
\n\
@example\n\
@group\n\
[i, j] = find (2 * eye (2))\n\
    @result{} i = [ 1; 2 ]\n\
    @result{} j = [ 1; 2 ]\n\
@end group\n\
@end example\n\
\n\
If three outputs are requested, @code{find} also returns a vector\n\
containing the nonzero values.  For example:\n\
\n\
@example\n\
@group\n\
[i, j, v] = find (3 * eye (2))\n\
       @result{} i = [ 1; 2 ]\n\
       @result{} j = [ 1; 2 ]\n\
       @result{} v = [ 3; 3 ]\n\
@end group\n\
@end example\n\
\n\
Note that this function is particularly useful for sparse matrices, as\n\
it extracts the nonzero elements as vectors, which can then be used to\n\
create the original matrix.  For example:\n\
\n\
@example\n\
@group\n\
sz = size (a);\n\
[i, j, v] = find (a);\n\
b = sparse (i, j, v, sz(1), sz(2));\n\
@end group\n\
@end example\n\
@seealso{nonzeros}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 3 || nargin < 1)
    {
      print_usage ();
      return retval;
    }

  // Setup the default options.
  octave_idx_type n_to_find = -1;
  if (nargin > 1)
    {
      double val = args(1).scalar_value ();

      if (error_state || (val < 0 || (! xisinf (val) && val != xround (val))))
        {
          error ("find: N must be a non-negative integer");
          return retval;
        }
      else if (! xisinf (val))
        n_to_find = val;
    }

  // Direction to do the searching (1 == forward, -1 == reverse).
  int direction = 1;
  if (nargin > 2)
    {
      direction = 0;

      std::string s_arg = args(2).string_value ();

      if (! error_state)
        {
          if (s_arg == "first")
            direction = 1;
          else if (s_arg == "last")
            direction = -1;
        }

      if (direction == 0)
        {
          error ("find: DIRECTION must be \"first\" or \"last\"");
          return retval;
        }
    }

  octave_value arg = args(0);

  if (arg.is_bool_type ())
    {
      if (arg.is_sparse_type ())
        {
          SparseBoolMatrix v = arg.sparse_bool_matrix_value ();

          if (! error_state)
            retval = find_nonzero_elem_idx (v, nargout,
                                            n_to_find, direction);
        }
      else if (nargout <= 1 && n_to_find == -1 && direction == 1)
        {
          // This case is equivalent to extracting indices from a logical
          // matrix. Try to reuse the possibly cached index vector.
          retval(0) = arg.index_vector ().unmask ();
        }
      else
        {
          boolNDArray v = arg.bool_array_value ();

          if (! error_state)
            retval = find_nonzero_elem_idx (v, nargout,
                                            n_to_find, direction);
        }
    }
  else if (arg.is_integer_type ())
    {
#define DO_INT_BRANCH(INTT) \
      else if (arg.is_ ## INTT ## _type ()) \
        { \
          INTT ## NDArray v = arg.INTT ## _array_value (); \
          \
          if (! error_state) \
            retval = find_nonzero_elem_idx (v, nargout, \
                                            n_to_find, direction);\
        }

      if (false)
        ;
      DO_INT_BRANCH (int8)
      DO_INT_BRANCH (int16)
      DO_INT_BRANCH (int32)
      DO_INT_BRANCH (int64)
      DO_INT_BRANCH (uint8)
      DO_INT_BRANCH (uint16)
      DO_INT_BRANCH (uint32)
      DO_INT_BRANCH (uint64)
      else
        panic_impossible ();
    }
  else if (arg.is_sparse_type ())
    {
      if (arg.is_real_type ())
        {
          SparseMatrix v = arg.sparse_matrix_value ();

          if (! error_state)
            retval = find_nonzero_elem_idx (v, nargout,
                                            n_to_find, direction);
        }
      else if (arg.is_complex_type ())
        {
          SparseComplexMatrix v = arg.sparse_complex_matrix_value ();

          if (! error_state)
            retval = find_nonzero_elem_idx (v, nargout,
                                            n_to_find, direction);
        }
      else
        gripe_wrong_type_arg ("find", arg);
    }
  else if (arg.is_perm_matrix ())
    {
      PermMatrix P = arg.perm_matrix_value ();

      if (! error_state)
        retval = find_nonzero_elem_idx (P, nargout, n_to_find, direction);
    }
  else if (arg.is_string ())
    {
      charNDArray chnda = arg.char_array_value ();

      if (! error_state)
        retval = find_nonzero_elem_idx (chnda, nargout, n_to_find, direction);
    }
  else if (arg.is_single_type ())
    {
      if (arg.is_real_type ())
        {
          FloatNDArray nda = arg.float_array_value ();

          if (! error_state)
            retval = find_nonzero_elem_idx (nda, nargout, n_to_find,
                                            direction);
        }
      else if (arg.is_complex_type ())
        {
          FloatComplexNDArray cnda = arg.float_complex_array_value ();

          if (! error_state)
            retval = find_nonzero_elem_idx (cnda, nargout, n_to_find,
                                            direction);
        }
    }
  else if (arg.is_real_type ())
    {
      NDArray nda = arg.array_value ();

      if (! error_state)
        retval = find_nonzero_elem_idx (nda, nargout, n_to_find, direction);
    }
  else if (arg.is_complex_type ())
    {
      ComplexNDArray cnda = arg.complex_array_value ();

      if (! error_state)
        retval = find_nonzero_elem_idx (cnda, nargout, n_to_find, direction);
    }
  else
    gripe_wrong_type_arg ("find", arg);

  return retval;
}

/*
%!assert (find (char ([0, 97])), 2)
%!assert (find ([1, 0, 1, 0, 1]), [1, 3, 5])
%!assert (find ([1; 0; 3; 0; 1]), [1; 3; 5])
%!assert (find ([0, 0, 2; 0, 3, 0; -1, 0, 0]), [3; 5; 7])

%!test
%! [i, j, v] = find ([0, 0, 2; 0, 3, 0; -1, 0, 0]);
%!
%! assert (i, [3; 2; 1]);
%! assert (j, [1; 2; 3]);
%! assert (v, [-1; 3; 2]);

%!assert (find (single ([1, 0, 1, 0, 1])), [1, 3, 5])
%!assert (find (single ([1; 0; 3; 0; 1])), [1; 3; 5])
%!assert (find (single ([0, 0, 2; 0, 3, 0; -1, 0, 0])), [3; 5; 7])

%!test
%! [i, j, v] = find (single ([0, 0, 2; 0, 3, 0; -1, 0, 0]));
%!
%! assert (i, [3; 2; 1]);
%! assert (j, [1; 2; 3]);
%! assert (v, single ([-1; 3; 2]));

%!test
%! pcol = [5 1 4 3 2];
%! P = eye (5) (:, pcol);
%! [i, j, v] = find (P);
%! [ifull, jfull, vfull] = find (full (P));
%! assert (i, ifull);
%! assert (j, jfull);
%! assert (all (v == 1));

%!test
%! prow = [5 1 4 3 2];
%! P = eye (5) (prow, :);
%! [i, j, v] = find (P);
%! [ifull, jfull, vfull] = find (full (P));
%! assert (i, ifull);
%! assert (j, jfull);
%! assert (all (v == 1));

%!assert (find ([2 0 1 0 5 0], 1), 1)
%!assert (find ([2 0 1 0 5 0], 2, "last"), [3, 5])

%!assert (find ([2 0 1 0 5 0], Inf), [1, 3, 5])
%!assert (find ([2 0 1 0 5 0], Inf, "last"), [1, 3, 5])

%!error find ()
*/
