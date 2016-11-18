/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <string>

#include "variables.h"
#include "utils.h"
#include "pager.h"
#include "defun.h"
#include "gripes.h"
#include "quit.h"
#include "unwind-prot.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "ov-bool-sparse.h"

DEFUN (issparse, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} issparse (@var{x})\n\
Return true if @var{x} is a sparse matrix.\n\
@seealso{ismatrix}\n\
@end deftypefn")
{
  if (args.length () != 1)
    {
      print_usage ();
      return octave_value ();
    }
  else
    return octave_value (args(0).is_sparse_type ());
}

DEFUN (sparse, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{s} =} sparse (@var{a})\n\
@deftypefnx {Built-in Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv}, @var{m}, @var{n})\n\
@deftypefnx {Built-in Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv})\n\
@deftypefnx {Built-in Function} {@var{s} =} sparse (@var{m}, @var{n})\n\
@deftypefnx {Built-in Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{s}, @var{m}, @var{n}, \"unique\")\n\
@deftypefnx {Built-in Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv}, @var{m}, @var{n}, @var{nzmax})\n\
Create a sparse matrix from a full matrix, or row, column, value triplets.\n\
\n\
If @var{a} is a full matrix, convert it to a sparse matrix representation,\n\
removing all zero values in the process.\n\
\n\
Given the integer index vectors @var{i} and @var{j}, and a 1-by-@code{nnz}\n\
vector of real or complex values @var{sv}, construct the sparse matrix\n\
@code{S(@var{i}(@var{k}),@var{j}(@var{k})) = @var{sv}(@var{k})} with overall\n\
dimensions @var{m} and @var{n}.  If any of @var{sv}, @var{i} or @var{j} are\n\
scalars, they are expanded to have a common size.\n\
\n\
If @var{m} or @var{n} are not specified their values are derived from the\n\
maximum index in the vectors @var{i} and @var{j} as given by\n\
@code{@var{m} = max (@var{i})}, @code{@var{n} = max (@var{j})}.\n\
\n\
@strong{Note}: if multiple values are specified with the same @var{i},\n\
@var{j} indices, the corresponding value in @var{s} will be the sum of the\n\
values at the repeated location.  See @code{accumarray} for an example of how\n\
to produce different behavior, such as taking the minimum instead.\n\
\n\
If the option @qcode{\"unique\"} is given, and more than one value is\n\
specified at the same @var{i}, @var{j} indices, then the last specified\n\
value will be used.\n\
\n\
@code{sparse (@var{m}, @var{n})} will create an empty @var{m}x@var{n} sparse\n\
matrix and is equivalent to @code{sparse ([], [], [], @var{m}, @var{n})}\n\
\n\
The argument @code{nzmax} is ignored but accepted for compatibility with\n\
@sc{matlab}.\n\
\n\
Example 1 (sum at repeated indices):\n\
\n\
@example\n\
@group\n\
@var{i} = [1 1 2]; @var{j} = [1 1 2]; @var{sv} = [3 4 5];\n\
sparse (@var{i}, @var{j}, @var{sv}, 3, 4)\n\
@result{}\n\
Compressed Column Sparse (rows = 3, cols = 4, nnz = 2 [17%])\n\
\n\
  (1, 1) ->  7\n\
  (2, 2) ->  5\n\
@end group\n\
@end example\n\
\n\
Example 2 (\"unique\" option):\n\
\n\
@example\n\
@group\n\
@var{i} = [1 1 2]; @var{j} = [1 1 2]; @var{sv} = [3 4 5];\n\
sparse (@var{i}, @var{j}, @var{sv}, 3, 4, \"unique\")\n\
@result{}\n\
Compressed Column Sparse (rows = 3, cols = 4, nnz = 2 [17%])\n\
\n\
  (1, 1) ->  4\n\
  (2, 2) ->  5\n\
@end group\n\
@end example\n\
@seealso{full, accumarray, spalloc, spdiags, speye, spones, sprand, sprandn, sprandsym, spconvert, spfun}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  // Temporarily disable sparse_auto_mutate if set (it's obsolete anyway).
  unwind_protect frame;
  frame.protect_var (Vsparse_auto_mutate);
  Vsparse_auto_mutate = false;

  if (nargin == 1)
    {
      octave_value arg = args(0);
      if (arg.is_bool_type ())
        retval = arg.sparse_bool_matrix_value ();
      else if (arg.is_complex_type ())
        retval = arg.sparse_complex_matrix_value ();
      else if (arg.is_numeric_type ())
        retval = arg.sparse_matrix_value ();
      else
        gripe_wrong_type_arg ("sparse", arg);
    }
  else if (nargin == 2)
    {
      octave_idx_type m = 0;
      octave_idx_type n = 0;

      get_dimensions (args(0), args(1), "sparse", m, n);

      if (! error_state)
        {
          if (m >= 0 && n >= 0)
            retval = SparseMatrix (m, n);
          else
            error ("sparse: dimensions must be non-negative");
        }
    }
  else if (nargin >= 3)
    {
      bool summation = true;
      if (nargin > 3 && args(nargin-1).is_string ())
        {
          std::string opt = args(nargin-1).string_value ();
          if (opt == "unique")
            summation = false;
          else if (opt == "sum" || opt == "summation")
            summation = true;
          else
            error ("sparse: invalid option: %s", opt.c_str ());

          nargin -= 1;
        }

      if (! error_state)
        {
          octave_idx_type m, n, nzmax;
          m = n = nzmax = -1;
          if (nargin == 6)
            {
              nzmax = args(5).idx_type_value ();
              nargin --;
            }

          if (nargin == 5)
            {
              get_dimensions (args(3), args(4), "sparse", m, n);

              if (! error_state && (m < 0 || n < 0))
                error ("sparse: dimensions must be non-negative");
            }
          else if (nargin != 3)
            print_usage ();

          if (! error_state)
            {
              idx_vector i = args(0).index_vector ();
              idx_vector j = args(1).index_vector ();

              if (args(2).is_bool_type ())
                retval = SparseBoolMatrix (args(2).bool_array_value (), i, j,
                                           m, n, summation, nzmax);
              else if (args(2).is_complex_type ())
                retval = SparseComplexMatrix (args(2).complex_array_value (),
                                              i, j, m, n, summation, nzmax);
              else if (args(2).is_numeric_type ())
                retval = SparseMatrix (args(2).array_value (), i, j,
                                       m, n, summation, nzmax);
              else
                gripe_wrong_type_arg ("sparse", args(2));
            }

        }
    }

  return retval;
}

DEFUN (spalloc, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{s} =} spalloc (@var{m}, @var{n}, @var{nz})\n\
Create an @var{m}-by-@var{n} sparse matrix with pre-allocated space for at\n\
most @var{nz} nonzero elements.\n\
\n\
This is useful for building a matrix incrementally by a sequence of indexed\n\
assignments.  Subsequent indexed assignments after @code{spalloc} will reuse\n\
the pre-allocated memory, provided they are of one of the simple forms\n\
\n\
@itemize\n\
@item @code{@var{s}(I:J) = @var{x}}\n\
\n\
@item @code{@var{s}(:,I:J) = @var{x}}\n\
\n\
@item @code{@var{s}(K:L,I:J) = @var{x}}\n\
@end itemize\n\
\n\
@b{and} that the following conditions are met:\n\
\n\
@itemize\n\
@item the assignment does not decrease nnz (@var{S}).\n\
\n\
@item after the assignment, nnz (@var{S}) does not exceed @var{nz}.\n\
\n\
@item no index is out of bounds.\n\
@end itemize\n\
\n\
Partial movement of data may still occur, but in general the assignment will\n\
be more memory and time efficient under these circumstances.  In particular,\n\
it is possible to efficiently build a pre-allocated sparse matrix from a\n\
contiguous block of columns.\n\
\n\
The amount of pre-allocated memory for a given matrix may be queried using\n\
the function @code{nzmax}.\n\
@seealso{nzmax, sparse}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      octave_idx_type m = args(0).idx_type_value ();
      octave_idx_type n = args(1).idx_type_value ();
      octave_idx_type nz = 0;
      if (nargin == 3)
        nz = args(2).idx_type_value ();
      if (error_state)
        ;
      else if (m >= 0 && n >= 0 && nz >= 0)
        retval = SparseMatrix (dim_vector (m, n), nz);
      else
        error ("spalloc: M,N,NZ must be non-negative");
    }
  else
    print_usage ();

  return retval;
}
