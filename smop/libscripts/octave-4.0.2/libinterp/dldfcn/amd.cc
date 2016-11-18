/*

Copyright (C) 2008-2015 David Bateman

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

// This is the octave interface to amd, which bore the copyright given
// in the help of the functions.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>

#include <string>
#include <vector>

#include "ov.h"
#include "defun-dld.h"
#include "pager.h"
#include "ov-re-mat.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "oct-map.h"

#include "oct-sparse.h"
#include "oct-locbuf.h"

#ifdef USE_64_BIT_IDX_T
#define AMD_NAME(name) amd_l ## name
#else
#define AMD_NAME(name) amd ## name
#endif

DEFUN_DLD (amd, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{p} =} amd (@var{S})\n\
@deftypefnx {Loadable Function} {@var{p} =} amd (@var{S}, @var{opts})\n\
\n\
Return the approximate minimum degree permutation of a matrix.\n\
\n\
This is a permutation such that the Cholesky@tie{}factorization of\n\
@code{@var{S} (@var{p}, @var{p})} tends to be sparser than the\n\
Cholesky@tie{}factorization of @var{S} itself.  @code{amd} is typically\n\
faster than @code{symamd} but serves a similar purpose.\n\
\n\
The optional parameter @var{opts} is a structure that controls the behavior\n\
of @code{amd}.  The fields of the structure are\n\
\n\
@table @asis\n\
@item @var{opts}.dense\n\
Determines what @code{amd} considers to be a dense row or column of the\n\
input matrix.  Rows or columns with more than @code{max (16, (dense *\n\
sqrt (@var{n})))} entries, where @var{n} is the order of the matrix @var{S},\n\
are ignored by @code{amd} during the calculation of the permutation.\n\
The value of dense must be a positive scalar and the default value is 10.0\n\
\n\
@item @var{opts}.aggressive\n\
If this value is a nonzero scalar, then @code{amd} performs aggressive\n\
absorption.  The default is not to perform aggressive absorption.\n\
@end table\n\
\n\
The author of the code itself is Timothy A. Davis\n\
@email{davis@@cise.ufl.edu}, University of Florida\n\
(see @url{http://www.cise.ufl.edu/research/sparse/amd}).\n\
@seealso{symamd, colamd}\n\
@end deftypefn")
{
  octave_value_list retval;

#ifdef HAVE_AMD
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();
  else
    {
      octave_idx_type n_row, n_col;
      const octave_idx_type *ridx, *cidx;
      SparseMatrix sm;
      SparseComplexMatrix scm;

      if (args(0).is_sparse_type ())
        {
          if (args(0).is_complex_type ())
            {
              scm = args(0).sparse_complex_matrix_value ();
              n_row = scm.rows ();
              n_col = scm.cols ();
              ridx = scm.xridx ();
              cidx = scm.xcidx ();
            }
          else
            {
              sm = args(0).sparse_matrix_value ();
              n_row = sm.rows ();
              n_col = sm.cols ();
              ridx = sm.xridx ();
              cidx = sm.xcidx ();
            }
        }
      else
        {
          if (args(0).is_complex_type ())
            sm = SparseMatrix (real (args(0).complex_matrix_value ()));
          else
            sm = SparseMatrix (args(0).matrix_value ());

          n_row = sm.rows ();
          n_col = sm.cols ();
          ridx = sm.xridx ();
          cidx = sm.xcidx ();
        }

      if (!error_state && n_row != n_col)
        error ("amd: matrix S must be square");

      if (!error_state)
        {
          OCTAVE_LOCAL_BUFFER (double, Control, AMD_CONTROL);
          AMD_NAME (_defaults) (Control) ;
          if (nargin > 1)
            {
              octave_scalar_map arg1 = args(1).scalar_map_value ();

              if (!error_state)
                {
                  octave_value tmp;

                  tmp = arg1.getfield ("dense");
                  if (tmp.is_defined ())
                    Control[AMD_DENSE] = tmp.double_value ();

                  tmp = arg1.getfield ("aggressive");
                  if (tmp.is_defined ())
                    Control[AMD_AGGRESSIVE] = tmp.double_value ();
                }
              else
                error ("amd: OPTS argument must be a scalar structure");
            }

          if (!error_state)
            {
              OCTAVE_LOCAL_BUFFER (octave_idx_type, P, n_col);
              Matrix xinfo (AMD_INFO, 1);
              double *Info = xinfo.fortran_vec ();

              // FIXME: how can we manage the memory allocation of amd
              //        in a cleaner manner?
              SUITESPARSE_ASSIGN_FPTR (malloc_func, amd_malloc, malloc);
              SUITESPARSE_ASSIGN_FPTR (free_func, amd_free, free);
              SUITESPARSE_ASSIGN_FPTR (calloc_func, amd_calloc, calloc);
              SUITESPARSE_ASSIGN_FPTR (realloc_func, amd_realloc, realloc);
              SUITESPARSE_ASSIGN_FPTR (printf_func, amd_printf, printf);

              octave_idx_type result = AMD_NAME (_order) (n_col, cidx, ridx, P,
                                                          Control, Info);

              switch (result)
                {
                case AMD_OUT_OF_MEMORY:
                  error ("amd: out of memory");
                  break;
                case AMD_INVALID:
                  error ("amd: matrix S is corrupted");
                  break;
                default:
                  {
                    if (nargout > 1)
                      retval(1) = xinfo;

                    Matrix Pout (1, n_col);
                    for (octave_idx_type i = 0; i < n_col; i++)
                      Pout.xelem (i) = P[i] + 1;

                    retval(0) = Pout;
                  }
                }
            }
        }
    }
#else

  error ("amd: not available in this version of Octave");

#endif

  return retval;
}
/*
%!shared A, A2, opts
%! A = ones (20, 30);
%! A2 = ones (30, 30);
%!
%!testif HAVE_AMD
%! assert(amd (A2), [1:30])
%! opts.dense = 25;
%! assert(amd (A2, opts), [1:30])
%! opts.aggressive = 1;
%! assert(amd (A2, opts), [1:30])

%!error <matrix S must be square|not available in this version> amd (A)
%!error amd (A2, 2)
%!error <matrix S is corrupted|not available in this version> amd ([])
*/
