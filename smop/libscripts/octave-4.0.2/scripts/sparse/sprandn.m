## Copyright (C) 2004-2015 Paul Kienzle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.
##
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn  {Function File} {} sprandn (@var{m}, @var{n}, @var{d})
## @deftypefnx {Function File} {} sprandn (@var{m}, @var{n}, @var{d}, @var{rc})
## @deftypefnx {Function File} {} sprandn (@var{s})
## Generate a sparse matrix with normally distributed random values.
##
## The size of the matrix is @var{m}x@var{n} with a density of values @var{d}.
## @var{d} must be between 0 and 1.  Values will be normally distributed with a
## mean of 0 and a variance of 1.
##
## If called with a single matrix argument, a sparse matrix is generated with
## random values wherever the matrix @var{s} is nonzero.
##
## If called with a scalar fourth argument @var{rc}, a random sparse matrix
## with reciprocal condition number @var{rc} is generated.  If @var{rc} is
## a vector, then it specifies the first singular values of the generated
## matrix (@code{length (@var{rc}) <= min (@var{m}, @var{n})}).
##
## @seealso{sprand, sprandsym, randn}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function s = sprandn (m, n, d, rc)

  if (nargin == 1 )
    s = __sprand_impl__ (m, @randn);
  elseif ( nargin == 3)
    s = __sprand_impl__ (m, n, d, "sprandn", @randn);
  elseif (nargin == 4)
    s = __sprand_impl__ (m, n, d, rc, "sprandn", @randn);
  else
    print_usage ();
  endif

endfunction


## Test 3-input calling form
%!test
%! s = sprandn (4, 10, 0.1);
%! assert (size (s), [4, 10]);
%! assert (nnz (s) / numel (s), 0.1);

## Test 4-input calling form
%!test
%! d = rand ();
%! s1 = sprandn (100, 100, d, 0.4);
%! rc = [5, 4, 3, 2, 1, 0.1];
%! s2 = sprandn (100, 100, d, rc);
%! s3 = sprandn (6, 4, d, rc);
%! assert (svd (s2)'(1:length (rc)), rc, sqrt (eps));
%! assert (1/cond (s1), 0.4, sqrt (eps));
%! assert (nnz (s1) / (100*100), d, 0.02);
%! assert (nnz (s2) / (100*100), d, 0.02);
%! assert (svd (s3)', [5 4 3 2], sqrt (eps));

## Test 1-input calling form
%!test
%! s = sprandn (sparse ([1 2 3], [3 2 3], [2 2 2]));
%! [i, j] = find (s);
%! assert (sort (i), [1 2 3]');
%! assert (sort (j), [2 3 3]');

## Test very large, very low density matrix doesn't fail
%!test
%! s = sprandn (1e6,1e6,1e-7);

## Test input validation
%!error sprandn ()
%!error sprandn (1, 2)
%!error sprandn (1, 2, 3, 4)
%!error <M must be an integer greater than 0> sprandn (ones (3), 3, 0.5)
%!error <M must be an integer greater than 0> sprandn (3.5, 3, 0.5)
%!error <M must be an integer greater than 0> sprandn (0, 3, 0.5)
%!error <N must be an integer greater than 0> sprandn (3, ones (3), 0.5)
%!error <N must be an integer greater than 0> sprandn (3, 3.5, 0.5)
%!error <N must be an integer greater than 0> sprandn (3, 0, 0.5)
%!error <D must be between 0 and 1> sprandn (3, 3, -1)
%!error <D must be between 0 and 1> sprandn (3, 3, 2)
%!error <RC must be a scalar or vector> sprandn (2, 2, 0.2, ones (3,3))
%!error <RC must be between 0 and 1> sprandn (2, 2, 0.2, -1)
%!error <RC must be between 0 and 1> sprandn (2, 2, 0.2, 2)

