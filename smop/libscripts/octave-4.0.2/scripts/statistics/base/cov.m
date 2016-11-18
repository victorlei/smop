## Copyright (C) 1995-2015 Kurt Hornik
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

## -*- texinfo -*-
## @deftypefn  {Function File} {} cov (@var{x})
## @deftypefnx {Function File} {} cov (@var{x}, @var{opt})
## @deftypefnx {Function File} {} cov (@var{x}, @var{y})
## @deftypefnx {Function File} {} cov (@var{x}, @var{y}, @var{opt})
## Compute the covariance matrix.
##
## If each row of @var{x} and @var{y} is an observation, and each column is
## a variable, then the @w{(@var{i}, @var{j})-th} entry of
## @code{cov (@var{x}, @var{y})} is the covariance between the @var{i}-th
## variable in @var{x} and the @var{j}-th variable in @var{y}.
## @tex
## $$
## \sigma_{ij} = {1 \over N-1} \sum_{i=1}^N (x_i - \bar{x})(y_i - \bar{y})
## $$
## where $\bar{x}$ and $\bar{y}$ are the mean values of $x$ and $y$.
## @end tex
## @ifnottex
##
## @example
## cov (x) = 1/N-1 * SUM_i (x(i) - mean(x)) * (y(i) - mean(y))
## @end example
##
## @end ifnottex
##
## If called with one argument, compute @code{cov (@var{x}, @var{x})}, the
## covariance between the columns of @var{x}.
##
## The argument @var{opt} determines the type of normalization to use.
## Valid values are
##
## @table @asis
## @item 0:
##   normalize with @math{N-1}, provides the best unbiased estimator of the
## covariance [default]
##
## @item 1:
##   normalize with @math{N}, this provides the second moment around the mean
## @end table
##
## Compatibility Note:: Octave always computes the covariance matrix.
## For two inputs, however, @sc{matlab} will calculate
## @code{cov (@var{x}(:), @var{y}(:))} whenever the number of elements in
## @var{x} and @var{y} are equal.  This will result in a scalar rather than
## a matrix output.  Code relying on this odd definition will need to be
## changed when running in Octave.
## @seealso{corr}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute covariances

function c = cov (x, y = [], opt = 0)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (   ! (isnumeric (x) || islogical (x))
      || ! (isnumeric (y) || islogical (y)))
    error ("cov: X and Y must be numeric matrices or vectors");
  endif

  if (ndims (x) != 2 || ndims (y) != 2)
    error ("cov: X and Y must be 2-D matrices or vectors");
  endif

  if (nargin == 2 && isscalar (y))
    opt = y;
  endif

  if (opt != 0 && opt != 1)
    error ("cov: normalization OPT must be 0 or 1");
  endif

  ## Special case, scalar has zero covariance
  if (isscalar (x))
    if (isa (x, "single"))
      c = single (0);
    else
      c = 0;
    endif
    return;
  endif

  if (isrow (x))
    x = x.';
  endif
  n = rows (x);

  if (nargin == 1 || isscalar (y))
    x = center (x, 1);
    c = conj (x' * x / (n - 1 + opt));
  else
    if (isrow (y))
      y = y.';
    endif
    if (rows (y) != n)
      error ("cov: X and Y must have the same number of observations");
    endif
    x = center (x, 1);
    y = center (y, 1);
    c = conj (x' * y / (n - 1 + opt));
  endif

endfunction


%!test
%! x = rand (10);
%! cx1 = cov (x);
%! cx2 = cov (x, x);
%! assert (size (cx1) == [10, 10] && size (cx2) == [10, 10]);
%! assert (cx1, cx2, 1e1*eps);

%!test
%! x = [1:3]';
%! y = [3:-1:1]';
%! assert (cov (x, y), -1, 5*eps);
%! assert (cov (x, flipud (y)), 1, 5*eps);
%! assert (cov ([x, y]), [1 -1; -1 1], 5*eps);

%!test
%! x = single ([1:3]');
%! y = single ([3:-1:1]');
%! assert (cov (x, y), single (-1), 5*eps);
%! assert (cov (x, flipud (y)), single (1), 5*eps);
%! assert (cov ([x, y]), single ([1 -1; -1 1]), 5*eps);

%!test
%! x = [1:5];
%! c = cov (x);
%! assert (isscalar (c));
%! assert (c, 2.5);

%!assert (cov (5), 0)
%!assert (cov (single (5)), single (0))

%!test
%! x = [1:5];
%! c = cov (x, 0);
%! assert (c, 2.5);
%! c = cov (x, 1);
%! assert (c, 2);

## Test input validation
%!error cov ()
%!error cov (1, 2, 3, 4)
%!error cov ([1; 2], ["A", "B"])
%!error cov (ones (2,2,2))
%!error cov (ones (2,2), ones (2,2,2))
%!error cov (1, 3)
%!error cov (ones (2,2), ones (3,2))

