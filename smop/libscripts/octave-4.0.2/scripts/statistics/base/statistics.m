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
## @deftypefn  {Function File} {} statistics (@var{x})
## @deftypefnx {Function File} {} statistics (@var{x}, @var{dim})
## Return a vector with the minimum, first quartile, median, third quartile,
## maximum, mean, standard deviation, skewness, and kurtosis of the elements of
## the vector @var{x}.
##
## If @var{x} is a matrix, calculate statistics over the first non-singleton
## dimension.
##
## If the optional argument @var{dim} is given, operate along this dimension.
## @seealso{min, max, median, mean, std, skewness, kurtosis}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute basic statistics

function stats = statistics (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("statistics: X must be a numeric vector or matrix");
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("statistics: DIM must be an integer and a valid dimension");
    endif
  endif

  if (sz(dim) < 2)
    error ("statistics: dimension of X is too small (<2)");
  endif

  emp_inv = quantile (x, [0.25; 0.5; 0.75], dim, 7);

  stats = cat (dim, min (x, [], dim), emp_inv, max (x, [], dim), mean (x, dim),
               std (x, [], dim), skewness (x, [], dim), kurtosis (x, [], dim));

endfunction


%!test
%! x = rand (7,5);
%! s = statistics (x);
%! assert (min (x), s(1,:), eps);
%! assert (median (x), s(3,:), eps);
%! assert (max (x), s(5,:), eps);
%! assert (mean (x), s(6,:), eps);
%! assert (std (x), s(7,:), eps);
%! assert (skewness (x), s(8,:), eps);
%! assert (kurtosis (x), s(9,:), eps);

%! x = rand (7,5);
%! s = statistics (x, 2);
%! assert (min (x, [], 2), s(:,1), eps);
%! assert (median (x, 2), s(:,3), eps);
%! assert (max (x, [], 2), s(:,5), eps);
%! assert (mean (x, 2), s(:,6), eps);
%! assert (std (x, [], 2), s(:,7), eps);
%! assert (skewness (x, [], 2), s(:,8), eps);
%! assert (kurtosis (x, [], 2), s(:,9), eps);

## Test input validation
%!error statistics ()
%!error statistics (1, 2, 3)
%!error statistics (['A'; 'B'])
%!error statistics (1, ones (2,2))
%!error statistics (1, 1.5)
%!error statistics (1, 0)
%!error statistics (1, 3)
%!error statistics (1)

