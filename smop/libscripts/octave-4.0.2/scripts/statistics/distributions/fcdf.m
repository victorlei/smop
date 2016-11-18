## Copyright (C) 2012 Rik Wehbring
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
## @deftypefn {Function File} {} fcdf (@var{x}, @var{m}, @var{n})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the F distribution with @var{m} and @var{n} degrees of
## freedom.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the F distribution

function cdf = fcdf (x, m, n)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (m) || ! isscalar (n))
    [retval, x, m, n] = common_size (x, m, n);
    if (retval > 0)
      error ("fcdf: X, M, and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (m) || iscomplex (n))
    error ("fcdf: X, M, and N must not be complex");
  endif

  if (isa (x, "single") || isa (m, "single") || isa (n, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = isnan (x) | !(m > 0) | !(m < Inf) | !(n > 0) | !(n < Inf);
  cdf(k) = NaN;

  k = (x == Inf) & (m > 0) & (m < Inf) & (n > 0) & (n < Inf);
  cdf(k) = 1;

  k = (x > 0) & (x < Inf) & (m > 0) & (m < Inf) & (n > 0) & (n < Inf);
  if (isscalar (m) && isscalar (n))
    cdf(k) = 1 - betainc (1 ./ (1 + m * x(k) / n), n/2, m/2);
  else
    cdf(k) = 1 - betainc (1 ./ (1 + m(k) .* x(k) ./ n(k)), n(k)/2, m(k)/2);
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 2 Inf];
%! y = [0 0 1/3 1/2 2/3 1];
%!assert (fcdf (x, 2*ones (1,6), 2*ones (1,6)), y, eps)
%!assert (fcdf (x, 2, 2*ones (1,6)), y, eps)
%!assert (fcdf (x, 2*ones (1,6), 2), y, eps)
%!assert (fcdf (x, [0 NaN Inf 2 2 2], 2), [NaN NaN NaN y(4:6)], eps)
%!assert (fcdf (x, 2, [0 NaN Inf 2 2 2]), [NaN NaN NaN y(4:6)], eps)
%!assert (fcdf ([x(1:2) NaN x(4:6)], 2, 2), [y(1:2) NaN y(4:6)], eps)

## Test class of input preserved
%!assert (fcdf ([x, NaN], 2, 2), [y, NaN], eps)
%!assert (fcdf (single ([x, NaN]), 2, 2), single ([y, NaN]), eps ("single"))
%!assert (fcdf ([x, NaN], single (2), 2), single ([y, NaN]), eps ("single"))
%!assert (fcdf ([x, NaN], 2, single (2)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error fcdf ()
%!error fcdf (1)
%!error fcdf (1,2)
%!error fcdf (1,2,3,4)
%!error fcdf (ones (3), ones (2), ones (2))
%!error fcdf (ones (2), ones (3), ones (2))
%!error fcdf (ones (2), ones (2), ones (3))
%!error fcdf (i, 2, 2)
%!error fcdf (2, i, 2)
%!error fcdf (2, 2, i)

