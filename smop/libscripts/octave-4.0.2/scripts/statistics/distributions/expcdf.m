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
## @deftypefn {Function File} {} expcdf (@var{x}, @var{lambda})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the exponential distribution with mean @var{lambda}.
##
## The arguments can be of common size or scalars.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the exponential distribution

function cdf = expcdf (x, lambda)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (lambda))
    [retval, x, lambda] = common_size (x, lambda);
    if (retval > 0)
      error ("expcdf: X and LAMBDA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (lambda))
    error ("expcdf: X and LAMBDA must not be complex");
  endif

  if (isa (x, "single") || isa (lambda, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = isnan (x) | !(lambda > 0);
  cdf(k) = NaN;

  k = (x == Inf) & (lambda > 0);
  cdf(k) = 1;

  k = (x > 0) & (x < Inf) & (lambda > 0);
  if (isscalar (lambda))
    cdf(k) = 1 - exp (-x(k) / lambda);
  else
    cdf(k) = 1 - exp (-x(k) ./ lambda(k));
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 Inf];
%! y = [0, 1 - exp(-x(2:end)/2)];
%!assert (expcdf (x, 2*ones (1,5)), y)
%!assert (expcdf (x, 2), y)
%!assert (expcdf (x, 2*[1 0 NaN 1 1]), [y(1) NaN NaN y(4:5)])

## Test class of input preserved
%!assert (expcdf ([x, NaN], 2), [y, NaN])
%!assert (expcdf (single ([x, NaN]), 2), single ([y, NaN]))
%!assert (expcdf ([x, NaN], single (2)), single ([y, NaN]))

## Test input validation
%!error expcdf ()
%!error expcdf (1)
%!error expcdf (1,2,3)
%!error expcdf (ones (3), ones (2))
%!error expcdf (ones (2), ones (3))
%!error expcdf (i, 2)
%!error expcdf (2, i)

