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
## @deftypefn {Function File} {} poisscdf (@var{x}, @var{lambda})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the Poisson distribution with parameter @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Poisson distribution

function cdf = poisscdf (x, lambda)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (lambda))
    [retval, x, lambda] = common_size (x, lambda);
    if (retval > 0)
      error ("poisscdf: X and LAMBDA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (lambda))
    error ("poisscdf: X and LAMBDA must not be complex");
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

  k = (x >= 0) & (x < Inf) & (lambda > 0);
  if (isscalar (lambda))
    cdf(k) = 1 - gammainc (lambda, floor (x(k)) + 1);
  else
    cdf(k) = 1 - gammainc (lambda(k), floor (x(k)) + 1);
  endif

endfunction


%!shared x,y
%! x = [-1 0 1 2 Inf];
%! y = [0, gammainc(1, (x(2:4) +1), "upper"), 1];
%!assert (poisscdf (x, ones (1,5)), y)
%!assert (poisscdf (x, 1), y)
%!assert (poisscdf (x, [1 0 NaN 1 1]), [y(1) NaN NaN y(4:5)])
%!assert (poisscdf ([x(1:2) NaN Inf x(5)], 1), [y(1:2) NaN 1 y(5)])

## Test class of input preserved
%!assert (poisscdf ([x, NaN], 1), [y, NaN])
%!assert (poisscdf (single ([x, NaN]), 1), single ([y, NaN]), eps ("single"))
%!assert (poisscdf ([x, NaN], single (1)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error poisscdf ()
%!error poisscdf (1)
%!error poisscdf (1,2,3)
%!error poisscdf (ones (3), ones (2))
%!error poisscdf (ones (2), ones (3))
%!error poisscdf (i, 2)
%!error poisscdf (2, i)

