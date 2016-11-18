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
## @deftypefn {Function File} {} poisspdf (@var{x}, @var{lambda})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the Poisson distribution with parameter @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the Poisson distribution

function pdf = poisspdf (x, lambda)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (lambda))
    [retval, x, lambda] = common_size (x, lambda);
    if (retval > 0)
      error ("poisspdf: X and LAMBDA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (lambda))
    error ("poisspdf: X and LAMBDA must not be complex");
  endif

  if (isa (x, "single") || isa (lambda, "single"))
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = isnan (x) | !(lambda > 0);
  pdf(k) = NaN;

  k = (x >= 0) & (x < Inf) & (x == fix (x)) & (lambda > 0);
  if (isscalar (lambda))
    pdf(k) = exp (x(k) * log (lambda) - lambda - gammaln (x(k) + 1));
  else
    pdf(k) = exp (x(k) .* log (lambda(k)) - lambda(k) - gammaln (x(k) + 1));
  endif

endfunction


%!shared x,y
%! x = [-1 0 1 2 Inf];
%! y = [0, exp(-1)*[1 1 0.5], 0];
%!assert (poisspdf (x, ones (1,5)), y, eps)
%!assert (poisspdf (x, 1), y, eps)
%!assert (poisspdf (x, [1 0 NaN 1 1]), [y(1) NaN NaN y(4:5)], eps)
%!assert (poisspdf ([x, NaN], 1), [y, NaN], eps)

## Test class of input preserved
%!assert (poisspdf (single ([x, NaN]), 1), single ([y, NaN]), eps ("single"))
%!assert (poisspdf ([x, NaN], single (1)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error poisspdf ()
%!error poisspdf (1)
%!error poisspdf (1,2,3)
%!error poisspdf (ones (3), ones (2))
%!error poisspdf (ones (2), ones (3))
%!error poisspdf (i, 2)
%!error poisspdf (2, i)

