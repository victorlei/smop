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
## @deftypefn {Function File} {} exppdf (@var{x}, @var{lambda})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the exponential distribution with mean @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the exponential distribution

function pdf = exppdf (x, lambda)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (lambda))
    [retval, x, lambda] = common_size (x, lambda);
    if (retval > 0)
      error ("exppdf: X and LAMBDA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (lambda))
    error ("exppdf: X and LAMBDA must not be complex");
  endif

  if (isa (x, "single") || isa (lambda, "single"))
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = isnan (x) | !(lambda > 0);
  pdf(k) = NaN;

  k = (x >= 0) & (x < Inf) & (lambda > 0);
  if (isscalar (lambda))
    pdf(k) = exp (-x(k) / lambda) / lambda;
  else
    pdf(k) = exp (-x(k) ./ lambda(k)) ./ lambda(k);
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 Inf];
%! y = gampdf (x, 1, 2);
%!assert (exppdf (x, 2*ones (1,5)), y)
%!assert (exppdf (x, 2*[1 0 NaN 1 1]), [y(1) NaN NaN y(4:5)])
%!assert (exppdf ([x, NaN], 2), [y, NaN])

## Test class of input preserved
%!assert (exppdf (single ([x, NaN]), 2), single ([y, NaN]))
%!assert (exppdf ([x, NaN], single (2)), single ([y, NaN]))

## Test input validation
%!error exppdf ()
%!error exppdf (1)
%!error exppdf (1,2,3)
%!error exppdf (ones (3), ones (2))
%!error exppdf (ones (2), ones (3))
%!error exppdf (i, 2)
%!error exppdf (2, i)

