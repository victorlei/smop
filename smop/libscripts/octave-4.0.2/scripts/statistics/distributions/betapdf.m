## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 1995-2015 Kurt Hornik
## Copyright (C) 2010 Christos Dimitrakakis
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
## @deftypefn {Function File} {} betapdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the Beta distribution with parameters @var{a} and @var{b}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>, CD <christos.dimitrakakis@gmail.com>
## Description: PDF of the Beta distribution

function pdf = betapdf (x, a, b)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (a) || ! isscalar (b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("betapdf: X, A, and B must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("betapdf: X, A, and B must not be complex");
  endif

  if (isa (x, "single") || isa (a, "single") || isa (b, "single"));
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = !(a > 0) | !(b > 0) | isnan (x);
  pdf(k) = NaN;

  k = (x > 0) & (x < 1) & (a > 0) & (b > 0) & ((a != 1) | (b != 1));
  if (isscalar (a) && isscalar (b))
    pdf(k) = exp ((a - 1) * log (x(k))
                  + (b - 1) * log (1 - x(k))
                  + gammaln (a + b) - gammaln (a) - gammaln (b));
  else
    pdf(k) = exp ((a(k) - 1) .* log (x(k))
                  + (b(k) - 1) .* log (1 - x(k))
                  + gammaln (a(k) + b(k)) - gammaln (a(k)) - gammaln (b(k)));
  endif

  ## Most important special cases when the density is finite.
  k = (x == 0) & (a == 1) & (b > 0) & (b != 1);
  if (isscalar (a) && isscalar (b))
    pdf(k) = exp (gammaln (a + b) - gammaln (a) - gammaln (b));
  else
    pdf(k) = exp (gammaln (a(k) + b(k)) - gammaln (a(k)) - gammaln (b(k)));
  endif

  k = (x == 1) & (b == 1) & (a > 0) & (a != 1);
  if (isscalar (a) && isscalar (b))
    pdf(k) = exp (gammaln (a + b) - gammaln (a) - gammaln (b));
  else
    pdf(k) = exp (gammaln (a(k) + b(k)) - gammaln (a(k)) - gammaln (b(k)));
  endif

  k = (x >= 0) & (x <= 1) & (a == 1) & (b == 1);
  pdf(k) = 1;

  ## Other special case when the density at the boundary is infinite.
  k = (x == 0) & (a < 1);
  pdf(k) = Inf;

  k = (x == 1) & (b < 1);
  pdf(k) = Inf;

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 2];
%! y = [0 2 1 0 0];
%!assert (betapdf (x, ones (1,5), 2*ones (1,5)), y)
%!assert (betapdf (x, 1, 2*ones (1,5)), y)
%!assert (betapdf (x, ones (1,5), 2), y)
%!assert (betapdf (x, [0 NaN 1 1 1], 2), [NaN NaN y(3:5)])
%!assert (betapdf (x, 1, 2*[0 NaN 1 1 1]), [NaN NaN y(3:5)])
%!assert (betapdf ([x, NaN], 1, 2), [y, NaN])

## Test class of input preserved
%!assert (betapdf (single ([x, NaN]), 1, 2), single ([y, NaN]))
%!assert (betapdf ([x, NaN], single (1), 2), single ([y, NaN]))
%!assert (betapdf ([x, NaN], 1, single (2)), single ([y, NaN]))

## Beta (1/2,1/2) == arcsine distribution
%!test
%! x = rand (10,1);
%! y = 1./(pi * sqrt (x.*(1-x)));
%! assert (betapdf (x, 1/2, 1/2), y, 50*eps);

## Test large input values to betapdf
%!assert (betapdf (0.5, 1000, 1000), 35.678, 1e-3)

## Test input validation
%!error betapdf ()
%!error betapdf (1)
%!error betapdf (1,2)
%!error betapdf (1,2,3,4)
%!error betapdf (ones (3), ones (2), ones (2))
%!error betapdf (ones (2), ones (3), ones (2))
%!error betapdf (ones (2), ones (2), ones (3))
%!error betapdf (i, 2, 2)
%!error betapdf (2, i, 2)
%!error betapdf (2, 2, i)

