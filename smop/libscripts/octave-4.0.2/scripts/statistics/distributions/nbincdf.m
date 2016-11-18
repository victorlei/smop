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
## @deftypefn {Function File} {} nbincdf (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the negative binomial distribution with parameters
## @var{n} and @var{p}.
##
## When @var{n} is integer this is the Pascal distribution.
## When @var{n} is extended to real numbers this is the Polya distribution.
##
## The number of failures in a Bernoulli experiment with success probability
## @var{p} before the @var{n}-th success follows this distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Pascal (negative binomial) distribution

function cdf = nbincdf (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (n) || ! isscalar (p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("nbincdf: X, N, and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n) || iscomplex (p))
    error ("nbincdf: X, N, and P must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single") || isa (p, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = (isnan (x) | isnan (n) | (n < 1) | (n == Inf)
       | (p < 0) | (p > 1) | isnan (p));
  cdf(k) = NaN;

  k = (x == Inf) & (n > 0) & (n < Inf) & (p >= 0) & (p <= 1);
  cdf(k) = 1;

  k = ((x >= 0) & (x < Inf) & (x == fix (x))
       & (n > 0) & (n < Inf) & (p > 0) & (p <= 1));
  if (isscalar (n) && isscalar (p))
    cdf(k) = 1 - betainc (1-p, x(k)+1, n);
  else
    cdf(k) = 1 - betainc (1-p(k), x(k)+1, n(k));
  endif

endfunction


%!shared x,y
%! x = [-1 0 1 2 Inf];
%! y = [0 1/2 3/4 7/8 1];
%!assert (nbincdf (x, ones (1,5), 0.5*ones (1,5)), y)
%!assert (nbincdf (x, 1, 0.5*ones (1,5)), y)
%!assert (nbincdf (x, ones (1,5), 0.5), y)
%!assert (nbincdf ([x(1:3) 0 x(5)], [0 1 NaN 1.5 Inf], 0.5), [NaN 1/2 NaN nbinpdf(0,1.5,0.5) NaN], eps)
%!assert (nbincdf (x, 1, 0.5*[-1 NaN 4 1 1]), [NaN NaN NaN y(4:5)])
%!assert (nbincdf ([x(1:2) NaN x(4:5)], 1, 0.5), [y(1:2) NaN y(4:5)])

## Test class of input preserved
%!assert (nbincdf ([x, NaN], 1, 0.5), [y, NaN])
%!assert (nbincdf (single ([x, NaN]), 1, 0.5), single ([y, NaN]))
%!assert (nbincdf ([x, NaN], single (1), 0.5), single ([y, NaN]))
%!assert (nbincdf ([x, NaN], 1, single (0.5)), single ([y, NaN]))

## Test input validation
%!error nbincdf ()
%!error nbincdf (1)
%!error nbincdf (1,2)
%!error nbincdf (1,2,3,4)
%!error nbincdf (ones (3), ones (2), ones (2))
%!error nbincdf (ones (2), ones (3), ones (2))
%!error nbincdf (ones (2), ones (2), ones (3))
%!error nbincdf (i, 2, 2)
%!error nbincdf (2, i, 2)
%!error nbincdf (2, 2, i)

