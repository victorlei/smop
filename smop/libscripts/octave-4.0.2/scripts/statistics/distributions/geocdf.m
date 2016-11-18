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
## @deftypefn {Function File} {} geocdf (@var{x}, @var{p})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the geometric distribution with parameter @var{p}.
##
## The geometric distribution models the number of failures (@var{x}-1) of a
## Bernoulli trial with probability @var{p} before the first success (@var{x}).
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the geometric distribution

function cdf = geocdf (x, p)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (p))
    [retval, x, p] = common_size (x, p);
    if (retval > 0)
      error ("geocdf: X and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (p))
    error ("geocdf: X and P must not be complex");
  endif

  if (isa (x, "single") || isa (p, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = isnan (x) | !(p >= 0) | !(p <= 1);
  cdf(k) = NaN;

  k = (x == Inf) & (p >= 0) & (p <= 1);
  cdf(k) = 1;

  k = (x >= 0) & (x < Inf) & (x == fix (x)) & (p > 0) & (p <= 1);
  if (isscalar (p))
    cdf(k) = 1 - ((1 - p) .^ (x(k) + 1));
  else
    cdf(k) = 1 - ((1 - p(k)) .^ (x(k) + 1));
  endif

endfunction


%!shared x,y
%! x = [-1 0 1 Inf];
%! y = [0 0.5 0.75 1];
%!assert (geocdf (x, 0.5*ones (1,4)), y)
%!assert (geocdf (x, 0.5), y)
%!assert (geocdf (x, 0.5*[-1 NaN 4 1]), [NaN NaN NaN y(4)])
%!assert (geocdf ([x(1:2) NaN x(4)], 0.5), [y(1:2) NaN y(4)])

## Test class of input preserved
%!assert (geocdf ([x, NaN], 0.5), [y, NaN])
%!assert (geocdf (single ([x, NaN]), 0.5), single ([y, NaN]))
%!assert (geocdf ([x, NaN], single (0.5)), single ([y, NaN]))

## Test input validation
%!error geocdf ()
%!error geocdf (1)
%!error geocdf (1,2,3)
%!error geocdf (ones (3), ones (2))
%!error geocdf (ones (2), ones (3))
%!error geocdf (i, 2)
%!error geocdf (2, i)

