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
## @deftypefn {Function File} {} geopdf (@var{x}, @var{p})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the geometric distribution with parameter @var{p}.
##
## The geometric distribution models the number of failures (@var{x}-1) of a
## Bernoulli trial with probability @var{p} before the first success (@var{x}).
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the geometric distribution

function pdf = geopdf (x, p)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (p))
    [retval, x, p] = common_size (x, p);
    if (retval > 0)
      error ("geopdf: X and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (p))
    error ("geopdf: X and P must not be complex");
  endif

  if (isa (x, "single") || isa (p, "single"))
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = isnan (x) | (x == Inf) | !(p >= 0) | !(p <= 1);
  pdf(k) = NaN;

  k = (x >= 0) & (x < Inf) & (x == fix (x)) & (p > 0) & (p <= 1);
  if (isscalar (p))
    pdf(k) = p * ((1 - p) .^ x(k));
  else
    pdf(k) = p(k) .* ((1 - p(k)) .^ x(k));
  endif

endfunction


%!shared x,y
%! x = [-1 0 1 Inf];
%! y = [0, 1/2, 1/4, NaN];
%!assert (geopdf (x, 0.5*ones (1,4)), y)
%!assert (geopdf (x, 0.5), y)
%!assert (geopdf (x, 0.5*[-1 NaN 4 1]), [NaN NaN NaN y(4)])
%!assert (geopdf ([x, NaN], 0.5), [y, NaN])

## Test class of input preserved
%!assert (geopdf (single ([x, NaN]), 0.5), single ([y, NaN]), 5*eps ("single"))
%!assert (geopdf ([x, NaN], single (0.5)), single ([y, NaN]), 5*eps ("single"))

## Test input validation
%!error geopdf ()
%!error geopdf (1)
%!error geopdf (1,2,3)
%!error geopdf (ones (3), ones (2))
%!error geopdf (ones (2), ones (3))
%!error geopdf (i, 2)
%!error geopdf (2, i)

