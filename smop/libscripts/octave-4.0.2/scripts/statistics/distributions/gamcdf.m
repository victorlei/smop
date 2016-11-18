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
## @deftypefn {Function File} {} gamcdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the Gamma distribution with shape parameter @var{a} and
## scale @var{b}.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: CDF of the Gamma distribution

function cdf = gamcdf (x, a, b)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (a) || ! isscalar (b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("gamcdf: X, A, and B must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("gamcdf: X, A, and B must not be complex");
  endif

  if (isa (x, "single") || isa (a, "single") || isa (b, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = isnan (x) | !(a > 0) | !(a < Inf) | !(b > 0) | !(b < Inf);
  cdf(k) = NaN;

  k = (x > 0) & (a > 0) & (a < Inf) & (b > 0) & (b < Inf);
  if (isscalar (a) && isscalar (b))
    cdf(k) = gammainc (x(k) / b, a);
  else
    cdf(k) = gammainc (x(k) ./ b(k), a(k));
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 2 Inf];
%! y = [0, gammainc(x(2:end), 1)];
%!assert (gamcdf (x, ones (1,6), ones (1,6)), y)
%!assert (gamcdf (x, 1, ones (1,6)), y)
%!assert (gamcdf (x, ones (1,6), 1), y)
%!assert (gamcdf (x, [0 -Inf NaN Inf 1 1], 1), [NaN NaN NaN NaN y(5:6)])
%!assert (gamcdf (x, 1, [0 -Inf NaN Inf 1 1]), [NaN NaN NaN NaN y(5:6)])
%!assert (gamcdf ([x(1:2) NaN x(4:6)], 1, 1), [y(1:2) NaN y(4:6)])

## Test class of input preserved
%!assert (gamcdf ([x, NaN], 1, 1), [y, NaN])
%!assert (gamcdf (single ([x, NaN]), 1, 1), single ([y, NaN]), eps ("single"))

## Test input validation
%!error gamcdf ()
%!error gamcdf (1)
%!error gamcdf (1,2)
%!error gamcdf (1,2,3,4)
%!error gamcdf (ones (3), ones (2), ones (2))
%!error gamcdf (ones (2), ones (3), ones (2))
%!error gamcdf (ones (2), ones (2), ones (3))
%!error gamcdf (i, 2, 2)
%!error gamcdf (2, i, 2)
%!error gamcdf (2, 2, i)

