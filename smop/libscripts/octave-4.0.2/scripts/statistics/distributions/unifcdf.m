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
## @deftypefn  {Function File} {} unifcdf (@var{x})
## @deftypefnx {Function File} {} unifcdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the uniform distribution on the interval
## [@var{a}, @var{b}].
##
## Default values are @var{a} = 0, @var{b} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the uniform distribution

function cdf = unifcdf (x, a = 0, b = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (a) || ! isscalar (b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("unifcdf: X, A, and B must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("unifcdf: X, A, and B must not be complex");
  endif

  if (isa (x, "single") || isa (a, "single") || isa (b, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = isnan (x) | !(a < b);
  cdf(k) = NaN;

  k = (x >= b) & (a < b);
  cdf(k) = 1;

  k = (x > a) & (x < b);
  if (isscalar (a) && isscalar (b))
    cdf(k) = (x(k) < b) .* (x(k) - a) / (b - a);
  else
    cdf(k) = (x(k) < b(k)) .* (x(k) - a(k)) ./ (b(k) - a(k));
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 2] + 1;
%! y = [0 0 0.5 1 1];
%!assert (unifcdf (x, ones (1,5), 2*ones (1,5)), y)
%!assert (unifcdf (x, 1, 2*ones (1,5)), y)
%!assert (unifcdf (x, ones (1,5), 2), y)
%!assert (unifcdf (x, [2 1 NaN 1 1], 2), [NaN 0 NaN 1 1])
%!assert (unifcdf (x, 1, 2*[0 1 NaN 1 1]), [NaN 0 NaN 1 1])
%!assert (unifcdf ([x(1:2) NaN x(4:5)], 1, 2), [y(1:2) NaN y(4:5)])

## Test class of input preserved
%!assert (unifcdf ([x, NaN], 1, 2), [y, NaN])
%!assert (unifcdf (single ([x, NaN]), 1, 2), single ([y, NaN]))
%!assert (unifcdf ([x, NaN], single (1), 2), single ([y, NaN]))
%!assert (unifcdf ([x, NaN], 1, single (2)), single ([y, NaN]))

## Test input validation
%!error unifcdf ()
%!error unifcdf (1,2)
%!error unifcdf (1,2,3,4)
%!error unifcdf (ones (3), ones (2), ones (2))
%!error unifcdf (ones (2), ones (3), ones (2))
%!error unifcdf (ones (2), ones (2), ones (3))
%!error unifcdf (i, 2, 2)
%!error unifcdf (2, i, 2)
%!error unifcdf (2, 2, i)

