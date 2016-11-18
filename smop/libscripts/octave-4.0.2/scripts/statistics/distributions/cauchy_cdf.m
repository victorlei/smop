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
## @deftypefn  {Function File} {} cauchy_cdf (@var{x})
## @deftypefnx {Function File} {} cauchy_cdf (@var{x}, @var{location}, @var{scale})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the Cauchy distribution with location parameter
## @var{location} and scale parameter @var{scale}.
##
## Default values are @var{location} = 0, @var{scale} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Cauchy distribution

function cdf = cauchy_cdf (x, location = 0, scale = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (location) || ! isscalar (scale))
    [retval, x, location, scale] = common_size (x, location, scale);
    if (retval > 0)
      error ("cauchy_cdf: X, LOCATION, and SCALE must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (location) || iscomplex (scale))
    error ("cauchy_cdf: X, LOCATION, and SCALE must not be complex");
  endif

  if (isa (x, "single") || isa (location, "single") || isa (scale, "single"));
    cdf = NaN (size (x), "single");
  else
    cdf = NaN (size (x));
  endif

  k = ! isinf (location) & (scale > 0) & (scale < Inf);
  if (isscalar (location) && isscalar (scale))
    cdf = 0.5 + atan ((x - location) / scale) / pi;
  else
    cdf(k) = 0.5 + atan ((x(k) - location(k)) ./ scale(k)) / pi;
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 2];
%! y = 1/pi * atan ((x-1) / 2) + 1/2;
%!assert (cauchy_cdf (x, ones (1,5), 2*ones (1,5)), y)
%!assert (cauchy_cdf (x, 1, 2*ones (1,5)), y)
%!assert (cauchy_cdf (x, ones (1,5), 2), y)
%!assert (cauchy_cdf (x, [-Inf 1 NaN 1 Inf], 2), [NaN y(2) NaN y(4) NaN])
%!assert (cauchy_cdf (x, 1, 2*[0 1 NaN 1 Inf]), [NaN y(2) NaN y(4) NaN])
%!assert (cauchy_cdf ([x(1:2) NaN x(4:5)], 1, 2), [y(1:2) NaN y(4:5)])

## Test class of input preserved
%!assert (cauchy_cdf ([x, NaN], 1, 2), [y, NaN])
%!assert (cauchy_cdf (single ([x, NaN]), 1, 2), single ([y, NaN]), eps ("single"))
%!assert (cauchy_cdf ([x, NaN], single (1), 2), single ([y, NaN]), eps ("single"))
%!assert (cauchy_cdf ([x, NaN], 1, single (2)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error cauchy_cdf ()
%!error cauchy_cdf (1,2)
%!error cauchy_cdf (1,2,3,4)
%!error cauchy_cdf (ones (3), ones (2), ones (2))
%!error cauchy_cdf (ones (2), ones (3), ones (2))
%!error cauchy_cdf (ones (2), ones (2), ones (3))
%!error cauchy_cdf (i, 2, 2)
%!error cauchy_cdf (2, i, 2)
%!error cauchy_cdf (2, 2, i)

