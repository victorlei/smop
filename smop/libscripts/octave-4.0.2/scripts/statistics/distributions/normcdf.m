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
## @deftypefn  {Function File} {} normcdf (@var{x})
## @deftypefnx {Function File} {} normcdf (@var{x}, @var{mu}, @var{sigma})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the normal distribution with mean @var{mu} and
## standard deviation @var{sigma}.
##
## Default values are @var{mu} = 0, @var{sigma} = 1.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: CDF of the normal distribution

function cdf = normcdf (x, mu = 0, sigma = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (mu) || ! isscalar (sigma))
    [retval, x, mu, sigma] = common_size (x, mu, sigma);
    if (retval > 0)
      error ("normcdf: X, MU, and SIGMA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (mu) || iscomplex (sigma))
    error ("normcdf: X, MU, and SIGMA must not be complex");
  endif

  if (isa (x, "single") || isa (mu, "single") || isa (sigma, "single"));
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  if (isscalar (mu) && isscalar (sigma))
    if (isfinite (mu) && (sigma > 0) && (sigma < Inf))
      cdf = stdnormal_cdf ((x - mu) / sigma);
    else
      cdf = NaN (size (x), class (cdf));
    endif
  else
    k = ! isfinite (mu) | !(sigma > 0) | !(sigma < Inf);
    cdf(k) = NaN;

    k = ! k;
    cdf(k) = stdnormal_cdf ((x(k) - mu(k)) ./ sigma(k));
  endif

endfunction


%!shared x,y
%! x = [-Inf 1 2 Inf];
%! y = [0, 0.5, 1/2*(1+erf(1/sqrt(2))), 1];
%!assert (normcdf (x, ones (1,4), ones (1,4)), y)
%!assert (normcdf (x, 1, ones (1,4)), y)
%!assert (normcdf (x, ones (1,4), 1), y)
%!assert (normcdf (x, [0 -Inf NaN Inf], 1), [y(1) NaN NaN NaN])
%!assert (normcdf (x, 1, [Inf NaN -1 0]), [NaN NaN NaN NaN])
%!assert (normcdf ([x(1:2) NaN x(4)], 1, 1), [y(1:2) NaN y(4)])

## Test class of input preserved
%!assert (normcdf ([x, NaN], 1, 1), [y, NaN])
%!assert (normcdf (single ([x, NaN]), 1, 1), single ([y, NaN]), eps ("single"))
%!assert (normcdf ([x, NaN], single (1), 1), single ([y, NaN]), eps ("single"))
%!assert (normcdf ([x, NaN], 1, single (1)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error normcdf ()
%!error normcdf (1,2)
%!error normcdf (1,2,3,4)
%!error normcdf (ones (3), ones (2), ones (2))
%!error normcdf (ones (2), ones (3), ones (2))
%!error normcdf (ones (2), ones (2), ones (3))
%!error normcdf (i, 2, 2)
%!error normcdf (2, i, 2)
%!error normcdf (2, 2, i)

