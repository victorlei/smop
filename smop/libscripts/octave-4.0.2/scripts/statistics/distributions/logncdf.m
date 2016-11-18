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
## @deftypefn  {Function File} {} logncdf (@var{x})
## @deftypefnx {Function File} {} logncdf (@var{x}, @var{mu}, @var{sigma})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the lognormal distribution with parameters
## @var{mu} and @var{sigma}.
##
## If a random variable follows this distribution, its logarithm is normally
## distributed with mean @var{mu} and standard deviation @var{sigma}.
##
## Default values are @var{mu} = 0, @var{sigma} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the log normal distribution

function cdf = logncdf (x, mu = 0, sigma = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (mu) || ! isscalar (sigma))
    [retval, x, mu, sigma] = common_size (x, mu, sigma);
    if (retval > 0)
      error ("logncdf: X, MU, and SIGMA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (mu) || iscomplex (sigma))
    error ("logncdf: X, MU, and SIGMA must not be complex");
  endif

  if (isa (x, "single") || isa (mu, "single") || isa (sigma, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = isnan (x) | !(sigma > 0) | !(sigma < Inf);
  cdf(k) = NaN;

  k = (x == Inf) & (sigma > 0) & (sigma < Inf);
  cdf(k) = 1;

  k = (x > 0) & (x < Inf) & (sigma > 0) & (sigma < Inf);
  if (isscalar (mu) && isscalar (sigma))
    cdf(k) = stdnormal_cdf ((log (x(k)) - mu) / sigma);
  else
    cdf(k) = stdnormal_cdf ((log (x(k)) - mu(k)) ./ sigma(k));
  endif

endfunction


%!shared x,y
%! x = [-1 0 1 e Inf];
%! y = [0, 0, 0.5, 1/2+1/2*erf(1/2), 1];
%!assert (logncdf (x, zeros (1,5), sqrt(2)*ones (1,5)), y, eps)
%!assert (logncdf (x, 0, sqrt(2)*ones (1,5)), y, eps)
%!assert (logncdf (x, zeros (1,5), sqrt(2)), y, eps)
%!assert (logncdf (x, [0 1 NaN 0 1], sqrt(2)), [0 0 NaN y(4:5)], eps)
%!assert (logncdf (x, 0, sqrt(2)*[0 NaN Inf 1 1]), [NaN NaN NaN y(4:5)], eps)
%!assert (logncdf ([x(1:3) NaN x(5)], 0, sqrt(2)), [y(1:3) NaN y(5)], eps)

## Test class of input preserved
%!assert (logncdf ([x, NaN], 0, sqrt(2)), [y, NaN], eps)
%!assert (logncdf (single ([x, NaN]), 0, sqrt(2)), single ([y, NaN]), eps ("single"))
%!assert (logncdf ([x, NaN], single (0), sqrt(2)), single ([y, NaN]), eps ("single"))
%!assert (logncdf ([x, NaN], 0, single (sqrt(2))), single ([y, NaN]), eps ("single"))

## Test input validation
%!error logncdf ()
%!error logncdf (1,2)
%!error logncdf (1,2,3,4)
%!error logncdf (ones (3), ones (2), ones (2))
%!error logncdf (ones (2), ones (3), ones (2))
%!error logncdf (ones (2), ones (2), ones (3))
%!error logncdf (i, 2, 2)
%!error logncdf (2, i, 2)
%!error logncdf (2, 2, i)

