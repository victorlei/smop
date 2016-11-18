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
## @deftypefn  {Function File} {} lognpdf (@var{x})
## @deftypefnx {Function File} {} lognpdf (@var{x}, @var{mu}, @var{sigma})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the lognormal distribution with parameters
## @var{mu} and @var{sigma}.
##
## If a random variable follows this distribution, its logarithm is normally
## distributed with mean @var{mu} and standard deviation @var{sigma}.
##
## Default values are @var{mu} = 0, @var{sigma} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the log normal distribution

function pdf = lognpdf (x, mu = 0, sigma = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (mu) || ! isscalar (sigma))
    [retval, x, mu, sigma] = common_size (x, mu, sigma);
    if (retval > 0)
      error ("lognpdf: X, MU, and SIGMA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (mu) || iscomplex (sigma))
    error ("lognpdf: X, MU, and SIGMA must not be complex");
  endif

  if (isa (x, "single") || isa (mu, "single") || isa (sigma, "single"))
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = isnan (x) | !(sigma > 0) | !(sigma < Inf);
  pdf(k) = NaN;

  k = (x > 0) & (x < Inf) & (sigma > 0) & (sigma < Inf);
  if (isscalar (mu) && isscalar (sigma))
    pdf(k) = normpdf (log (x(k)), mu, sigma) ./ x(k);
  else
    pdf(k) = normpdf (log (x(k)), mu(k), sigma(k)) ./ x(k);
  endif

endfunction


%!shared x,y
%! x = [-1 0 e Inf];
%! y = [0, 0, 1/(e*sqrt(2*pi)) * exp(-1/2), 0];
%!assert (lognpdf (x, zeros (1,4), ones (1,4)), y, eps)
%!assert (lognpdf (x, 0, ones (1,4)), y, eps)
%!assert (lognpdf (x, zeros (1,4), 1), y, eps)
%!assert (lognpdf (x, [0 1 NaN 0], 1), [0 0 NaN y(4)], eps)
%!assert (lognpdf (x, 0, [0 NaN Inf 1]), [NaN NaN NaN y(4)], eps)
%!assert (lognpdf ([x, NaN], 0, 1), [y, NaN], eps)

## Test class of input preserved
%!assert (lognpdf (single ([x, NaN]), 0, 1), single ([y, NaN]), eps ("single"))
%!assert (lognpdf ([x, NaN], single (0), 1), single ([y, NaN]), eps ("single"))
%!assert (lognpdf ([x, NaN], 0, single (1)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error lognpdf ()
%!error lognpdf (1,2)
%!error lognpdf (1,2,3,4)
%!error lognpdf (ones (3), ones (2), ones (2))
%!error lognpdf (ones (2), ones (3), ones (2))
%!error lognpdf (ones (2), ones (2), ones (3))
%!error lognpdf (i, 2, 2)
%!error lognpdf (2, i, 2)
%!error lognpdf (2, 2, i)

