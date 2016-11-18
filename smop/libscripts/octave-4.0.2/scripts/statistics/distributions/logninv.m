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
## @deftypefn  {Function File} {} logninv (@var{x})
## @deftypefnx {Function File} {} logninv (@var{x}, @var{mu}, @var{sigma})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the lognormal distribution with parameters
## @var{mu} and @var{sigma}.
##
## If a random variable follows this distribution, its logarithm is normally
## distributed with mean @var{mu} and standard deviation @var{sigma}.
##
## Default values are @var{mu} = 0, @var{sigma} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the log normal distribution

function inv = logninv (x, mu = 0, sigma = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (mu) || ! isscalar (sigma))
    [retval, x, mu, sigma] = common_size (x, mu, sigma);
    if (retval > 0)
      error ("logninv: X, MU, and SIGMA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (mu) || iscomplex (sigma))
    error ("logninv: X, MU, and SIGMA must not be complex");
  endif

  if (isa (x, "single") || isa (mu, "single") || isa (sigma, "single"))
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  k = !(x >= 0) | !(x <= 1) | !(sigma > 0) | !(sigma < Inf);
  inv(k) = NaN;

  k = (x == 1) & (sigma > 0) & (sigma < Inf);
  inv(k) = Inf;

  k = (x >= 0) & (x < 1) & (sigma > 0) & (sigma < Inf);
  if (isscalar (mu) && isscalar (sigma))
    inv(k) = exp (mu) .* exp (sigma .* stdnormal_inv (x(k)));
  else
    inv(k) = exp (mu(k)) .* exp (sigma(k) .* stdnormal_inv (x(k)));
  endif

endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (logninv (x, ones (1,5), ones (1,5)), [NaN 0 e Inf NaN])
%!assert (logninv (x, 1, ones (1,5)), [NaN 0 e Inf NaN])
%!assert (logninv (x, ones (1,5), 1), [NaN 0 e Inf NaN])
%!assert (logninv (x, [1 1 NaN 0 1], 1), [NaN 0 NaN Inf NaN])
%!assert (logninv (x, 1, [1 0 NaN Inf 1]), [NaN NaN NaN NaN NaN])
%!assert (logninv ([x(1:2) NaN x(4:5)], 1, 2), [NaN 0 NaN Inf NaN])

## Test class of input preserved
%!assert (logninv ([x, NaN], 1, 1), [NaN 0 e Inf NaN NaN])
%!assert (logninv (single ([x, NaN]), 1, 1), single ([NaN 0 e Inf NaN NaN]))
%!assert (logninv ([x, NaN], single (1), 1), single ([NaN 0 e Inf NaN NaN]))
%!assert (logninv ([x, NaN], 1, single (1)), single ([NaN 0 e Inf NaN NaN]))

## Test input validation
%!error logninv ()
%!error logninv (1,2)
%!error logninv (1,2,3,4)
%!error logninv (ones (3), ones (2), ones (2))
%!error logninv (ones (2), ones (3), ones (2))
%!error logninv (ones (2), ones (2), ones (3))
%!error logninv (i, 2, 2)
%!error logninv (2, i, 2)
%!error logninv (2, 2, i)

