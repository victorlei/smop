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
## @deftypefn {Function File} {} poissinv (@var{x}, @var{lambda})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the Poisson distribution with parameter @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Poisson distribution

function inv = poissinv (x, lambda)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (lambda))
    [retval, x, lambda] = common_size (x, lambda);
    if (retval > 0)
      error ("poissinv: X and LAMBDA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (lambda))
    error ("poissinv: X and LAMBDA must not be complex");
  endif

  if (isa (x, "single") || isa (lambda, "single"))
    inv = zeros (size (x), "single");
  else
    inv = zeros (size (x));
  endif

  k = (x < 0) | (x > 1) | isnan (x) | !(lambda > 0);
  inv(k) = NaN;

  k = (x == 1) & (lambda > 0);
  inv(k) = Inf;

  k = find ((x > 0) & (x < 1) & (lambda > 0));
  if (isscalar (lambda))
    cdf = exp (-lambda) * ones (size (k));
  else
    cdf = exp (-lambda(k));
  endif

  while (1)
    m = find (cdf < x(k));
    if (any (m))
      inv(k(m)) += 1;
      if (isscalar (lambda))
        cdf(m) = cdf(m) + poisspdf (inv(k(m)), lambda);
      else
        cdf(m) = cdf(m) + poisspdf (inv(k(m)), lambda(k(m)));
      endif
    else
      break;
    endif
  endwhile

endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (poissinv (x, ones (1,5)), [NaN 0 1 Inf NaN])
%!assert (poissinv (x, 1), [NaN 0 1 Inf NaN])
%!assert (poissinv (x, [1 0 NaN 1 1]), [NaN NaN NaN Inf NaN])
%!assert (poissinv ([x(1:2) NaN x(4:5)], 1), [NaN 0 NaN Inf NaN])

## Test class of input preserved
%!assert (poissinv ([x, NaN], 1), [NaN 0 1 Inf NaN NaN])
%!assert (poissinv (single ([x, NaN]), 1), single ([NaN 0 1 Inf NaN NaN]))
%!assert (poissinv ([x, NaN], single (1)), single ([NaN 0 1 Inf NaN NaN]))

## Test input validation
%!error poissinv ()
%!error poissinv (1)
%!error poissinv (1,2,3)
%!error poissinv (ones (3), ones (2))
%!error poissinv (ones (2), ones (3))
%!error poissinv (i, 2)
%!error poissinv (2, i)

