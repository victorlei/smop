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
## @deftypefn {Function File} {} expinv (@var{x}, @var{lambda})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the exponential distribution with mean @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the exponential distribution

function inv = expinv (x, lambda)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (lambda))
    [retval, x, lambda] = common_size (x, lambda);
    if (retval > 0)
      error ("expinv: X and LAMBDA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (lambda))
    error ("expinv: X and LAMBDA must not be complex");
  endif

  if (! isscalar (x))
    sz = size (x);
  else
    sz = size (lambda);
  endif

  if (iscomplex (x) || iscomplex (lambda))
    error ("expinv: X and LAMBDA must not be complex");
  endif

  if (isa (x, "single") || isa (lambda, "single"))
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  k = (x == 1) & (lambda > 0);
  inv(k) = Inf;

  k = (x >= 0) & (x < 1) & (lambda > 0);
  if (isscalar (lambda))
    inv(k) = - lambda * log (1 - x(k));
  else
    inv(k) = - lambda(k) .* log (1 - x(k));
  endif

endfunction


%!shared x
%! x = [-1 0 0.3934693402873666 1 2];
%!assert (expinv (x, 2*ones (1,5)), [NaN 0 1 Inf NaN], eps)
%!assert (expinv (x, 2), [NaN 0 1 Inf NaN], eps)
%!assert (expinv (x, 2*[1 0 NaN 1 1]), [NaN NaN NaN Inf NaN], eps)
%!assert (expinv ([x(1:2) NaN x(4:5)], 2), [NaN 0 NaN Inf NaN], eps)

## Test class of input preserved
%!assert (expinv ([x, NaN], 2), [NaN 0 1 Inf NaN NaN], eps)
%!assert (expinv (single ([x, NaN]), 2), single ([NaN 0 1 Inf NaN NaN]), eps)
%!assert (expinv ([x, NaN], single (2)), single ([NaN 0 1 Inf NaN NaN]), eps)

## Test input validation
%!error expinv ()
%!error expinv (1)
%!error expinv (1,2,3)
%!error expinv (ones (3), ones (2))
%!error expinv (ones (2), ones (3))
%!error expinv (i, 2)
%!error expinv (2, i)

