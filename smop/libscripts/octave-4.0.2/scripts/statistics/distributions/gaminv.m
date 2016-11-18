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
## @deftypefn {Function File} {} gaminv (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the Gamma distribution with shape parameter @var{a} and
## scale @var{b}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Gamma distribution

function inv = gaminv (x, a, b)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (a) || ! isscalar (b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("gaminv: X, A, and B must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("gaminv: X, A, and B must not be complex");
  endif

  if (isa (x, "single") || isa (a, "single") || isa (b, "single"))
    inv = zeros (size (x), "single");
  else
    inv = zeros (size (x));
  endif

  k = ((x < 0) | (x > 1) | isnan (x)
       | !(a > 0) | !(a < Inf) | !(b > 0) | !(b < Inf));
  inv(k) = NaN;

  k = (x == 1) & (a > 0) & (a < Inf) & (b > 0) & (b < Inf);
  inv(k) = Inf;

  k = find ((x > 0) & (x < 1) & (a > 0) & (a < Inf) & (b > 0) & (b < Inf));
  if (any (k))
    if (! isscalar (a) || ! isscalar (b))
      a = a(k);
      b = b(k);
      y = a .* b;
    else
      y = a * b * ones (size (k));
    endif
    x = x(k);

    if (isa (x, "single"))
      myeps = eps ("single");
    else
      myeps = eps;
    endif

    l = find (x < myeps);
    if (any (l))
      y(l) = sqrt (myeps) * ones (length (l), 1);
    endif

    y_old = y;
    for i = 1 : 100
      h     = (gamcdf (y_old, a, b) - x) ./ gampdf (y_old, a, b);
      y_new = y_old - h;
      ind   = find (y_new <= myeps);
      if (any (ind))
        y_new(ind) = y_old(ind) / 10;
        h = y_old - y_new;
      endif
      if (max (abs (h)) < sqrt (myeps))
        break;
      endif
      y_old = y_new;
    endfor

    inv(k) = y_new;
  endif

endfunction


%!shared x
%! x = [-1 0 0.63212055882855778 1 2];
%!assert (gaminv (x, ones (1,5), ones (1,5)), [NaN 0 1 Inf NaN], eps)
%!assert (gaminv (x, 1, ones (1,5)), [NaN 0 1 Inf NaN], eps)
%!assert (gaminv (x, ones (1,5), 1), [NaN 0 1 Inf NaN], eps)
%!assert (gaminv (x, [1 -Inf NaN Inf 1], 1), [NaN NaN NaN NaN NaN])
%!assert (gaminv (x, 1, [1 -Inf NaN Inf 1]), [NaN NaN NaN NaN NaN])
%!assert (gaminv ([x(1:2) NaN x(4:5)], 1, 1), [NaN 0 NaN Inf NaN])

## Test class of input preserved
%!assert (gaminv ([x, NaN], 1, 1), [NaN 0 1 Inf NaN NaN], eps)
%!assert (gaminv (single ([x, NaN]), 1, 1), single ([NaN 0 1 Inf NaN NaN]), eps ("single"))
%!assert (gaminv ([x, NaN], single (1), 1), single ([NaN 0 1 Inf NaN NaN]), eps ("single"))
%!assert (gaminv ([x, NaN], 1, single (1)), single ([NaN 0 1 Inf NaN NaN]), eps ("single"))

## Test input validation
%!error gaminv ()
%!error gaminv (1)
%!error gaminv (1,2)
%!error gaminv (1,2,3,4)
%!error gaminv (ones (3), ones (2), ones (2))
%!error gaminv (ones (2), ones (3), ones (2))
%!error gaminv (ones (2), ones (2), ones (3))
%!error gaminv (i, 2, 2)
%!error gaminv (2, i, 2)
%!error gaminv (2, 2, i)

