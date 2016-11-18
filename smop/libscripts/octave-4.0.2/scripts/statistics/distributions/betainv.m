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
## @deftypefn {Function File} {} betainv (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the Beta distribution with parameters @var{a} and @var{b}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Beta distribution

function inv = betainv (x, a, b)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (a) || ! isscalar (b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("betainv: X, A, and B must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("betainv: X, A, and B must not be complex");
  endif

  if (isa (x, "single") || isa (a, "single") || isa (b, "single"))
    inv = zeros (size (x), "single");
  else
    inv = zeros (size (x));
  endif

  k = (x < 0) | (x > 1) | !(a > 0) | !(b > 0) | isnan (x);
  inv(k) = NaN;

  k = (x == 1) & (a > 0) & (b > 0);
  inv(k) = 1;

  k = find ((x > 0) & (x < 1) & (a > 0) & (b > 0));
  if (any (k))
    if (! isscalar (a) || ! isscalar (b))
      a = a(k);
      b = b(k);
      y = a ./ (a + b);
    else
      y = a / (a + b) * ones (size (k));
    endif
    x = x(k);

    if (isa (y, "single"))
      myeps = eps ("single");
    else
      myeps = eps;
    endif

    l = find (y < myeps);
    if (any (l))
      y(l) = sqrt (myeps) * ones (length (l), 1);
    endif
    l = find (y > 1 - myeps);
    if (any (l))
      y(l) = 1 - sqrt (myeps) * ones (length (l), 1);
    endif

    y_old = y;
    for i = 1 : 10000
      h     = (betacdf (y_old, a, b) - x) ./ betapdf (y_old, a, b);
      y_new = y_old - h;
      ind   = find (y_new <= myeps);
      if (any (ind))
        y_new(ind) = y_old(ind) / 10;
      endif
      ind = find (y_new >= 1 - myeps);
      if (any (ind))
        y_new(ind) = 1 - (1 - y_old(ind)) / 10;
      endif
      h = y_old - y_new;
      if (max (abs (h)) < sqrt (myeps))
        break;
      endif
      y_old = y_new;
    endfor

    inv(k) = y_new;
  endif

endfunction


%!shared x
%! x = [-1 0 0.75 1 2];
%!assert (betainv (x, ones (1,5), 2*ones (1,5)), [NaN 0 0.5 1 NaN])
%!assert (betainv (x, 1, 2*ones (1,5)), [NaN 0 0.5 1 NaN])
%!assert (betainv (x, ones (1,5), 2), [NaN 0 0.5 1 NaN])
%!assert (betainv (x, [1 0 NaN 1 1], 2), [NaN NaN NaN 1 NaN])
%!assert (betainv (x, 1, 2*[1 0 NaN 1 1]), [NaN NaN NaN 1 NaN])
%!assert (betainv ([x(1:2) NaN x(4:5)], 1, 2), [NaN 0 NaN 1 NaN])

## Test class of input preserved
%!assert (betainv ([x, NaN], 1, 2), [NaN 0 0.5 1 NaN NaN])
%!assert (betainv (single ([x, NaN]), 1, 2), single ([NaN 0 0.5 1 NaN NaN]))
%!assert (betainv ([x, NaN], single (1), 2), single ([NaN 0 0.5 1 NaN NaN]))
%!assert (betainv ([x, NaN], 1, single (2)), single ([NaN 0 0.5 1 NaN NaN]))

## Test input validation
%!error betainv ()
%!error betainv (1)
%!error betainv (1,2)
%!error betainv (1,2,3,4)
%!error betainv (ones (3), ones (2), ones (2))
%!error betainv (ones (2), ones (3), ones (2))
%!error betainv (ones (2), ones (2), ones (3))
%!error betainv (i, 2, 2)
%!error betainv (2, i, 2)
%!error betainv (2, 2, i)

