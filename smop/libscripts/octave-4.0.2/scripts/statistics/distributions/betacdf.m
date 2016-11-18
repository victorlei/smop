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
## @deftypefn {Function File} {} betacdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the Beta distribution with parameters @var{a} and
## @var{b}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Beta distribution

function cdf = betacdf (x, a, b)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (a) || ! isscalar (b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("betacdf: X, A, and B must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("betacdf: X, A, and B must not be complex");
  endif

  if (isa (x, "single") || isa (a, "single") || isa (b, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = isnan (x) | !(a > 0) | !(b > 0);
  cdf(k) = NaN;

  k = (x >= 1) & (a > 0) & (b > 0);
  cdf(k) = 1;

  k = (x > 0) & (x < 1) & (a > 0) & (b > 0);
  if (isscalar (a) && isscalar (b))
    cdf(k) = betainc (x(k), a, b);
  else
    cdf(k) = betainc (x(k), a(k), b(k));
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 2];
%! y = [0 0 0.75 1 1];
%!assert (betacdf (x, ones (1,5), 2*ones (1,5)), y)
%!assert (betacdf (x, 1, 2*ones (1,5)), y)
%!assert (betacdf (x, ones (1,5), 2), y)
%!assert (betacdf (x, [0 1 NaN 1 1], 2), [NaN 0 NaN 1 1])
%!assert (betacdf (x, 1, 2*[0 1 NaN 1 1]), [NaN 0 NaN 1 1])
%!assert (betacdf ([x(1:2) NaN x(4:5)], 1, 2), [y(1:2) NaN y(4:5)])

## Test class of input preserved
%!assert (betacdf ([x, NaN], 1, 2), [y, NaN])
%!assert (betacdf (single ([x, NaN]), 1, 2), single ([y, NaN]))
%!assert (betacdf ([x, NaN], single (1), 2), single ([y, NaN]))
%!assert (betacdf ([x, NaN], 1, single (2)), single ([y, NaN]))

## Test input validation
%!error betacdf ()
%!error betacdf (1)
%!error betacdf (1,2)
%!error betacdf (1,2,3,4)
%!error betacdf (ones (3), ones (2), ones (2))
%!error betacdf (ones (2), ones (3), ones (2))
%!error betacdf (ones (2), ones (2), ones (3))

