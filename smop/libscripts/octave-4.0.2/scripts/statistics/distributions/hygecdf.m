## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 1997-2015 Kurt Hornik
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
## @deftypefn {Function File} {} hygecdf (@var{x}, @var{t}, @var{m}, @var{n})
## Compute the cumulative distribution function (CDF) at @var{x} of the
## hypergeometric distribution with parameters @var{t}, @var{m}, and @var{n}.
##
## This is the probability of obtaining not more than @var{x} marked items
## when randomly drawing a sample of size @var{n} without replacement from a
## population of total size @var{t} containing @var{m} marked items.
##
## The parameters @var{t}, @var{m}, and @var{n} must be positive integers
## with @var{m} and @var{n} not greater than @var{t}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the hypergeometric distribution

function cdf = hygecdf (x, t, m, n)

  if (nargin != 4)
    print_usage ();
  endif

  if (! isscalar (t) || ! isscalar (m) || ! isscalar (n))
    [retval, x, t, m, n] = common_size (x, t, m, n);
    if (retval > 0)
      error ("hygecdf: X, T, M, and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (t) || iscomplex (m) || iscomplex (n))
    error ("hygecdf: X, T, M, and N must not be complex");
  endif

  if (isa (x, "single") || isa (t, "single")
      || isa (m, "single") || isa (n, "single"))
    cdf = NaN (size (x), "single");
  else
    cdf = NaN (size (x));
  endif

  ok = ((t >= 0) & (m >= 0) & (n > 0) & (m <= t) & (n <= t) &
        (t == fix (t)) & (m == fix (m)) & (n == fix (n)));

  if (isscalar (t))
    if (ok)
      cdf = discrete_cdf (x, 0 : n, hygepdf (0 : n, t, m, n));
    endif
  else
    for i = find (ok(:)')  # Must be row vector arg to for loop
      v = 0 : n(i);
      cdf(i) = discrete_cdf (x(i), v, hygepdf (v, t(i), m(i), n(i)));
    endfor
  endif

endfunction


%!shared x,y
%! x = [-1 0 1 2 3];
%! y = [0 1/6 5/6 1 1];
%!assert (hygecdf (x, 4*ones (1,5), 2, 2), y, eps)
%!assert (hygecdf (x, 4, 2*ones (1,5), 2), y, eps)
%!assert (hygecdf (x, 4, 2, 2*ones (1,5)), y, eps)
%!assert (hygecdf (x, 4*[1 -1 NaN 1.1 1], 2, 2), [y(1) NaN NaN NaN y(5)], eps)
%!assert (hygecdf (x, 4, 2*[1 -1 NaN 1.1 1], 2), [y(1) NaN NaN NaN y(5)], eps)
%!assert (hygecdf (x, 4, 5, 2), [NaN NaN NaN NaN NaN])
%!assert (hygecdf (x, 4, 2, 2*[1 -1 NaN 1.1 1]), [y(1) NaN NaN NaN y(5)], eps)
%!assert (hygecdf (x, 4, 2, 5), [NaN NaN NaN NaN NaN])
%!assert (hygecdf ([x(1:2) NaN x(4:5)], 4, 2, 2), [y(1:2) NaN y(4:5)], eps)

## Test class of input preserved
%!assert (hygecdf ([x, NaN], 4, 2, 2), [y, NaN], eps)
%!assert (hygecdf (single ([x, NaN]), 4, 2, 2), single ([y, NaN]), eps ("single"))
%!assert (hygecdf ([x, NaN], single (4), 2, 2), single ([y, NaN]), eps ("single"))
%!assert (hygecdf ([x, NaN], 4, single (2), 2), single ([y, NaN]), eps ("single"))
%!assert (hygecdf ([x, NaN], 4, 2, single (2)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error hygecdf ()
%!error hygecdf (1)
%!error hygecdf (1,2)
%!error hygecdf (1,2,3)
%!error hygecdf (1,2,3,4,5)
%!error hygecdf (ones (2), ones (3), 1, 1)
%!error hygecdf (1, ones (2), ones (3), 1)
%!error hygecdf (1, 1, ones (2), ones (3))
%!error hygecdf (i, 2, 2, 2)
%!error hygecdf (2, i, 2, 2)
%!error hygecdf (2, 2, i, 2)
%!error hygecdf (2, 2, 2, i)

