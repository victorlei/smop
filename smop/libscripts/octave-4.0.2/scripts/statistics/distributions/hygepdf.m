## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 1996-2015 Kurt Hornik
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
## @deftypefn {Function File} {} hygepdf (@var{x}, @var{t}, @var{m}, @var{n})
## Compute the probability density function (PDF) at @var{x} of the
## hypergeometric distribution with parameters @var{t}, @var{m}, and @var{n}.
##
## This is the probability of obtaining @var{x} marked items when randomly
## drawing a sample of size @var{n} without replacement from a population of
## total size @var{t} containing @var{m} marked items.
##
## The parameters @var{t}, @var{m}, and @var{n} must be positive integers
## with @var{m} and @var{n} not greater than @var{t}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the hypergeometric distribution

function pdf = hygepdf (x, t, m, n)

  if (nargin != 4)
    print_usage ();
  endif

  if (! isscalar (t) || ! isscalar (m) || ! isscalar (n))
    [retval, x, t, m, n] = common_size (x, t, m, n);
    if (retval > 0)
      error ("hygepdf: X, T, M, and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (t) || iscomplex (m) || iscomplex (n))
    error ("hygepdf: X, T, M, and N must not be complex");
  endif

  if (isa (x, "single") || isa (t, "single")
      || isa (m, "single") || isa (n, "single"))
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  ## everything in nel gives NaN
  nel = (isnan (x) | (t < 0) | (m < 0) | (n <= 0) | (m > t) | (n > t) |
        (t != fix (t)) | (m != fix (m)) | (n != fix (n)));
  ## everything in zel gives 0 unless in nel
  zel = ((x != fix (x)) | (x < 0) | (x > m) | (n < x) | (n-x > t-m));

  pdf(nel) = NaN;

  k = !nel & !zel;
  if (any (k(:)))
    if (isscalar (t) && isscalar (m) && isscalar (n))
      pdf(k) = (bincoeff (m, x(k)) .* bincoeff (t-m, n-x(k))
                / bincoeff (t, n));
    else
      pdf(k) = (bincoeff (m(k), x(k)) .* bincoeff (t(k)-m(k), n(k)-x(k))
                ./ bincoeff (t(k), n(k)));
    endif
  endif

endfunction


%!shared x,y
%! x = [-1 0 1 2 3];
%! y = [0 1/6 4/6 1/6 0];
%!assert (hygepdf (x, 4*ones (1,5), 2, 2), y)
%!assert (hygepdf (x, 4, 2*ones (1,5), 2), y)
%!assert (hygepdf (x, 4, 2, 2*ones (1,5)), y)
%!assert (hygepdf (x, 4*[1 -1 NaN 1.1 1], 2, 2), [0 NaN NaN NaN 0])
%!assert (hygepdf (x, 4, 2*[1 -1 NaN 1.1 1], 2), [0 NaN NaN NaN 0])
%!assert (hygepdf (x, 4, 5, 2), [NaN NaN NaN NaN NaN])
%!assert (hygepdf (x, 4, 2, 2*[1 -1 NaN 1.1 1]), [0 NaN NaN NaN 0])
%!assert (hygepdf (x, 4, 2, 5), [NaN NaN NaN NaN NaN])
%!assert (hygepdf ([x, NaN], 4, 2, 2), [y, NaN], eps)

## Test class of input preserved
%!assert (hygepdf (single ([x, NaN]), 4, 2, 2), single ([y, NaN]))
%!assert (hygepdf ([x, NaN], single (4), 2, 2), single ([y, NaN]))
%!assert (hygepdf ([x, NaN], 4, single (2), 2), single ([y, NaN]))
%!assert (hygepdf ([x, NaN], 4, 2, single (2)), single ([y, NaN]))

## Test input validation
%!error hygepdf ()
%!error hygepdf (1)
%!error hygepdf (1,2)
%!error hygepdf (1,2,3)
%!error hygepdf (1,2,3,4,5)
%!error hygepdf (1, ones (3), ones (2), ones (2))
%!error hygepdf (1, ones (2), ones (3), ones (2))
%!error hygepdf (1, ones (2), ones (2), ones (3))
%!error hygepdf (i, 2, 2, 2)
%!error hygepdf (2, i, 2, 2)
%!error hygepdf (2, 2, i, 2)
%!error hygepdf (2, 2, 2, i)

