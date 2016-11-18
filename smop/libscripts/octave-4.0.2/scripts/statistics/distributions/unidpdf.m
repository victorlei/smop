## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn {Function File} {} unidpdf (@var{x}, @var{n})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of a discrete uniform distribution which assumes
## the integer values 1--@var{n} with equal probability.
##
## Warning: The underlying implementation uses the double class and will only
## be accurate for @var{n} @leq{} @code{bitmax} (@w{@math{2^{53} - 1}} on
## IEEE 754 compatible systems).
## @end deftypefn

function pdf = unidpdf (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("unidpdf: X and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n))
    error ("unidpdf: X and N must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single"))
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = isnan (x) | ! (n > 0 & n == fix (n));
  pdf(k) = NaN;

  k = !k & (x >= 1) & (x <= n) & (x == fix (x));
  if (isscalar (n))
    pdf(k) = 1 / n;
  else
    pdf(k) = 1 ./ n(k);
  endif

endfunction


%!shared x,y
%! x = [-1 0 1 2 10 11];
%! y = [0 0 0.1 0.1 0.1 0];
%!assert (unidpdf (x, 10*ones (1,6)), y)
%!assert (unidpdf (x, 10), y)
%!assert (unidpdf (x, 10*[0 NaN 1 1 1 1]), [NaN NaN y(3:6)])
%!assert (unidpdf ([x, NaN], 10), [y, NaN])

## Test class of input preserved
%!assert (unidpdf (single ([x, NaN]), 10), single ([y, NaN]))
%!assert (unidpdf ([x, NaN], single (10)), single ([y, NaN]))

## Test input validation
%!error unidpdf ()
%!error unidpdf (1)
%!error unidpdf (1,2,3)
%!error unidpdf (ones (3), ones (2))
%!error unidpdf (ones (2), ones (3))
%!error unidpdf (i, 2)
%!error unidpdf (2, i)

