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
## @deftypefn {Function File} {} unidcdf (@var{x}, @var{n})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of a discrete uniform distribution which assumes
## the integer values 1--@var{n} with equal probability.
## @end deftypefn

function cdf = unidcdf (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("unidcdf: X and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n))
    error ("unidcdf: X and N must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  knan = isnan (x) | ! (n > 0 & n == fix (n));
  if (any (knan(:)))
    cdf(knan) = NaN;
  endif

  k = (x >= n) & !knan;
  cdf(k) = 1;

  k = (x >= 1) & (x < n) & !knan;
  if (isscalar (n))
    cdf(k) = floor (x(k)) / n;
  else
    cdf(k) = floor (x(k)) ./ n(k);
  endif

endfunction


%!shared x,y
%! x = [0 1 2.5 10 11];
%! y = [0, 0.1 0.2 1.0 1.0];
%!assert (unidcdf (x, 10*ones (1,5)), y)
%!assert (unidcdf (x, 10), y)
%!assert (unidcdf (x, 10*[0 1 NaN 1 1]), [NaN 0.1 NaN y(4:5)])
%!assert (unidcdf ([x(1:2) NaN Inf x(5)], 10), [y(1:2) NaN 1 y(5)])

## Test class of input preserved
%!assert (unidcdf ([x, NaN], 10), [y, NaN])
%!assert (unidcdf (single ([x, NaN]), 10), single ([y, NaN]))
%!assert (unidcdf ([x, NaN], single (10)), single ([y, NaN]))

## Test input validation
%!error unidcdf ()
%!error unidcdf (1)
%!error unidcdf (1,2,3)
%!error unidcdf (ones (3), ones (2))
%!error unidcdf (ones (2), ones (3))
%!error unidcdf (i, 2)
%!error unidcdf (2, i)

