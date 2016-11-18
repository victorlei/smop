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
## @deftypefn {Function File} {} chi2cdf (@var{x}, @var{n})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the chi-square distribution with @var{n} degrees of
## freedom.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: CDF of the chi-square distribution

function cdf = chi2cdf (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("chi2cdf: X and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n))
    error ("chi2cdf: X and N must not be complex");
  endif

  cdf = gamcdf (x, n/2, 2);

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 2];
%! y = [0, 1 - exp(-x(2:end)/2)];
%!assert (chi2cdf (x, 2*ones (1,5)), y, eps)
%!assert (chi2cdf (x, 2), y, eps)
%!assert (chi2cdf (x, 2*[1 0 NaN 1 1]), [y(1) NaN NaN y(4:5)], eps)
%!assert (chi2cdf ([x(1:2) NaN x(4:5)], 2), [y(1:2) NaN y(4:5)], eps)

## Test class of input preserved
%!assert (chi2cdf ([x, NaN], 2), [y, NaN], eps)
%!assert (chi2cdf (single ([x, NaN]), 2), single ([y, NaN]), eps ("single"))
%!assert (chi2cdf ([x, NaN], single (2)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error chi2cdf ()
%!error chi2cdf (1)
%!error chi2cdf (1,2,3)
%!error chi2cdf (ones (3), ones (2))
%!error chi2cdf (ones (2), ones (3))
%!error chi2cdf (i, 2)
%!error chi2cdf (2, i)

