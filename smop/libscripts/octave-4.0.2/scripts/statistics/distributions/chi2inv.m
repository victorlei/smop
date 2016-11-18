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
## @deftypefn {Function File} {} chi2inv (@var{x}, @var{n})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the chi-square distribution with @var{n} degrees of freedom.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: Quantile function of the chi-square distribution

function inv = chi2inv (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("chi2inv: X and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n))
    error ("chi2inv: X and N must not be complex");
  endif

  inv = gaminv (x, n/2, 2);

endfunction


%!shared x
%! x = [-1 0 0.3934693402873666 1 2];
%!assert (chi2inv (x, 2*ones (1,5)), [NaN 0 1 Inf NaN], 5*eps)
%!assert (chi2inv (x, 2), [NaN 0 1 Inf NaN], 5*eps)
%!assert (chi2inv (x, 2*[0 1 NaN 1 1]), [NaN 0 NaN Inf NaN], 5*eps)
%!assert (chi2inv ([x(1:2) NaN x(4:5)], 2), [NaN 0 NaN Inf NaN], 5*eps)

## Test class of input preserved
%!assert (chi2inv ([x, NaN], 2), [NaN 0 1 Inf NaN NaN], 5*eps)
%!assert (chi2inv (single ([x, NaN]), 2), single ([NaN 0 1 Inf NaN NaN]), 5*eps ("single"))
%!assert (chi2inv ([x, NaN], single (2)), single ([NaN 0 1 Inf NaN NaN]), 5*eps ("single"))

## Test input validation
%!error chi2inv ()
%!error chi2inv (1)
%!error chi2inv (1,2,3)
%!error chi2inv (ones (3), ones (2))
%!error chi2inv (ones (2), ones (3))
%!error chi2inv (i, 2)
%!error chi2inv (2, i)

