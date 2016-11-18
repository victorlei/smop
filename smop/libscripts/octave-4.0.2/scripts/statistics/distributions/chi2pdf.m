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
## @deftypefn {Function File} {} chi2pdf (@var{x}, @var{n})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the chi-square distribution with @var{n} degrees of freedom.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: PDF of the chi-square distribution

function pdf = chi2pdf (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("chi2pdf: X and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n))
    error ("chi2pdf: X and N must not be complex");
  endif

  pdf = gampdf (x, n/2, 2);

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 Inf];
%! y = [0, 1/2 * exp(-x(2:5)/2)];
%!assert (chi2pdf (x, 2*ones (1,5)), y)
%!assert (chi2pdf (x, 2), y)
%!assert (chi2pdf (x, 2*[1 0 NaN 1 1]), [y(1) NaN NaN y(4:5)])
%!assert (chi2pdf ([x, NaN], 2), [y, NaN])

## Test class of input preserved
%!assert (chi2pdf (single ([x, NaN]), 2), single ([y, NaN]))
%!assert (chi2pdf ([x, NaN], single (2)), single ([y, NaN]))

## Test input validation
%!error chi2pdf ()
%!error chi2pdf (1)
%!error chi2pdf (1,2,3)
%!error chi2pdf (ones (3), ones (2))
%!error chi2pdf (ones (2), ones (3))
%!error chi2pdf (i, 2)
%!error chi2pdf (2, i)

