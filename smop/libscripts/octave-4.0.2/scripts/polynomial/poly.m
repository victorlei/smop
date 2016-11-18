## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn  {Function File} {} poly (@var{A})
## @deftypefnx {Function File} {} poly (@var{x})
## If @var{A} is a square @math{N}-by-@math{N} matrix, @code{poly (@var{A})}
## is the row vector of the coefficients of @code{det (z * eye (N) - A)},
## the characteristic polynomial of @var{A}.
##
## For example, the following code finds the eigenvalues of @var{A} which are
## the roots of @code{poly (@var{A})}.
##
## @example
## @group
## roots (poly (eye (3)))
##     @result{} 1.00001 + 0.00001i
##        1.00001 - 0.00001i
##        0.99999 + 0.00000i
## @end group
## @end example
##
## In fact, all three eigenvalues are exactly 1 which emphasizes that for
## numerical performance the @code{eig} function should be used to compute
## eigenvalues.
##
## If @var{x} is a vector, @code{poly (@var{x})} is a vector of the
## coefficients of the polynomial whose roots are the elements of @var{x}.
## That is, if @var{c} is a polynomial, then the elements of
## @code{@var{d} = roots (poly (@var{c}))} are contained in @var{c}.  The
## vectors @var{c} and @var{d} are not identical, however, due to sorting and
## numerical errors.
## @seealso{roots, eig}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 24 December 1993
## Adapted-By: jwe

function y = poly (x)

  if (nargin != 1)
    print_usage ();
  endif

  m = min (size (x));
  n = max (size (x));
  if (m == 0)
    y = 1;
    return;
  elseif (m == 1)
    v = x;
  elseif (m == n)
    v = eig (x);
  else
    print_usage ();
  endif

  y = zeros (1, n+1);
  y(1) = 1;
  for j = 1:n;
    y(2:(j+1)) = y(2:(j+1)) - v(j) .* y(1:j);
  endfor

  if (all (all (imag (x) == 0)))
    y = real (y);
  endif

endfunction


%!assert (poly ([]), 1)
%!assert (poly ([1, 2, 3]), [1, -6, 11, -6])
%!assert (poly ([1, 2; 3, 4]), [1, -5, -2], sqrt (eps))

%!error poly ([1, 2, 3; 4, 5, 6])

