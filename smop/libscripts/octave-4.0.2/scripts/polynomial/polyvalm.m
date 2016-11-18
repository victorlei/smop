## Copyright (C) 1994-2015 John W. Eaton
## Copyright (C) 2009 Jaroslav Hajek
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
## @deftypefn {Function File} {} polyvalm (@var{c}, @var{x})
## Evaluate a polynomial in the matrix sense.
##
## @code{polyvalm (@var{c}, @var{x})} will evaluate the polynomial in the
## matrix sense, i.e., matrix multiplication is used instead of element by
## element multiplication as used in @code{polyval}.
##
## The argument @var{x} must be a square matrix.
## @seealso{polyval, roots, poly}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function y = polyvalm (c, x)

  if (nargin != 2)
    print_usage ();
  endif

  if (! (isvector (c) || isempty (c)))
    error ("polyvalm: first argument must be a vector");
  endif

  if (! issquare (x))
    error ("polyvalm: second argument must be a square matrix");
  endif

  n = length (c);
  if (n == 0)
    y = zeros (rows (x), class (x));
  else
    id = eye (rows (x), class (x));
    y = c(1) * id;
    for i = 2:n
      y = y * x + c(i) * id;
    endfor
  endif

endfunction


%!assert (! any (polyvalm ([], [1, 2; 3, 4]))(:));
%!assert (polyvalm ([1, 2, 3, 4], [3, -4, 1; -2, 0, 2; -1, 4, -3]), [117, -124, 11; -70, 36, 38; -43, 92, -45])

%!error <must be a square matrix> polyvalm ([1, 1, 1], [1, 2; 3, 4; 5, 6])

