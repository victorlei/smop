## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn  {Function File} {} rot90 (@var{A})
## @deftypefnx {Function File} {} rot90 (@var{A}, @var{k})
## Rotate array by 90 degree increments.
##
## Return a copy of @var{A} with the elements rotated counterclockwise in
## 90-degree increments.
##
## The second argument is optional, and specifies how many 90-degree rotations
## are to be applied (the default value is 1).  Negative values of @var{k}
## rotate the matrix in a clockwise direction.
## For example,
##
## @example
## @group
## rot90 ([1, 2; 3, 4], -1)
##     @result{}  3  1
##         4  2
## @end group
## @end example
##
## @noindent
## rotates the given matrix clockwise by 90 degrees.  The following are all
## equivalent statements:
##
## @example
## @group
## rot90 ([1, 2; 3, 4], -1)
## rot90 ([1, 2; 3, 4], 3)
## rot90 ([1, 2; 3, 4], 7)
## @end group
## @end example
##
## The rotation is always performed on the plane of the first two dimensions,
## i.e., rows and columns.  To perform a rotation on any other plane, use
## @code{rotdim}.
##
## @seealso{rotdim, fliplr, flipud, flip}
## @end deftypefn

## Author: jwe

function B = rot90 (A, k = 1)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! (isscalar (k) && isreal (k) && k == fix (k)))
    error ("rot90: K must be a single real integer");
  endif

  k = mod (k, 4);
  nd = ndims (A);

  if (k == 0)
    B = A;
  elseif (k == 1)
    B = flipud (permute (A, [2 1 3:1:nd]));
  elseif (k == 2)
    idx(1:nd) = {':'};
    idx{1} = rows (A):-1:1;
    idx{2} = columns (A):-1:1;
    B = A(idx{:});
  elseif (k == 3)
    B = permute (flipud (A), [2 1 3:1:nd]);
  else
    error ("rot90: internal error!");
  endif

endfunction


%!test
%! x1 = [1, 2; 3, 4];
%! x2 = [2, 4; 1, 3];
%! x3 = [4, 3; 2, 1];
%! x4 = [3, 1; 4, 2];
%!
%! assert (rot90 (x1), x2);
%! assert (rot90 (x1, 2), x3);
%! assert (rot90 (x1, 3), x4);
%! assert (rot90 (x1, 4), x1);
%! assert (rot90 (x1, 5), x2);
%! assert (rot90 (x1, -1), x4);

## Test NDArrays
%!test
%! a(1:2,1:2,1) = [1 2; 3 4];
%! a(1:2,1:2,2) = [5 6; 7 8];
%! b(1:2,1:2,1) = [2 4; 1 3];
%! b(1:2,1:2,2) = [6 8; 5 7];
%! assert (rot90 (a, 1), b)
%! assert (rot90 (a, 2), rot90 (b, 1))
%! assert (rot90 (a, 3), rot90 (b, 2))

%!test
%! a = b = zeros (2, 2, 1, 2);
%! a(1:2,1:2,:,1) = [1 2; 3 4];
%! a(1:2,1:2,:,2) = [5 6; 7 8];
%! b(1:2,1:2,:,1) = [2 4; 1 3];
%! b(1:2,1:2,:,2) = [6 8; 5 7];
%! assert (rot90 (a, 1), b)
%! assert (rot90 (a, 2), rot90 (b, 1))
%! assert (rot90 (a, 3), rot90 (b, 2))

## With non-square matrices
%!test
%! a = zeros (3, 2, 1, 2);
%! b = zeros (2, 3, 1, 2);
%! a(1:2,1:3,:,1) = [ 1  2  3;  4  5  6];
%! a(1:2,1:3,:,2) = [ 7  8  9; 10 11 12];
%! b(1:3,1:2,:,1) = [ 3  6;  2  5;  1  4];
%! b(1:3,1:2,:,2) = [ 9 12;  8 11;  7 10];
%! assert (rot90 (a, 1), b)
%! assert (rot90 (a, 2), rot90 (b, 1))
%! assert (rot90 (a, 3), rot90 (b, 2))

## Test input validation
%!error rot90 ()
%!error rot90 (1, 2, 3)
%!error rot90 (1, ones (2))
%!error rot90 (1, 1.5)
%!error rot90 (1, 1+i)

