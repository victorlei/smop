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
## @deftypefn {Function File} {} fliplr (@var{x})
## Flip array left to right.
##
## Return a copy of @var{x} with the order of the columns reversed.  In other
## words, @var{x} is flipped left-to-right about a vertical axis.  For example:
##
## @example
## @group
## fliplr ([1, 2; 3, 4])
##      @result{}  2  1
##          4  3
## @end group
## @end example
##
## @seealso{flipud, flip, rot90, rotdim}
## @end deftypefn

## Author: jwe

function y = fliplr (x)

  if (nargin != 1)
    print_usage ();
  endif
  y = flip (x, 2);

endfunction


%!assert (fliplr ([1, 2; 3, 4]), [2, 1; 4, 3])
%!assert (fliplr ([1, 2; 3, 4; 5, 6]), [2, 1; 4, 3; 6, 5])
%!assert (fliplr ([1, 2, 3; 4, 5, 6]), [3, 2, 1; 6, 5, 4])
%!assert (fliplr ([1 2 3].'), [1 2 3].')

## Test NDArrays
%!test
%! a(:,:,1) = [ 1  2;  3  4;  5  6];
%! a(:,:,2) = [ 7  8;  9 10; 11 12];
%! b(:,:,1) = [ 2  1;  4  3;  6  5];
%! b(:,:,2) = [ 8  7; 10  9; 12 11];
%! assert (fliplr (a), b)

## Test NDArray with singleton dimensions
%!test
%! a(:,:,:,1) = [ 1  2;  3  4;  5  6];
%! a(:,:,:,2) = [ 7  8;  9 10; 11 12];
%! b(:,:,:,1) = [ 2  1;  4  3;  6  5];
%! b(:,:,:,2) = [ 8  7; 10  9; 12 11];
%! assert (fliplr (a), b)

## Test for 1 row, i.e., returns the same
%!test
%! a(:,1,:,1) = [ 1  2  3  4];
%! a(:,1,:,2) = [ 5  6  7  8];
%! assert (fliplr (a), a)

%!error fliplr()
%!error fliplr (1, 2)

