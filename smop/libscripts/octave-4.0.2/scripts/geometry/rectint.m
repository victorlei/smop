## Copyright (C) 2015 Carnë Draug
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
## @deftypefn {Function File} {@var{area} =} rectint (@var{a}, @var{b})
## Compute area or volume of intersection of rectangles or N-D boxes.
##
## Compute the area of intersection of rectangles in @var{a} and rectangles in
## @var{b}.  N-dimensional boxes are supported in which case the volume, or
## hypervolume is computed according to the number of dimensions.
##
## 2-dimensional rectangles are defined as @code{[xpos ypos width height]}
## where xpos and ypos are the position of the bottom left corner.  Higher
## dimensions are supported where the coordinates for the minimum value of each
## dimension follow the length of the box in that dimension, e.g.,
## @code{[xpos ypos zpos kpos @dots{} width height depth k_length @dots{}]}.
##
## Each row of @var{a} and @var{b} define a rectangle, and if both define
## multiple rectangles, then the output, @var{area}, is a matrix where the i-th
## row corresponds to the i-th row of a and the j-th column corresponds to the
## j-th row of b.
##
## @seealso{polyarea}
## @end deftypefn

## Author: 2015 Carnë Draug <carandraug@octave.org>

function dists = rectint (a, b)

  if (nargin != 2)
    print_usage ();
  elseif (columns (a) != columns (b))
    error ("rectint: A and B must have same number of columns");
  elseif (ndims (a) > 2)
    error ("rectint: A and B must be 2-d arrays");
  elseif (mod (columns (a), 2))
    error ("rectint: number of columns of A and B must be a multiple of two");
  endif

  nd = columns (a) / 2;
  na = rows (a);
  nb = rows (b);

  a_start = a(:,1:nd);
  b_start = b(:,1:nd);

  a_end = a_start + a(:,nd+1:end);
  b_end = b_start + b(:,nd+1:end);

  a_start = reshape (a_start, [na 1 nd]);
  b_start = reshape (b_start, [1 nb nd]);

  a_end   = reshape (a_end,   [na 1 nd]);
  b_end   = reshape (b_end,   [1 nb nd]);

  ## We get a 3d matrix where each dimension is in the 3rd dimension
  dists = bsxfun (@min , a_end, b_end) - bsxfun (@max, a_start, b_start);
  dists(dists < 0) = 0;
  dists = prod (dists, 3);

endfunction

## Exactly overlapping
%!assert (rectint ([0 0 1 1], [0 0 1 1]), 1)
## rect2 completely enclosed by rect1
%!assert (rectint ([-1 -1 3 3], [0 0 1 1]), 1)
## rect1 completely enclosed by rect2
%!assert (rectint ([0 0 1 1], [-1 -1 3 3]), 1)
## rect1 right and top in rect2
%!assert (rectint ([-1 -1 1.5 1.5], [0 0 1 1]), 0.25)
## rect2 right and top in rect1
%!assert (rectint ([0 0 1 1], [-1 -1 1.5 1.5]), 0.25)
## no overlap - shared corner
%!assert (rectint ([0 0 1 1], [1 1 2 2]), 0)
## no overlap - shared edge
%!assert (rectint ([0 0 1 1], [0 1 2 2]), 0)
## Correct orientation of output
%!assert (rectint ([0 0 1 1;0.5 0.5 1 1;-1 -1 2 2], [1 1 2 2]), [0;0.25;0])
%!assert (rectint ([1 1 2 2], [0 0 1 1;0.5 0.5 1 1;-1 -1 2 2]), [0 0.25 0])

## bug #44904
%!assert (rectint ([0 0 5 5], [6 6 5 5]), 0)
%!assert (rectint ([0 0 5 5], [0 6 5 5]), 0)
%!assert (rectint ([0 0 5 5], [6 0 5 5]), 0)
%!assert (rectint ([0 0 0 5 5 5], [0 0 6 5 5 5]), 0)

## Test volumes
%!shared r1, r2, r3, r4, r5
%! r1 = [  5   3 0  7   5 2];
%! r2 = [  2   5 0  4   2 2];
%! r3 = [ 10   7 0 10   3 2];
%! r4 = [ 10  -5 0  5   7 2];
%! r5 = [-10   0 0 40  11 2];

%!assert (rectint (r5, r1), 70)
%!assert (rectint (r5, r4), 20)
%!assert (rectint (r5, [r1; r2; r3; r4]), [70 16 60 20])

## Test multiple volumes in both A and B
%!assert (rectint ([r2; r5], [r1; r3; r4]), [4 0 0; 70 60 20])
