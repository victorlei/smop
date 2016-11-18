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
## @deftypefn  {Function File} {} cross (@var{x}, @var{y})
## @deftypefnx {Function File} {} cross (@var{x}, @var{y}, @var{dim})
## Compute the vector cross product of two 3-dimensional vectors @var{x} and
## @var{y}.
##
## If @var{x} and @var{y} are matrices, the cross product is applied along the
## first dimension with three elements.
##
## The optional argument  @var{dim} forces the cross product to be calculated
## along the specified dimension.
##
## Example Code:
##
## @example
## @group
## cross ([1,1,0], [0,1,1])
##      @result{} [ 1; -1; 1 ]
## @end group
## @end example
##
## @seealso{dot, curl, divergence}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Created: 15 October 1994
## Adapted-By: jwe

function z = cross (x, y, dim)

  if (nargin != 2 && nargin != 3)
    print_usage ();
  endif

  if (ndims (x) < 3 && ndims (y) < 3 && nargin < 3)
    ## COMPATIBILITY -- opposite behaviour for cross(row,col)
    ## Swap x and y in the assignments below to get the matlab behaviour.
    ## Better yet, fix the calling code so that it uses conformant vectors.
    if (columns (x) == 1 && rows (y) == 1)
      warning ("cross: taking cross product of column by row");
      y = y.';
    elseif (rows (x) == 1 && columns (y) == 1)
      warning ("cross: taking cross product of row by column");
      x = x.';
    endif
  endif

  if (nargin == 2)
     dim = find (size (x) == 3, 1);
     if (isempty (dim))
       error ("cross: must have at least one dimension with 3 elements");
     endif
   else
     if (size (x, dim) != 3)
       error ("cross: dimension DIM must have 3 elements");
     endif
  endif

  nd = ndims (x);
  sz = size (x);
  idx2 = idx3 = idx1 = {':'}(ones (1, nd));
  idx1(dim) = 1;
  idx2(dim) = 2;
  idx3(dim) = 3;

  if (size_equal (x, y))
    x1 = x(idx1{:});
    x2 = x(idx2{:});
    x3 = x(idx3{:});
    y1 = y(idx1{:});
    y2 = y(idx2{:});
    y3 = y(idx3{:});
    z = cat (dim, (x2.*y3 - x3.*y2), (x3.*y1 - x1.*y3), (x1.*y2 - x2.*y1));
  else
    error ("cross: X and Y must have the same dimensions");
  endif

endfunction


%!test
%! x = [1 0 0];
%! y = [0 1 0];
%! r = [0 0 1];
%! assert (cross (x, y), r, 2e-8);

%!test
%! x = [1 2 3];
%! y = [4 5 6];
%! r = [(2*6-3*5) (3*4-1*6) (1*5-2*4)];
%! assert (cross (x, y), r, 2e-8);

%!test
%! x = [1 0 0; 0 1 0; 0 0 1];
%! y = [0 1 0; 0 0 1; 1 0 0];
%! r = [0 0 1; 1 0 0; 0 1 0];
%! assert (cross (x, y, 2), r, 2e-8);
%! assert (cross (x, y, 1), -r, 2e-8);

%!error cross (0,0)
%!error cross ()

