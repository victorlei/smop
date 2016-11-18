## Copyright (C) 2004-2015 David Bateman
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
## @deftypefn  {Function File} {} rotdim (@var{x})
## @deftypefnx {Function File} {} rotdim (@var{x}, @var{n})
## @deftypefnx {Function File} {} rotdim (@var{x}, @var{n}, @var{plane})
## Return a copy of @var{x} with the elements rotated counterclockwise in
## 90-degree increments.
##
## The second argument @var{n} is optional, and specifies how many 90-degree
## rotations are to be applied (the default value is 1).  Negative values of
## @var{n} rotate the matrix in a clockwise direction.
##
## The third argument is also optional and defines the plane of the rotation.
## If present, @var{plane} is a two element vector containing two different
## valid dimensions of the matrix.  When @var{plane} is not given the first two
## non-singleton dimensions are used.
##
## For example,
##
## @example
## @group
## rotdim ([1, 2; 3, 4], -1, [1, 2])
##      @result{}  3  1
##          4  2
## @end group
## @end example
##
## @noindent
## rotates the given matrix clockwise by 90 degrees.  The following are all
## equivalent statements:
##
## @example
## @group
## rotdim ([1, 2; 3, 4], -1, [1, 2])
## rotdim ([1, 2; 3, 4], 3, [1, 2])
## rotdim ([1, 2; 3, 4], 7, [1, 2])
## @end group
## @end example
## @seealso{rot90, fliplr, flipud, flip}
## @end deftypefn

function y = rotdim (x, n, plane)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin > 1 && ! isempty (n))
    if (! isscalar (n) || ! isreal (n) || fix (n) != n)
      error ("rotdim: N must be a scalar integer");
    endif
  else
    n = 1;
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin < 3)
    if (nd > 2)
      ## Find the first two non-singleton dimension.
      plane = [];
      dim = 0;
      while (dim < nd)
        dim = dim + 1;
        if (sz (dim) != 1)
          plane = [plane, dim];
          if (length (plane) == 2)
            break;
          endif
        endif
      endwhile
      if (length (plane) < 1)
        plane = [1, 2];
      elseif (length (plane) < 2)
        plane = [1, plane];
      endif
    else
      plane = [1, 2];
    endif
  else
    if (! (isvector (plane) && length (plane) == 2
           && all (plane == fix (plane)) && all (plane > 0)
           && all (plane < (nd + 1)) && plane(1) != plane(2)))
      error ("rotdim: PLANE must be a 2 element integer vector defining a valid PLANE");
    endif
  endif

  n = rem (n, 4);
  if (n < 0)
    n = n + 4;
  endif
  if (n == 0)
    y = x;
  elseif (n == 2)
    y = flip (flip (x, plane(1)), plane(2));
  elseif (n == 1 || n == 3)
    perm = 1:nd;
    perm(plane(1)) = plane(2);
    perm(plane(2)) = plane(1);
    y = permute (x, perm);
    if (n == 1)
      y = flip (y, min (plane));
    else
      y = flip (y, max (plane));
    endif
  else
    error ("rotdim: internal error!");
  endif

endfunction


%!shared r, rr
%! r = [1,2,3];  rr = [3,2,1];
%!assert (rotdim (r, 0), r)
%!assert (rotdim (r, 1), rr')
%!assert (rotdim (r, 2), rr)
%!assert (rotdim (r, 3), r')
%!assert (rotdim (r, 3), rotdim (r, -1))
%!assert (rotdim (r, 1), rotdim (r))

%!shared c, cr
%! c = [1;2;3];  cr = [3;2;1];
%!assert (rotdim (c, 0), c)
%!assert (rotdim (c, 1), c')
%!assert (rotdim (c, 2), cr)
%!assert (rotdim (c, 3), cr')
%!assert (rotdim (c, 3), rotdim (c, -1))
%!assert (rotdim (c, 1), rotdim (c))

%!shared m
%! m = [1,2;3,4];
%!assert (rotdim (m, 0), m)
%!assert (rotdim (m, 1), [2,4;1,3])
%!assert (rotdim (m, 2), [4,3;2,1])
%!assert (rotdim (m, 3), [3,1;4,2])
%!assert (rotdim (m, 3), rotdim (m, -1))
%!assert (rotdim (m, 1), rotdim (m))

## FIXME: We need tests for multidimensional arrays
##        and different values of PLANE.

%!error rotdim ()
%!error rotdim (1, 2, 3, 4)

