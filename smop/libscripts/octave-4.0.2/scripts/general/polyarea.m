## Copyright (C) 1999-2015 David M. Doolin
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
## @deftypefn  {Function File} {} polyarea (@var{x}, @var{y})
## @deftypefnx {Function File} {} polyarea (@var{x}, @var{y}, @var{dim})
##
## Determine area of a polygon by triangle method.
##
## The variables @var{x} and @var{y} define the vertex pairs, and must
## therefore have the same shape.  They can be either vectors or arrays.  If
## they are arrays then the columns of @var{x} and @var{y} are treated
## separately and an area returned for each.
##
## If the optional @var{dim} argument is given, then @code{polyarea} works
## along this dimension of the arrays @var{x} and @var{y}.
##
## @end deftypefn

## todo:  Add moments for centroid, etc.
##
## bugs and limitations:
##        Probably ought to be an optional check to make sure that
##        traversing the vertices doesn't make any sides cross
##        (Is simple closed curve the technical definition of this?).

## Author: David M. Doolin <doolin@ce.berkeley.edu>
## Date: 1999-04-14
## Modified-by:
##    2000-01-15 Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
##    * use matlab compatible interface
##    * return absolute value of area so traversal order doesn't matter
##    2005-10-13 Torsten Finke
##    * optimization saving half the sums and multiplies

function a = polyarea (x, y, dim)
  if (nargin != 2 && nargin != 3)
    print_usage ();
  elseif (size_equal (x, y))
    if (nargin == 2)
      a = abs (sum (x .* (shift (y, -1) - shift (y, 1)))) / 2;
    else
      a = abs (sum (x .* (shift (y, -1, dim) - shift (y, 1, dim)), dim)) / 2;
    endif
  else
    error ("polyarea: X and Y must have the same shape");
  endif
endfunction


%!shared x, y
%! x = [1;1;3;3;1];
%! y = [1;3;3;1;1];
%!assert (polyarea (x,y), 4, eps)
%!assert (polyarea ([x,x],[y,y]), [4,4], eps)
%!assert (polyarea ([x,x],[y,y],1), [4,4], eps)
%!assert (polyarea ([x,x]',[y,y]',2), [4;4], eps)

