## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn  {Function File} {[@var{xx}, @var{yy}] =} meshgrid (@var{x}, @var{y})
## @deftypefnx {Function File} {[@var{xx}, @var{yy}, @var{zz}] =} meshgrid (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {[@var{xx}, @var{yy}] =} meshgrid (@var{x})
## @deftypefnx {Function File} {[@var{xx}, @var{yy}, @var{zz}] =} meshgrid (@var{x})
## Given vectors of @var{x} and @var{y} coordinates, return matrices @var{xx}
## and @var{yy} corresponding to a full 2-D grid.
##
## The rows of @var{xx} are copies of @var{x}, and the columns of @var{yy} are
## copies of @var{y}.  If @var{y} is omitted, then it is assumed to be the same
## as @var{x}.
##
## If the optional @var{z} input is given, or @var{zz} is requested, then the
## output will be a full 3-D grid.
##
## @code{meshgrid} is most frequently used to produce input for a 2-D or 3-D
## function that will be plotted.  The following example creates a surface
## plot of the ``sombrero'' function.
##
## @example
## @group
## f = @@(x,y) sin (sqrt (x.^2 + y.^2)) ./ sqrt (x.^2 + y.^2);
## range = linspace (-8, 8, 41);
## [@var{X}, @var{Y}] = meshgrid (range, range);
## Z = f (X, Y);
## surf (X, Y, Z);
## @end group
## @end example
##
## Programming Note: @code{meshgrid} is restricted to 2-D or 3-D grid
## generation.  The @code{ndgrid} function will generate 1-D through N-D
## grids.  However, the functions are not completely equivalent.  If @var{x}
## is a vector of length M and @var{y} is a vector of length N, then
## @code{meshgrid} will produce an output grid which is NxM@.  @code{ndgrid}
## will produce an output which is @nospell{MxN} (transpose) for the same
## input.  Some core functions expect @code{meshgrid} input and others expect
## @code{ndgrid} input.  Check the documentation for the function in question
## to determine the proper input format.
## @seealso{ndgrid, mesh, contour, surf}
## @end deftypefn

## Author: jwe

function [xx, yy, zz] = meshgrid (x, y, z)

  if (nargin == 0 || nargin > 3)
    print_usage ();
  endif

  if (nargin < 2)
    y = x;
  endif

  ## Use repmat to ensure that result values have the same type as the inputs

  if (nargout < 3)
    if (! (isvector (x) && isvector (y)))
      error ("meshgrid: X and Y must be vectors");
    endif
    xx = repmat (x(:).', length (y), 1);
    yy = repmat (y(:), 1, length (x));
  else
    if (nargin < 3)
      z = y;
    endif
    if (! (isvector (x) && isvector (y) && isvector (z)))
      error ("meshgrid: X, Y, and Z must be vectors");
    endif
    lenx = length (x);
    leny = length (y);
    lenz = length (z);
    xx = repmat (repmat (x(:).', leny, 1), [1, 1, lenz]);
    yy = repmat (repmat (y(:), 1, lenx), [1, 1, lenz]);
    zz = reshape (repmat (z(:).', lenx*leny, 1)(:), leny, lenx, lenz);
  endif

endfunction


%!test
%! x = 1:2;
%! y = 1:3;
%! z = 1:4;
%! [XX, YY, ZZ] = meshgrid (x, y, z);
%! assert (size_equal (XX, YY, ZZ));
%! assert (ndims (XX), 3);
%! assert (size (XX), [3, 2, 4]);
%! assert (XX(1) * YY(1) * ZZ(1), x(1) * y(1) * z(1));
%! assert (XX(end) * YY(end) * ZZ(end), x(end) * y(end) * z(end));

%!test
%! x = 1:2;
%! y = 1:3;
%! [XX, YY] = meshgrid (x, y);
%! assert (size_equal (XX, YY));
%! assert (ndims (XX), 2);
%! assert (size (XX), [3, 2]);
%! assert (XX(1) * YY(1), x(1) * y(1));
%! assert (XX(end) * YY(end), x(end) * y(end));

%!test
%! x = 1:3;
%! [XX1, YY1] = meshgrid (x, x);
%! [XX2, YY2] = meshgrid (x);
%! assert (size_equal (XX1, XX2, YY1, YY2));
%! assert (ndims (XX1), 2);
%! assert (size (XX1), [3, 3]);
%! assert (XX1, XX2);
%! assert (YY1, YY2);

## Test input validation
%!error meshgrid ()
%!error meshgrid (1,2,3,4)
%!error <X and Y must be vectors> meshgrid (ones (2,2), 1:3)
%!error <X and Y must be vectors> meshgrid (1:3, ones (2,2))
%!error <X, Y, and Z must be vectors> [X,Y,Z] = meshgrid (1:3, 1:3, ones (2,2))

