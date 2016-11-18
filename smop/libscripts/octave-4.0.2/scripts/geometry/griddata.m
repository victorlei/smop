## Copyright (C) 1999-2015 Kai Habel
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
## @deftypefn  {Function File} {@var{zi} =} griddata (@var{x}, @var{y}, @var{z}, @var{xi}, @var{yi})
## @deftypefnx {Function File} {@var{zi} =} griddata (@var{x}, @var{y}, @var{z}, @var{xi}, @var{yi}, @var{method})
## @deftypefnx {Function File} {[@var{xi}, @var{yi}, @var{zi}] =} griddata (@dots{})
##
## Generate a regular mesh from irregular data using interpolation.
##
## The function is defined by @code{@var{z} = f (@var{x}, @var{y})}.  Inputs
## @code{@var{x}, @var{y}, @var{z}} are vectors of the same length or
## @code{@var{x}, @var{y}} are vectors and @code{@var{z}} is matrix.
##
## The interpolation points are all @code{(@var{xi}, @var{yi})}.  If @var{xi},
## @var{yi} are vectors then they are made into a 2-D mesh.
##
## The interpolation method can be @qcode{"nearest"}, @qcode{"cubic"} or
## @qcode{"linear"}.  If method is omitted it defaults to @qcode{"linear"}.
## @seealso{griddata3, griddatan, delaunay}
## @end deftypefn

## Author:      Kai Habel <kai.habel@gmx.de>
## Adapted-by:  Alexander Barth <barth.alexander@gmail.com>
##              xi and yi are not "meshgridded" if both are vectors
##              of the same size (for compatibility)

function [rx, ry, rz] = griddata (x, y, z, xi, yi, method = "linear")

  if (nargin < 5 || nargin > 7)
    print_usage ();
  endif

  if (ischar (method))
    method = tolower (method);
  endif

  ## Meshgrid if x and y are vectors but z is matrix
  if (isvector (x) && isvector (y) && all ([numel(y), numel(x)] == size (z)))
    [x, y] = meshgrid (x, y);
  endif

  if (isvector (x) && isvector (y) && isvector (z))
    if (! isequal (length (x), length (y), length (z)))
      error ("griddata: X, Y, and Z must be vectors of the same length");
    endif
  elseif (! size_equal (x, y, z))
    error ("griddata: lengths of X, Y must match the columns and rows of Z");
  endif

  ## Meshgrid xi and yi if they are a row and column vector.
  if (rows (xi) == 1 && columns (yi) == 1)
    [xi, yi] = meshgrid (xi, yi);
  elseif (isvector (xi) && isvector (yi))
    ## Otherwise, convert to column vectors
    xi = xi(:);
    yi = yi(:);
  endif

  if (! size_equal (xi, yi))
    error ("griddata: XI and YI must be vectors or matrices of same size");
  endif

  x = x(:);
  y = y(:);
  z = z(:);

  ## Triangulate data.
  tri = delaunay (x, y);
  zi = NaN (size (xi));

  if (strcmp (method, "cubic"))
    error ("griddata: cubic interpolation not yet implemented");

  elseif (strcmp (method, "nearest"))
    ## Search index of nearest point.
    idx = dsearch (x, y, tri, xi, yi);
    valid = ! isnan (idx);
    zi(valid) = z(idx(valid));

  elseif (strcmp (method, "linear"))
    ## Search for every point the enclosing triangle.
    tri_list = tsearch (x, y, tri, xi(:), yi(:));

    ## Only keep the points within triangles.
    valid = ! isnan (tri_list);
    tri_list = tri_list(valid);
    nr_t = rows (tri_list);

    tri = tri(tri_list,:);

    ## Assign x,y,z for each point of triangle.
    x1 = x(tri(:,1));
    x2 = x(tri(:,2));
    x3 = x(tri(:,3));

    y1 = y(tri(:,1));
    y2 = y(tri(:,2));
    y3 = y(tri(:,3));

    z1 = z(tri(:,1));
    z2 = z(tri(:,2));
    z3 = z(tri(:,3));

    ## Calculate norm vector.
    N = cross ([x2-x1, y2-y1, z2-z1], [x3-x1, y3-y1, z3-z1]);
    ## Normalize.
    N = diag (norm (N, "rows")) \ N;

    ## Calculate D of plane equation
    ## Ax+By+Cz+D = 0;
    D = -(N(:,1) .* x1 + N(:,2) .* y1 + N(:,3) .* z1);

    ## Calculate zi by solving plane equation for xi, yi.
    zi(valid) = -(N(:,1).*xi(:)(valid) + N(:,2).*yi(:)(valid) + D) ./ N(:,3);

  else
    error ("griddata: unknown interpolation METHOD");
  endif

  if (nargout == 3)
    rx = xi;
    ry = yi;
    rz = zi;
  elseif (nargout == 1)
    rx = zi;
  elseif (nargout == 0)
    mesh (xi, yi, zi);
  endif

endfunction


%!demo
%! clf;
%! colormap ("default");
%! x = 2*rand (100,1) - 1;
%! y = 2*rand (size (x)) - 1;
%! z = sin (2*(x.^2 + y.^2));
%! [xx,yy] = meshgrid (linspace (-1,1,32));
%! griddata (x,y,z,xx,yy);
%! title ("nonuniform grid sampled at 100 points");

%!demo
%! clf;
%! colormap ("default");
%! x = 2*rand (1000,1) - 1;
%! y = 2*rand (size (x)) - 1;
%! z = sin (2*(x.^2 + y.^2));
%! [xx,yy] = meshgrid (linspace (-1,1,32));
%! griddata (x,y,z,xx,yy);
%! title ("nonuniform grid sampled at 1000 points");

%!demo
%! clf;
%! colormap ("default");
%! x = 2*rand (1000,1) - 1;
%! y = 2*rand (size (x)) - 1;
%! z = sin (2*(x.^2 + y.^2));
%! [xx,yy] = meshgrid (linspace (-1,1,32));
%! griddata (x,y,z,xx,yy,"nearest");
%! title ("nonuniform grid sampled at 1000 points with nearest neighbor");

%!testif HAVE_QHULL
%! [xx,yy] = meshgrid (linspace (-1,1,32));
%! x = xx(:);
%! x = x + 10*(2*round (rand (size (x))) - 1) * eps;
%! y = yy(:);
%! y = y + 10*(2*round (rand (size (y))) - 1) * eps;
%! z = sin (2*(x.^2 + y.^2));
%! zz = griddata (x,y,z,xx,yy,"linear");
%! zz2 = sin (2*(xx.^2 + yy.^2));
%! zz2(isnan (zz)) = NaN;
%! assert (zz, zz2, 100*eps);

## Test input validation
%!error griddata ()
%!error griddata (1)
%!error griddata (1,2)
%!error griddata (1,2,3)
%!error griddata (1,2,3,4)
%!error griddata (1,2,3,4,5,6,7)
%!error <vectors of the same length> griddata (1:3, 1:3, 1:4, 1:3, 1:3)
%!error <vectors of the same length> griddata (1:3, 1:4, 1:3, 1:3, 1:3)
%!error <vectors of the same length> griddata (1:4, 1:3, 1:3, 1:3, 1:3)
%!error <the columns and rows of Z> griddata (1:4, 1:3, ones (4,4), 1:3, 1:3)
%!error <the columns and rows of Z> griddata (1:4, 1:3, ones (3,5), 1:3, 1:3)
%!error <matrices of same size> griddata (1:3, 1:3, 1:3, 1:4, 1:3)
%!error <matrices of same size> griddata (1:3, 1:3, 1:3, 1:3, 1:4)

