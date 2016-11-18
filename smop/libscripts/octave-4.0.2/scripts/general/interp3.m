## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Function File} {@var{vi} =} interp3 (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@var{v}, @var{n})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@var{v})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@dots{}, @var{method})
## @deftypefnx {Function File} {@var{vi} =} interp3 (@dots{}, @var{method}, @var{extrapval})
##
## Three-dimensional interpolation.
##
## Interpolate reference data @var{x}, @var{y}, @var{z}, @var{v} to determine
## @var{vi} at the coordinates @var{xi}, @var{yi}, @var{zi}.  The reference
## data @var{x}, @var{y}, @var{z} can be matrices, as returned by
## @code{meshgrid}, in which case the sizes of @var{x}, @var{y}, @var{z}, and
## @var{v} must be equal.  If @var{x}, @var{y}, @var{z} are vectors describing
## a cubic grid then @code{length (@var{x}) == columns (@var{v})},
## @code{length (@var{y}) == rows (@var{v})}, and
## @code{length (@var{z}) == size (@var{v}, 3)}.  In either case the input
## data must be strictly monotonic.
##
## If called without @var{x}, @var{y}, @var{z}, and just a single reference
## data matrix @var{v}, the 3-D region
## @code{@var{x} = 1:columns (@var{v}), @var{y} = 1:rows (@var{v}),
## @var{z} = 1:size (@var{v}, 3)} is assumed.
## This saves memory if the grid is regular and the distance between points is
## not important.
##
## If called with a single reference data matrix @var{v} and a refinement
## value @var{n}, then perform interpolation over a 3-D grid where each original
## interval has been recursively subdivided @var{n} times.  This results in
## @code{2^@var{n}-1} additional points for every interval in the original
## grid.  If @var{n} is omitted a value of 1 is used.  As an example, the
## interval [0,1] with @code{@var{n}==2} results in a refined interval with
## points at [0, 1/4, 1/2, 3/4, 1].
##
## The interpolation @var{method} is one of:
##
## @table @asis
## @item @qcode{"nearest"}
## Return the nearest neighbor.
##
## @item @qcode{"linear"} (default)
## Linear interpolation from nearest neighbors.
##
## @item @qcode{"cubic"}
## Piecewise cubic Hermite interpolating polynomial---shape-preserving
## interpolation with smooth first derivative (not implemented yet).
##
## @item @qcode{"spline"}
## Cubic spline interpolation---smooth first and second derivatives
## throughout the curve.
## @end table
##
## @var{extrapval} is a scalar number.  It replaces values beyond the endpoints
## with @var{extrapval}.  Note that if @var{extrapval} is used, @var{method}
## must be specified as well.  If @var{extrapval} is omitted and the
## @var{method} is @qcode{"spline"}, then the extrapolated values of the
## @qcode{"spline"} are used.  Otherwise the default @var{extrapval} value for
## any other @var{method} is @qcode{"NA"}.
## @seealso{interp1, interp2, interpn, meshgrid}
## @end deftypefn

## FIXME: Need to add support for 'cubic' method (maybe change interpn).

function vi = interp3 (varargin)

  narginchk (1,9);

  method = "linear";
  extrapval = [];
  nargs = nargin;

  if (nargin < 1 || ! isnumeric (varargin{1}))
    print_usage ();
  endif

  if (nargs > 1 && ischar (varargin{end-1}))
    if (! isnumeric (varargin{end}) || ! isscalar (varargin{end}))
      error ("interp3: EXTRAPVAL must be a numeric scalar");
    endif
    extrapval = varargin{end};
    method = varargin{end-1};
    nargs -= 2;
  elseif (ischar (varargin{end}))
    method = varargin{end};
    nargs--;
  endif

  if (method(1) == "*")
    warning ("interp3: ignoring unsupported '*' flag to METHOD");
    method(1) = [];
  endif
  method = validatestring (method, ...
    {"nearest", "linear", "cubic", "spline"});

  if (nargs < 3)
    ## Calling form interp3 (v) OR interp3 (v, n)
    v = varargin{1};
    if (ndims (v) != 3)
      error ("interp3: V must be a 3-D array of values");
    endif
    n = varargin(2:nargs);
    v = permute (v, [2, 1, 3]);
    if (isempty (extrapval))
      vi = interpn (v, n{:}, method);
    else
      vi = interpn (v, n{:}, method, extrapval);
    endif
    vi = ipermute (vi, [2, 1, 3]);

  elseif (nargs == 4 && ! isvector (varargin{1}))
    ## Calling form interp3 (v, xi, yi, zi)
    v = varargin{1};
    if (ndims (v) != 3)
      error ("interp3: V must be a 3-D array of values");
    endif
    xi = varargin(2:4);
    if (any (! cellfun (@isvector, xi)))
      ## Meshgridded values rather than vectors
      if (! size_equal (xi{:}))
        error ("interp3: XI, YI, and ZI dimensions must be equal");
      endif
      for i = 1 : 3
        xi{i} = permute (xi{i}, [2, 1, 3]);
      endfor
    endif
    v = permute (v, [2, 1, 3]);
    if (isempty (extrapval))
      vi = interpn (v, xi{:}, method);
    else
      vi = interpn (v, xi{:}, method, extrapval);
    endif
    vi = ipermute (vi, [2, 1, 3]);

  elseif (nargs == 7)
    ## Calling form interp3 (x, y, z, v, xi, yi, zi)
    v = varargin{4};
    if (ndims (v) != 3)
      error ("interp3: V must be a 3-D array of values");
    endif
    x = varargin(1:3);
    if (any (! cellfun (@isvector, x)))
      ## Meshgridded values rather than vectors
      if (! size_equal (x{:}, v))
        error ("interp3: X, Y, Z, and V dimensions must be equal");
      endif
      for i = 1 : 3
        x{i} = permute (x{i}, [2, 1, 3]);
      endfor
    endif
    xi = varargin(5:7);
    if (any (! cellfun (@isvector, xi)))
      ## Meshgridded values rather than vectors
      if (! size_equal (xi{:}))
        error ("interp3: XI, YI, and ZI dimensions must be equal");
      endif
      for i = 1 : 3
        xi{i} = permute (xi{i}, [2, 1, 3]);
      endfor
    endif
    v = permute (v, [2, 1, 3]);
    if (isempty (extrapval))
      vi = interpn (x{:}, v, xi{:}, method);
    else
      vi = interpn (x{:}, v, xi{:}, method, extrapval);
    endif
    vi = ipermute (vi, [2, 1, 3]);

  else
    error ("interp3: wrong number or incorrectly formatted input arguments");
  endif

endfunction


## FIXME: Need some demo blocks here to show off the function like interp2.m.

%!test  # basic test
%! x = y = z = -1:1;  y = y + 2;
%! f = @(x,y,z) x.^2 - y - z.^2;
%! [xx, yy, zz] = meshgrid (x, y, z);
%! v = f (xx,yy,zz);
%! xi = yi = zi = -1:0.5:1;  yi = yi + 2.1;
%! [xxi, yyi, zzi] = meshgrid (xi, yi, zi);
%! vi = interp3 (x, y, z, v, xxi, yyi, zzi);
%! [xxi, yyi, zzi] = ndgrid (yi, xi, zi);
%! vi2 = interpn (y, x, z, v, xxi, yyi, zzi);
%! assert (vi, vi2, 10*eps);

%!test  # meshgridded xi, yi, zi
%! x = z = 1:2;  y = 1:3;
%! v = ones ([3,2,2]);  v(:,2,1) = [7;5;4];  v(:,1,2) = [2;3;5];
%! xi = zi = .6:1.6;  yi = 1;
%! [xxi3, yyi3, zzi3] = meshgrid (xi, yi, zi);
%! [xxi, yyi, zzi] = ndgrid (yi, xi, zi);
%! vi = interp3 (x, y, z, v, xxi3, yyi3, zzi3, "nearest");
%! vi2 = interpn (y, x, z, v, xxi, yyi, zzi, "nearest");
%! assert (vi, vi2);

%!test  # vector xi, yi, zi
%! x = z = 1:2;  y = 1:3;
%! v = ones ([3,2,2]);  v(:,2,1) = [7;5;4];  v(:,1,2) = [2;3;5];
%! xi = zi = .6:1.6;  yi = 1;
%! vi = interp3 (x, y, z, v, xi, yi, zi, "nearest");
%! vi2 = interpn (y, x, z, v, yi, xi, zi,"nearest");
%! assert (vi, vi2);

%!test  # vector xi+1 with extrap value
%! x = z = 1:2;  y = 1:3;
%! v = ones ([3,2,2]);  v(:,2,1) = [7;5;4];  v(:,1,2) = [2;3;5];
%! xi = zi = .6:1.6;  yi = 1;
%! vi = interp3 (x, y, z, v, xi+1, yi, zi, "nearest", 3);
%! vi2 = interpn (y, x, z, v, yi, xi+1, zi, "nearest", 3);
%! assert (vi, vi2);

%!test  # input value matrix--no x,y,z
%! x = z = 1:2;  y = 1:3;
%! v = ones ([3,2,2]);  v(:,2,1) = [7;5;4];  v(:,1,2) = [2;3;5];
%! xi = zi = .6:1.6;  yi = 1;
%! vi = interp3 (v, xi, yi, zi, "nearest");
%! vi2 = interpn (v, yi, xi, zi,"nearest");
%! assert (vi, vi2);

%!test  # input value matrix--no x,y,z, with extrap value
%! x = z = 1:2;  y = 1:3;
%! v = ones ([3,2,2]);  v(:,2,1) = [7;5;4];  v(:,1,2) = [2;3;5];
%! xi = zi = .6:1.6;  yi = 1;
%! vi = interp3 (v, xi, yi, zi, "nearest", 3);
%! vi2 = interpn (v, yi, xi, zi, "nearest", 3);
%! assert (vi, vi2);

%!test # extrapolation
%! X=[0,0.5,1]; Y=X; Z=X;
%! V = zeros (3,3,3);
%! V(:,:,1) = [1 3 5; 3 5 7; 5 7 9];
%! V(:,:,2) = V(:,:,1) + 2;
%! V(:,:,3) = V(:,:,2) + 2;
%! tol = 10 * eps;
%! x=[-0.1,0,0.1]; y=x; z=x;
%! assert(interp3(X,Y,Z,V,x,y,z,"spline"), [-0.2, 1.0, 2.2]',tol);
%! assert(interp3(X,Y,Z,V,x,y,z,"linear"), [NA, 1.0, 2.2]',tol);
%! assert(interp3(X,Y,Z,V,x,y,z,"spline", 0), [0, 1.0, 2.2]',tol);
%! assert(interp3(X,Y,Z,V,x,y,z,"linear", 0), [0, 1.0, 2.2]',tol);

%!shared z, zout, tol
%! z = zeros (3, 3, 3);
%! zout = zeros (5, 5, 5);
%! z(:,:,1) = [1 3 5; 3 5 7; 5 7 9];
%! z(:,:,2) = z(:,:,1) + 2;
%! z(:,:,3) = z(:,:,2) + 2;
%! for n = 1:5
%!   zout(:,:,n) = [1 2 3 4 5;
%!                  2 3 4 5 6;
%!                  3 4 5 6 7;
%!                  4 5 6 7 8;
%!                  5 6 7 8 9] + (n-1);
%! end
%! tol = 10 * eps;
%!
%!assert (interp3 (z), zout, tol)
%!assert (interp3 (z, "linear"), zout, tol)
%!assert (interp3 (z, "spline"), zout, tol)

## Test input validation
%!error interp3 ()
%!error interp3 ({1})
%!error <EXTRAPVAL must be a numeric scalar> interp3 (1,2,3,4,1,2,3,"linear", {1})
%!error <EXTRAPVAL must be a numeric scalar> interp3 (1,2,3,4,1,2,3,"linear", ones (2,2))
%!warning <ignoring unsupported '\*' flag> interp3 (rand (3,3,3), 1, "*linear");
%!error <V must be a 3-D array> interp3 (rand (2,2))
%!error <V must be a 3-D array> interp3 (rand (2,2), 1,1,1)
%!error <XI, YI, and ZI dimensions must be equal> interp3 (rand (2,2,2), 1,1, ones (2,2))
%!error <V must be a 3-D array> interp3 (1:2, 1:2, 1:2, rand (2,2), 1,1,1)
%!error <X, Y, Z, and V dimensions must be equal> interp3 (ones(1,2,2), ones(2,2,2), ones(2,2,2), rand (2,2,2), 1,1,1)
%!error <XI, YI, and ZI dimensions must be equal> interp3 (1:2, 1:2, 1:2, rand (2,2,2), 1,1, ones (2,2))

