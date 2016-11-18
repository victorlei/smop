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
## @deftypefn  {Function File} {@var{vi} =} interpn (@var{x1}, @var{x2}, @dots{}, @var{v}, @var{y1}, @var{y2}, @dots{})
## @deftypefnx {Function File} {@var{vi} =} interpn (@var{v}, @var{y1}, @var{y2}, @dots{})
## @deftypefnx {Function File} {@var{vi} =} interpn (@var{v}, @var{m})
## @deftypefnx {Function File} {@var{vi} =} interpn (@var{v})
## @deftypefnx {Function File} {@var{vi} =} interpn (@dots{}, @var{method})
## @deftypefnx {Function File} {@var{vi} =} interpn (@dots{}, @var{method}, @var{extrapval})
##
## Perform @var{n}-dimensional interpolation, where @var{n} is at least two.
##
## Each element of the @var{n}-dimensional array @var{v} represents a value
## at a location given by the parameters @var{x1}, @var{x2}, @dots{}, @var{xn}.
## The parameters @var{x1}, @var{x2}, @dots{}, @var{xn} are either
## @var{n}-dimensional arrays of the same size as the array @var{v} in
## the @qcode{"ndgrid"} format or vectors.  The parameters @var{y1}, etc.
## respect a similar format to @var{x1}, etc., and they represent the points
## at which the array @var{vi} is interpolated.
##
## If @var{x1}, @dots{}, @var{xn} are omitted, they are assumed to be
## @code{x1 = 1 : size (@var{v}, 1)}, etc.  If @var{m} is specified, then
## the interpolation adds a point half way between each of the interpolation
## points.  This process is performed @var{m} times.  If only @var{v} is
## specified, then @var{m} is assumed to be @code{1}.
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
## @item @qcode{"pchip"}
## Piecewise cubic Hermite interpolating polynomial---shape-preserving
## interpolation with smooth first derivative (not implemented yet).
##
## @item @qcode{"cubic"}
## Cubic interpolation (same as @qcode{"pchip"} [not implemented yet]).
##
## @item @qcode{"spline"}
## Cubic spline interpolation---smooth first and second derivatives
## throughout the curve.
## @end table
##
## The default method is @qcode{"linear"}.
##
## @var{extrapval} is a scalar number.  It replaces values beyond the endpoints
## with @var{extrapval}.  Note that if @var{extrapval} is used, @var{method}
## must be specified as well.  If @var{extrapval} is omitted and the
## @var{method} is @qcode{"spline"}, then the extrapolated values of the
## @qcode{"spline"} are used.  Otherwise the default @var{extrapval} value for
## any other @var{method} is @qcode{"NA"}.
## @seealso{interp1, interp2, interp3, spline, ndgrid}
## @end deftypefn

function vi = interpn (varargin)

  method = "linear";
  extrapval = [];
  nargs = nargin;

  if (nargin < 1 || ! isnumeric (varargin{1}))
    print_usage ();
  endif

  if (nargs > 1 && ischar (varargin{end-1}))
    if (! isnumeric (varargin{end}) || ! isscalar (varargin{end}))
      error ("interpn: EXTRAPVAL must be a numeric scalar");
    endif
    extrapval = varargin{end};
    method = varargin{end-1};
    nargs -= 2;
  elseif (ischar (varargin{end}))
    method = varargin{end};
    nargs--;
  endif

  if (method(1) == "*")
    warning ("interpn: ignoring unsupported '*' flag to METHOD");
    method(1) = [];
  endif
  method = validatestring (method, ...
    {"nearest", "linear", "pchip", "cubic", "spline"});

  if (nargs < 3)
    v = varargin{1};
    m = 1;
    if (nargs == 2)
      if (ischar (varargin{2}))
        method = varargin{2};
      elseif (isnumeric (m) && isscalar (m) && fix (m) == m)
        m = varargin{2};
      else
        print_usage ();
      endif
    endif
    sz = size (v);
    nd = ndims (v);
    x = cell (1, nd);
    y = cell (1, nd);
    for i = 1 : nd
      x{i} = 1 : sz(i);
      y{i} = 1 : (1 / (2 ^ m)) : sz(i);
    endfor
    y{1} = y{1}.';
    [y{:}] = ndgrid (y{:});
  elseif (! isvector (varargin{1}) && nargs == (ndims (varargin{1}) + 1))
    v = varargin{1};
    sz = size (v);
    nd = ndims (v);
    x = cell (1, nd);
    y = varargin(2 : nargs);
    for i = 1 : nd
      x{i} = 1 : sz(i);
    endfor
  elseif (rem (nargs, 2) == 1
          && nargs == (2 * ndims (varargin{ceil (nargs / 2)})) + 1)
    nv = ceil (nargs / 2);
    v = varargin{nv};
    sz = size (v);
    nd = ndims (v);
    x = varargin(1 : (nv - 1));
    y = varargin((nv + 1) : nargs);
  else
    error ("interpn: wrong number or incorrectly formatted input arguments");
  endif

  if (any (! cellfun ("isvector", x)))
    for i = 2 : nd
      if (! size_equal (x{1}, x{i}) || ! size_equal (x{i}, v))
        error ("interpn: dimensional mismatch");
      endif
      idx(1 : nd) = {1};
      idx(i) = ":";
      x{i} = x{i}(idx{:})(:);
    endfor
    idx(1 : nd) = {1};
    idx(1) = ":";
    x{1} = x{1}(idx{:})(:);
  endif

  method = tolower (method);

  all_vectors = all (cellfun ("isvector", y));
  different_lengths = numel (unique (cellfun ("numel", y))) > 1;
  if (all_vectors && different_lengths)
    [foobar(1:numel(y)).y] = ndgrid (y{:});
    y = {foobar.y};
  endif

  if (strcmp (method, "linear"))
    vi = __lin_interpn__ (x{:}, v, y{:});
    if (isempty (extrapval))
      extrapval = NA;
    endif
    vi(isna (vi)) = extrapval;
  elseif (strcmp (method, "nearest"))
    yshape = size (y{1});
    yidx = cell (1, nd);
    for i = 1 : nd
      y{i} = y{i}(:);
      yidx{i} = lookup (x{i}, y{i}, "lr");
    endfor
    idx = cell (1,nd);
    for i = 1 : nd
      idx{i} = yidx{i} ...
               + (y{i} - x{i}(yidx{i})(:) >= x{i}(yidx{i} + 1)(:) - y{i});
    endfor
    vi = v(sub2ind (sz, idx{:}));
    idx = zeros (prod (yshape), 1);
    for i = 1 : nd
      idx |= y{i} < min (x{i}(:)) | y{i} > max (x{i}(:));
    endfor
    if (isempty (extrapval))
      extrapval = NA;
    endif
    vi(idx) = extrapval;
    vi = reshape (vi, yshape);
  elseif (strcmp (method, "spline"))
    if (any (! cellfun ("isvector", y)))
      for i = 2 : nd
        if (! size_equal (y{1}, y{i}))
          error ("interpn: dimensional mismatch");
        endif
        idx(1 : nd) = {1};
        idx(i) = ":";
        y{i} = y{i}(idx{:});
      endfor
      idx(1 : nd) = {1};
      idx(1) = ":";
      y{1} = y{1}(idx{:});
    endif

    vi = __splinen__ (x, v, y, extrapval, "interpn");

    if (size_equal (y{:}))
      ly = length (y{1});
      idx = cell (1, ly);
      q = cell (1, nd);
      for i = 1 : ly
        q(:) = i;
        idx{i} = q;
      endfor
      vi = vi(cellfun (@(x) sub2ind (size (vi), x{:}), idx));
      vi = reshape (vi, size (y{1}));
    endif
  elseif (strcmp (method, "cubic"))
    error ("interpn: cubic interpolation not yet implemented");
  else
    error ("interpn: unrecognized interpolation METHOD");
  endif

endfunction


%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,4];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi, yi, interpn (x,y,A.',xi,yi, "linear").');
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x(:),y(:),A(:),"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,4];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi, yi, interpn (x,y,A.',xi,yi, "nearest").');
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x(:),y(:),A(:),"b*"); hold off;

%!#demo  # FIXME: Uncomment when support for "cubic" has been added
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,2];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi, yi, interpn (x,y,A.',xi,yi, "cubic").');
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x(:),y(:),A(:),"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,2];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi, yi, interpn (x,y,A.',xi,yi, "spline").');
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x(:),y(:),A(:),"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! x = y = z = -1:1;
%! f = @(x,y,z) x.^2 - y - z.^2;
%! [xx, yy, zz] = meshgrid (x, y, z);
%! v = f (xx,yy,zz);
%! xi = yi = zi = -1:0.1:1;
%! [xxi, yyi, zzi] = ndgrid (xi, yi, zi);
%! vi = interpn (x, y, z, v, xxi, yyi, zzi, "spline");
%! mesh (yi, zi, squeeze (vi(1,:,:)));

%!test
%! [x,y,z] = ndgrid (0:2);
%! f = x + y + z;
%! assert (interpn (x,y,z,f,[.5 1.5],[.5 1.5],[.5 1.5]), [1.5, 4.5]);
%! assert (interpn (x,y,z,f,[.51 1.51],[.51 1.51],[.51 1.51],"nearest"), [3, 6]);
%! assert (interpn (x,y,z,f,[.5 1.5],[.5 1.5],[.5 1.5],"spline"), [1.5, 4.5]);
%! assert (interpn (x,y,z,f,x,y,z), f);
%! assert (interpn (x,y,z,f,x,y,z,"nearest"), f);
%! assert (interpn (x,y,z,f,x,y,z,"spline"), f);

%!test
%! [x, y, z] = ndgrid (0:2, 1:4, 2:6);
%! f = x + y + z;
%! xi = [0.5 1.0 1.5];  yi = [1.5 2.0 2.5 3.5];  zi = [2.5 3.5 4.0 5.0 5.5];
%! fi = interpn (x, y, z, f, xi, yi, zi);
%! [xi, yi, zi] = ndgrid (xi, yi, zi);
%! assert (fi, xi + yi + zi);

%!test
%! xi = 0:2;  yi = 1:4;  zi = 2:6;
%! [x, y, z] = ndgrid (xi, yi, zi);
%! f = x + y + z;
%! fi = interpn (x, y, z, f, xi, yi, zi, "nearest");
%! assert (fi, x + y + z);

%!test
%! [x,y,z] = ndgrid (0:2);
%! f = x.^2 + y.^2 + z.^2;
%! assert (interpn (x,y,-z,f,1.5,1.5,-1.5), 7.5);

%!test  # for Matlab-compatible rounding for "nearest"
%! x = meshgrid (1:4);
%! assert (interpn (x, 2.5, 2.5, "nearest"), 3);

%!test
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
%! endfor
%! tol = 10*eps;
%! assert (interpn (z), zout, tol);
%! assert (interpn (z, "linear"), zout, tol);
%! assert (interpn (z, "spline"), zout, tol);

## Test input validation
%!warning <ignoring unsupported '\*' flag> interpn (rand (3,3), 1, "*linear");

