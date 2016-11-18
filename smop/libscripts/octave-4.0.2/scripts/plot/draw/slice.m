## Copyright (C) 2007-2015 Kai Habel, David Bateman
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
## @deftypefn  {Function File} {} slice (@var{x}, @var{y}, @var{z}, @var{v}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {Function File} {} slice (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {Function File} {} slice (@var{v}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {Function File} {} slice (@var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {Function File} {} slice (@dots{}, @var{method})
## @deftypefnx {Function File} {} slice (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} slice (@dots{})
## Plot slices of 3-D data/scalar fields.
##
## Each element of the 3-dimensional array @var{v} represents a scalar value at
## a location given by the parameters @var{x}, @var{y}, and @var{z}.  The
## parameters @var{x}, @var{x}, and @var{z} are either 3-dimensional arrays of
## the same size as the array @var{v} in the @qcode{"meshgrid"} format or
## vectors.  The parameters @var{xi}, etc. respect a similar format to
## @var{x}, etc., and they represent the points at which the array @var{vi}
## is interpolated using interp3.  The vectors @var{sx}, @var{sy}, and
## @var{sz} contain points of orthogonal slices of the respective axes.
##
## If @var{x}, @var{y}, @var{z} are omitted, they are assumed to be
## @code{x = 1:size (@var{v}, 2)}, @code{y = 1:size (@var{v}, 1)} and
## @code{z = 1:size (@var{v}, 3)}.
##
## @var{method} is one of:
##
## @table @asis
## @item @qcode{"nearest"}
## Return the nearest neighbor.
##
## @item @qcode{"linear"}
## Linear interpolation from nearest neighbors.
##
## @item @qcode{"cubic"}
## Cubic interpolation from four nearest neighbors (not implemented yet).
##
## @item @qcode{"spline"}
## Cubic spline interpolation---smooth first and second derivatives
## throughout the curve.
## @end table
##
## The default method is @qcode{"linear"}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
##
## Examples:
##
## @example
## @group
## [x, y, z] = meshgrid (linspace (-8, 8, 32));
## v = sin (sqrt (x.^2 + y.^2 + z.^2)) ./ (sqrt (x.^2 + y.^2 + z.^2));
## slice (x, y, z, v, [], 0, []);
##
## [xi, yi] = meshgrid (linspace (-7, 7));
## zi = xi + yi;
## slice (x, y, z, v, xi, yi, zi);
## @end group
## @end example
## @seealso{interp3, surface, pcolor}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function h = slice (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("slice", varargin{:});

  method = "linear";

  if (ischar (varargin{end}))
    method = varargin{end};
    nargs -= 1;
  endif

  if (nargs == 4)
    v = varargin{1};
    if (ndims (v) != 3)
      error ("slice: V must be a 3-dimensional array of values");
    endif
    [nx, ny, nz] = size (v);
    [x, y, z] = meshgrid (1:nx, 1:ny, 1:nz);
    sx = varargin{2};
    sy = varargin{3};
    sz = varargin{4};
  elseif (nargs == 7)
    v = varargin{4};
    if (ndims (v) != 3)
      error ("slice: V must be a 3-dimensional array of values");
    endif
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
    if (isvector (x) && isvector (y) && isvector (z))
      [x, y, z] = meshgrid (x, y, z);
    elseif (ndims (x) == 3 && size_equal (x, y, z))
      ## Do nothing.
    else
      error ("slice: X, Y, Z size mismatch");
    endif
    sx = varargin{5};
    sy = varargin{6};
    sz = varargin{7};
  else
    print_usage ();
  endif

  if (any ([isvector(sx), isvector(sy), isvector(sz)]))
    have_sval = true;
  elseif (ndims (sx) == 2 && size_equal (sx, sy, sz))
    have_sval = false;
  else
    error ("slice: dimensional mismatch for (XI, YI, ZI) or (SX, SY, SZ)");
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    sidx = 1;
    minv = min (v(:));
    maxv = max (v(:));
    set (hax, "clim", [minv, maxv]);

    if (have_sval)
      ns = length (sx) + length (sy) + length (sz);
      hs = zeros (ns,1);
      [ny, nx, nz] = size (v);
      if (length (sz) > 0)
        for i = 1:length (sz)
          [xi, yi, zi] = meshgrid (squeeze (x(1,:,1)),
                                   squeeze (y(:,1,1)), sz(i));
          vz = squeeze (interp3 (x, y, z, v, xi, yi, zi, method));
          htmp(sidx++) = surface (xi, yi, sz(i) * ones (size (yi)), vz);
        endfor
      endif

      if (length (sy) > 0)
        for i = length (sy):-1:1
          [xi, yi, zi] = meshgrid (squeeze (x(1,:,1)),
                                   sy(i),
                                   squeeze (z(1,1,:)));
          vy = squeeze (interp3 (x, y, z, v, xi, yi, zi, method));
          htmp(sidx++) = surface (squeeze (xi),
                                  squeeze (sy(i) * ones (size (zi))),
                                  squeeze (zi), vy);
        endfor
      endif

      if (length (sx) > 0)
        for i = length (sx):-1:1
          [xi, yi, zi] = meshgrid (sx(i), squeeze (y(:,1,1)), squeeze (z(1,1,:)));
          vx = squeeze (interp3 (x, y, z, v, xi, yi, zi, method));
          htmp(sidx++) = surface (squeeze (sx(i) * ones (size (zi))),
                                  squeeze (yi), squeeze(zi), vx);
        endfor
      endif
    else
      vi = interp3 (x, y, z, v, sx, sy, sz);
      htmp = surface (sx, sy, sz, vi);
    endif

    if (! ishold ())
      set (hax, "view", [-37.5, 30.0], "box", "off",
                "xgrid", "on", "ygrid", "on", "zgrid", "on");
    endif

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! [x, y, z] = meshgrid (linspace (-8, 8, 32));
%! v = sin (sqrt (x.^2 + y.^2 + z.^2)) ./ (sqrt (x.^2 + y.^2 + z.^2));
%! slice (x, y, z, v, [], 0, []);

%!demo
%! clf;
%! colormap ('default');
%! [x, y, z] = meshgrid (linspace (-8, 8, 32));
%! v = sin (sqrt (x.^2 + y.^2 + z.^2)) ./ (sqrt (x.^2 + y.^2 + z.^2));
%! [xi, yi] = meshgrid (linspace (-7, 7));
%! zi = xi + yi;
%! slice (x, y, z, v, xi, yi, zi);

