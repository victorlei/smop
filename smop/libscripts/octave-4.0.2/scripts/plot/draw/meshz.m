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
## @deftypefn  {Function File} {} meshz (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} meshz (@var{z})
## @deftypefnx {Function File} {} meshz (@dots{}, @var{c})
## @deftypefnx {Function File} {} meshz (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} meshz (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} meshz (@dots{})
## Plot a 3-D wireframe mesh with a surrounding curtain.
##
## The wireframe mesh is plotted using rectangles.  The vertices of the
## rectangles [@var{x}, @var{y}] are typically the output of @code{meshgrid}.
## over a 2-D rectangular region in the x-y plane.  @var{z} determines the
## height above the plane of each vertex.  If only a single @var{z} matrix is
## given, then it is plotted over the meshgrid
## @code{@var{x} = 0:columns (@var{z}) - 1, @var{y} = 0:rows (@var{z}) - 1}.
## Thus, columns of @var{z} correspond to different @var{x} values and rows
## of @var{z} correspond to different @var{y} values.
##
## The color of the mesh is computed by linearly scaling the @var{z} values
## to fit the range of the current colormap.  Use @code{caxis} and/or
## change the colormap to control the appearance.
##
## Optionally the color of the mesh can be specified independently of @var{z}
## by supplying a color matrix, @var{c}.
##
## Any property/value pairs are passed directly to the underlying surface
## object.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
##
## @seealso{mesh, meshc, contour, surf, surface, waterfall, meshgrid, hidden, shading, colormap, caxis}
## @end deftypefn

function h = meshz (varargin)

  if (! all (cellfun ("isreal", varargin)))
    error ("meshz: X, Y, Z, C arguments must be real");
  endif

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("meshz", varargin{:});

  ## Find where property/value pairs start
  charidx = find (cellfun ("isclass", varargin, "char"), 1);

  if (isempty (charidx))
    charidx = nargin + 1;
  endif

  if (nargin == 1)
    z = varargin{1};
    [m, n] = size (z);
    x = 0:(n-1);
    y = (0:(m-1)).';
    c = z;
  elseif (nargin == 2)
    z = varargin{1};
    [m, n] = size (z);
    x = 0:(n-1);
    y = (0:(m-1)).';
    c = varargin{2};
  elseif (charidx == 4)
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
    c = z;
  else
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
    c = varargin{4};
  endif

  ## Create a border of one rectangle (2 points) as the curtain around
  ## the data and draw it with the mean (max + min / 2) color of the data.

  if (isvector (x) && isvector (y))
    x = [x(1), x(1), x(:).', x(end), x(end)];
    y = [y(1); y(1); y(:); y(end); y(end)];
  else
    x = [x(1,1), x(1,1), x(1,:), x(1,end), x(1,end);
         x(1,1), x(1,1), x(1,:), x(1,end), x(1,end);
         x(:,1), x(:,1), x, x(:,end), x(:,end);
         x(end,1), x(end,1), x(end,:), x(end,end), x(end,end);
         x(end,1), x(end,1), x(end,:), x(end,end), x(end,end) ];
    y = [y(1,1), y(1,1), y(1,:), y(1,end), y(1,end);
         y(1,1), y(1,1), y(1,:), y(1,end), y(1,end);
         y(:,1), y(:,1), y, y(:,end), y(:,end);
         y(end,1), y(end,1), y(end,:), y(end,end), y(end,end);
         y(end,1), y(end,1), y(end,:), y(end,end), y(end,end) ];
  endif

  zref = min (z(isfinite (z)));
  z = [zref .* ones(1, columns(z) + 4);
       zref .* ones(1, 2), z(1,:), zref .* ones(1, 2);
       zref .* ones(rows(z), 1), z(:,1),z, z(:,end), zref .* ones(rows(z), 1);
       zref .* ones(1, 2), z(end,:), zref .* ones(1, 2);
       zref .* ones(1, columns(z) + 4)];

  cdat = c(isfinite (c(:)));
  cref = (min (cdat) + max (cdat)) / 2;
  c = [cref .* ones(2, columns(c) + 4);
       cref .* ones(rows(c), 2), c, cref .* ones(rows(c), 2);
       cref .* ones(2, columns(c) + 4)];

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    htmp = mesh (x, y, z, c, varargin{charidx:end});
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
%! Z = peaks ();
%! meshz (Z);
%! title ('meshz() plot of peaks() function');

%!demo
%! clf;
%! colormap ('default');
%! Z = peaks ();
%! subplot (1,2,1)
%!  mesh (Z);
%!  daspect ([2.5, 2.5, 1]);
%!  title ('mesh() plot');
%! subplot (1,2,2)
%!  meshz (Z);
%!  daspect ([2.5, 2.5, 1]);
%!  title ('meshz() plot');

%!demo
%! clf;
%! colormap ('default');
%! [X,Y,Z] = peaks ();
%! [fx, fy] = gradient (Z);
%! C = sqrt (fx.^2 + fy.^2);
%! meshz (X,Y,Z,C);
%! title ('meshz() plot with color determined by gradient');

