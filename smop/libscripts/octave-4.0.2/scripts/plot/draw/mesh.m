## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn  {Function File} {} mesh (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} mesh (@var{z})
## @deftypefnx {Function File} {} mesh (@dots{}, @var{c})
## @deftypefnx {Function File} {} mesh (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} mesh (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} mesh (@dots{})
## Plot a 3-D wireframe mesh.
##
## The wireframe mesh is plotted using rectangles.  The vertices of the
## rectangles [@var{x}, @var{y}] are typically the output of @code{meshgrid}.
## over a 2-D rectangular region in the x-y plane.  @var{z} determines the
## height above the plane of each vertex.  If only a single @var{z} matrix is
## given, then it is plotted over the meshgrid
## @code{@var{x} = 1:columns (@var{z}), @var{y} = 1:rows (@var{z})}.
## Thus, columns of @var{z} correspond to different @var{x} values and rows
## of @var{z} correspond to different @var{y} values.
##
## The color of the mesh is computed by linearly scaling the @var{z} values
## to fit the range of the current colormap.  Use @code{caxis} and/or
## change the colormap to control the appearance.
##
## Optionally, the color of the mesh can be specified independently of @var{z}
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
## @seealso{ezmesh, meshc, meshz, trimesh, contour, surf, surface, meshgrid, hidden, shading, colormap, caxis}
## @end deftypefn

## Author: jwe

function h = mesh (varargin)

  if (! all (cellfun ("isreal", varargin)))
    error ("mesh: X, Y, Z, C arguments must be real");
  endif

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("mesh", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    mesh_props = {"facecolor", "w", "edgecolor", "flat"};
    chararg = find (cellfun ("isclass", varargin, "char"), 1);
    if (isempty (chararg))
      htmp = surface (varargin{:}, mesh_props{:});
    else
      htmp = surface (varargin{1:chararg-1}, mesh_props{:},
                      varargin{chararg:end});
    endif

    if (! ishold ())
      set (hax, "view", [-37.5, 30],
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
%! x = logspace (0,1,11);
%! z = x'*x;
%! mesh (x, x, z);
%! xlabel 'X-axis';
%! ylabel 'Y-axis';
%! zlabel 'Z-axis';
%! title ('mesh() with color proportional to height');

%!demo
%! clf;
%! x = logspace (0,1,11);
%! z = x'*x;
%! mesh (x, x, z, z.^2);
%! xlabel 'X-axis';
%! ylabel 'Y-axis';
%! zlabel 'linear scale';
%! title ('mesh() with color proportional to Z^2');

%!demo
%! clf;
%! x = logspace (0,1,11);
%! z = x'*x;
%! mesh (x, x, z, z.^2);
%! set (gca, 'zscale', 'log');
%! xlabel 'X-axis';
%! ylabel 'Y-axis';
%! zlabel 'log scale';
%! title ({'mesh() with color proportional to Z^2', 'Z-axis is log scale'});
%! try
%!   if (strcmp (get (gcf, '__graphics_toolkit__'), 'gnuplot'))
%!     title ({'Gnuplot: mesh color is wrong', 'This is a Gnuplot bug'});
%!   endif
%! catch
%! end

%!demo
%! clf;
%! x = logspace (0,1,11);
%! z = x'*x;
%! mesh (x, x, z, 'facecolor', 'none', 'edgecolor', 'c');
%! xlabel 'X-axis';
%! ylabel 'Y-axis';
%! zlabel 'Z-axis';
%! title ({'mesh() default properties overridden', ...
%!         'transparent mesh with cyan color'});

