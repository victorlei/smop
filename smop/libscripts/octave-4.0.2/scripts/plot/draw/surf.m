## Copyright (C) 2007-2015 Kai Habel
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
## @deftypefn  {Function File} {} surf (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} surf (@var{z})
## @deftypefnx {Function File} {} surf (@dots{}, @var{c})
## @deftypefnx {Function File} {} surf (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} surf (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} surf (@dots{})
## Plot a 3-D surface mesh.
##
## The surface mesh is plotted using shaded rectangles.  The vertices of the
## rectangles [@var{x}, @var{y}] are typically the output of @code{meshgrid}.
## over a 2-D rectangular region in the x-y plane.  @var{z} determines the
## height above the plane of each vertex.  If only a single @var{z} matrix is
## given, then it is plotted over the meshgrid
## @code{@var{x} = 1:columns (@var{z}), @var{y} = 1:rows (@var{z})}.
## Thus, columns of @var{z} correspond to different @var{x} values and rows
## of @var{z} correspond to different @var{y} values.
##
## The color of the surface is computed by linearly scaling the @var{z} values
## to fit the range of the current colormap.  Use @code{caxis} and/or
## change the colormap to control the appearance.
##
## Optionally, the color of the surface can be specified independently of
## @var{z} by supplying a color matrix, @var{c}.
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
## Note: The exact appearance of the surface can be controlled with the
## @code{shading} command or by using @code{set} to control surface object
## properties.
## @seealso{ezsurf, surfc, surfl, surfnorm, trisurf, contour, mesh, surface, meshgrid, hidden, shading, colormap, caxis}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function retval = surf (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("surf", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    htmp = surface (varargin{:});

    if (! ishold (hax))
      set (hax, "view", [-37.5, 30],
                "xgrid", "on", "ygrid", "on", "zgrid", "on");
    endif

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    retval = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! Z = peaks ();
%! surf (Z);
%! title ({'surf() plot of peaks() function'; 'color determined by height Z'});

%!demo
%! clf;
%! colormap ('default');
%! Z = sombrero ();
%! [Fx,Fy] = gradient (Z);
%! surf (Z, Fx+Fy);
%! shading interp;
%! title ({'surf() plot of peaks() function'; ...
%!         'facecolor is interpolated, color determined by gradient of Z'});

%!demo
%! clf;
%! colormap ('default');
%! [X,Y,Z] = sombrero ();
%! [~,Fy] = gradient (Z);
%! surf (X, Y, Z, Fy);
%! shading interp;
%! title ({'surf() plot of peaks() function'; ...
%!         'facecolor is interpolated, color determined by Y-gradient of Z'});

