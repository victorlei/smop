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
## @deftypefn  {Function File} {} meshc (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} meshc (@var{z})
## @deftypefnx {Function File} {} meshc (@dots{}, @var{c})
## @deftypefnx {Function File} {} meshc (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} meshc (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} meshc (@dots{})
## Plot a 3-D wireframe mesh with underlying contour lines.
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
## Optionally the color of the mesh can be specified independently of @var{z}
## by supplying a color matrix, @var{c}.
##
## Any property/value pairs are passed directly to the underlying surface
## object.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a 2-element vector with a graphics
## handle to the created surface object and to the created contour plot.
##
## @seealso{ezmeshc, mesh, meshz, contour, surfc, surface, meshgrid, hidden, shading, colormap, caxis}
## @end deftypefn

function h = meshc (varargin)

  if (! all (cellfun ("isreal", varargin)))
    error ("meshc: X, Y, Z, C arguments must be real");
  endif

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("meshc", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    ## FIXME: gnuplot does not support a filled surface and a
    ##        non-filled contour.  3D filled patches are also not supported.
    ##        Thus, the facecolor will be transparent for the gnuplot backend.
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
                "xgrid", "on", "ygrid", "on", "zgrid", "on",
                "xlimmode", "manual", "ylimmode", "manual");
    endif

    drawnow ();

    zmin = get (hax, "zlim")(1);
    [~, htmp2] = __contour__ (hax, zmin, varargin{:});

    htmp = [htmp; htmp2];

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
%! [X, Y] = meshgrid (linspace (-3, 3, 40));
%! Z = sqrt (abs (X .* Y)) ./ (1 + X.^2 + Y.^2);
%! meshc (X, Y, Z);
%! title ('meshc() combines mesh/contour plots');

