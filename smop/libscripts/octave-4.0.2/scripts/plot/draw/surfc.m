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
## @deftypefn  {Function File} {} surfc (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} surfc (@var{z})
## @deftypefnx {Function File} {} surfc (@dots{}, @var{c})
## @deftypefnx {Function File} {} surfc (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} surfc (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} surfc (@dots{})
## Plot a 3-D surface mesh with underlying contour lines.
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
## @seealso{ezsurfc, surf, surfl, surfnorm, trisurf, contour, mesh, surface, meshgrid, hidden, shading, colormap, caxis}
## @end deftypefn

function h = surfc (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("surfc", varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    surfc_props = {"facecolor", "flat"};
    chararg = find (cellfun ("isclass", varargin, "char"), 1);
    if (isempty (chararg))
      htmp = surface (varargin{:}, surfc_props{:});
    else
      htmp = surface (varargin{1:chararg-1}, surfc_props{:},
                      varargin{chararg:end});
    endif

    if (! ishold ())
      set (hax, "view", [-37.5, 30],
                "xgrid", "on", "ygrid", "on", "zgrid", "on",
                "xlimmode", "manual", "ylimmode", "manual");
    endif

    drawnow ();

    ## don't pass string arguments to __contour__()
    stop_idx = find (cellfun ("isclass", varargin, "char"), 1);
    if (isempty (stop_idx))
      stop_idx = nargin;
    else
      stop_idx--;
    endif

    if (stop_idx - 1 == 1 || stop_idx - 1 == 3)
      ## Don't pass a color matrix c to __contour__
      stop_idx -= 1;
    endif

    zmin = get (hax, "zlim")(1);
    [~, htmp2] = __contour__ (hax, zmin, varargin{1:stop_idx});

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
%! Z = peaks ();
%! surfc (Z);
%! title ('surfc() combines surf/contour plots');

%!demo
%! clf;
%! colormap ('default');
%! Z = sombrero ();
%! [Fx,Fy] = gradient (Z);
%! surfc (Z, Fx+Fy);
%! shading interp;
%! title ({'surfc() plot of sombrero() function'; ...
%!         'facecolor is interpolated, color determined by gradient of Z'});

%!demo
%! clf;
%! colormap ('default');
%! [X,Y,Z] = sombrero ();
%! [~,Fy] = gradient (Z);
%! surfc (X,Y,Z,Fy);
%! shading interp;
%! title ({'surfc() plot of peaks() function'; ...
%!         'facecolor is interpolated, color determined by Y-gradient of Z'});

