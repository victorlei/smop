## Copyright (C) 2009-2015 Kai Habel
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
## @deftypefn  {Function File} {} surfl (@var{z})
## @deftypefnx {Function File} {} surfl (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} surfl (@dots{}, @var{lsrc})
## @deftypefnx {Function File} {} surfl (@var{x}, @var{y}, @var{z}, @var{lsrc}, @var{P})
## @deftypefnx {Function File} {} surfl (@dots{}, "cdata")
## @deftypefnx {Function File} {} surfl (@dots{}, "light")
## @deftypefnx {Function File} {} surfl (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} surfl (@dots{})
## Plot a 3-D surface using shading based on various lighting models.
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
## The default lighting mode @qcode{"cdata"}, changes the cdata property of the
## surface object to give the impression of a lighted surface.
## @strong{Warning:} The alternative mode @qcode{"light"} mode which creates a
## light object to illuminate the surface is not implemented (yet).
##
## The light source location can be specified using @var{lsrc}.  It can be given
## as a 2-element vector [azimuth, elevation] in degrees, or as a 3-element
## vector [lx, ly, lz].  The default value is rotated 45 degrees
## counterclockwise to the current view.
##
## The material properties of the surface can specified using a 4-element
## vector @var{P} = [@var{AM} @var{D} @var{SP} @var{exp}] which defaults to
## @var{p} = [0.55 0.6 0.4 10].
##
## @table @asis
## @item @qcode{"AM"} strength of ambient light
##
## @item @qcode{"D"} strength of diffuse reflection
##
## @item @qcode{"SP"} strength of specular reflection
##
## @item @qcode{"EXP"} specular exponent
## @end table
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
##
## Example:
##
## @example
## @group
## colormap (bone (64));
## surfl (peaks);
## shading interp;
## @end group
## @end example
## @seealso{diffuse, specular, surf, shading, colormap, caxis}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function h = surfl (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("surfl", varargin{:});

  if (nargin == 0)
    print_usage ();
  endif

  ## Check for lighting type.
  use_cdata = true;
  if (ischar (varargin{end}))
    switch (tolower (varargin{end}))
      case "light"
        warning ("surfl: light method not supported (yet), using cdata method instead");
        ## This can be implemented when light objects are supported.
        use_cdata = false;
      case "cdata"
        use_cdata = true;
      otherwise
        error ("surfl: unknown lighting method");
    endswitch
    varargin(end) = [];
  endif

  ## Check for reflection properties argument.
  ##
  ## r = [ambient light strength,
  ##      diffuse reflection strength,
  ##      specular reflection strength,
  ##      specular shine]
  if (isnumeric (varargin{end}) && length (varargin{end}) == 4)
    r = varargin{end};
    varargin(end) = [];
  else
    ## Default values.
    r = [0.55, 0.6, 0.4, 10];
  endif

  ## Check for light vector (lv) argument.
  have_lv = false;
  if (isnumeric (varargin{end}))
    len = numel (varargin{end});
    lastarg = varargin{end};
    if (len == 3)
      lv = lastarg;
      varargin(end) = [];
      have_lv = true;
    elseif (len == 2)
      [lv(1), lv(2), lv(3)] = sph2cart ((lastarg(1) - 90) * pi/180,
                                         lastarg(2) * pi/180,
                                         1.0);
      varargin(end) = [];
      have_lv = true;
    endif
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    htmp = surface (varargin{:});
    if (! ishold (hax))
      set (hax, "view", [-37.5, 30],
                "xgrid", "on", "ygrid", "on", "zgrid", "on", "clim", [0 1]);
    endif

    ## Get view vector (vv).
    [az, el] = view ();
    vv = sph2cart ((az - 90) * pi/180.0, el * pi/180.0, 1.0);

    if (! have_lv)
      ## Calculate light vector (lv) from view vector.
      phi = pi / 4;  # 45 degrees
      R = [cos(phi), -sin(phi), 0;
           sin(phi),  cos(phi), 0;
           0,         0,        1];
      lv = (R * vv.').';
    endif

    vn = get (htmp, "vertexnormals");
    dar = get (hax, "dataaspectratio");
    vn(:,:,1) *= dar(1);
    vn(:,:,2) *= dar(2);
    vn(:,:,3) *= dar(3);

    ## Normalize vn.
    vn ./= repmat (sqrt (sumsq (vn, 3)), [1, 1, 3]);
    [nr, nc] = size (get (htmp, "zdata"));

    ## Ambient, diffuse, and specular term.
    cdata = (  r(1) * ones (nr, nc)
             + r(2) * diffuse  (vn(:,:,1), vn(:,:,2), vn(:,:,3), lv)
             + r(3) * specular (vn(:,:,1), vn(:,:,2), vn(:,:,3), lv, vv, r(4)));
    cdata ./= sum (r(1:3));

    set (htmp, "cdata", cdata);

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
%! [X,Y,Z] = sombrero ();
%! colormap (copper (64));
%! surfl (X,Y,Z);
%! shading interp;
%! title ('surfl() with defaults');

%!demo
%! clf;
%! [X,Y,Z] = sombrero ();
%! colormap (copper (64));
%! surfl (X,Y,Z, [62.50,30], [0.2 0.6 0.4 25]);
%! shading interp;
%! title ('surfl() with lighting vector and material properties');

