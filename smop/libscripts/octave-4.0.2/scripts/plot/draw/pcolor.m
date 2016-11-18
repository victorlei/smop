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
## @deftypefn  {Function File} {} pcolor (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} pcolor (@var{c})
## @deftypefnx {Function File} {} pcolor (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} pcolor (@dots{})
## Produce a 2-D density plot.
##
## A @code{pcolor} plot draws rectangles with colors from the matrix @var{c}
## over the two-dimensional region represented by the matrices @var{x} and
## @var{y}.  @var{x} and @var{y} are the coordinates of the mesh's vertices
## and are typically the output of @code{meshgrid}.  If @var{x} and @var{y} are
## vectors, then a typical vertex is (@var{x}(j), @var{y}(i), @var{c}(i,j)).
## Thus, columns of @var{c} correspond to different @var{x} values and rows
## of @var{c} correspond to different @var{y} values.
##
## The values in @var{c} are scaled to span the range of the current
## colormap.  Limits may be placed on the color axis by the command
## @code{caxis}, or by setting the @code{clim} property of the parent axis.
##
## The face color of each cell of the mesh is determined by interpolating
## the values of @var{c} for each of the cell's vertices; Contrast this with
## @code{imagesc} which renders one cell for each element of @var{c}.
##
## @code{shading} modifies an attribute determining the manner by which the
## face color of each cell is interpolated from the values of @var{c},
## and the visibility of the cells' edges.  By default the attribute is
## @qcode{"faceted"}, which renders a single color for each cell's face with
## the edge visible.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
##
## @seealso{caxis, shading, meshgrid, contour, imagesc}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function h = pcolor (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("pcolor", varargin{:});

  if (nargin == 1)
    c = varargin{1};
    [nr, nc] = size (c);
    x = 1:nc;
    y = 1:nr;
    z = zeros (nr, nc);
  elseif (nargin == 3)
    x = varargin{1};
    y = varargin{2};
    c = varargin{3};
    z = zeros (size (c));
  else
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    htmp = surface (x, y, z, c);

    set (htmp, "facecolor", "flat");
    if (! ishold ())
      set (hax, "view", [0, 90], "box", "on");
      ## FIXME: Maybe this should be in the general axis limit setting routine?
      ##        When values are integers (such as from meshgrid), we want to
      ##        use tight limits for pcolor, mesh, surf, etc.  Situation is
      ##        complicated immensely by vector or matrix input and meshgrid()
      ##        or ndgrid() format.
      meshgrid_fmt = true;
      if (isvector (x))
        xrng = x(isfinite (x));
      else
        xrng = x(1, isfinite (x(1,:)));    # meshgrid format (default)
        if (all (xrng == xrng(1)))
          xrng = x(isfinite (x(:,1)), 1);  # ndgrid format
          meshgrid_fmt = false;
        endif
      endif
      if (isvector (y))
        yrng = y(isfinite (y));
      else
        if (meshgrid_fmt)
          yrng = y(isfinite (y(:,1)), 1);
        else
          yrng = y(1, isfinite (y(1,:)));
        endif
      endif
      if (all (xrng == fix (xrng)))
        xmin = min (xrng);
        xmax = max (xrng);
        if (xmin < xmax)
          xlim ([xmin, xmax]);
        endif
      endif
      if (all (yrng == fix (yrng)))
        ymin = min (yrng);
        ymax = max (yrng);
        if (ymin < ymax)
          ylim ([ymin, ymax]);
        endif
      endif
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
%! Z = peaks ();
%! pcolor (Z);
%! title ('pcolor() of peaks with facet shading');

%!demo
%! clf;
%! colormap ('default');
%! [X,Y,Z] = sombrero ();
%! [Fx,Fy] = gradient (Z);
%! pcolor (X,Y,Fx+Fy);
%! shading interp;
%! axis tight;
%! title ('pcolor() of peaks with interp shading');

