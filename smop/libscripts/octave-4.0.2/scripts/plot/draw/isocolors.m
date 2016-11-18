## Copyright (C) 2009-2015 Martin Helm
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
## @deftypefn  {Function File} {[@var{cd}] =} isocolors (@var{c}, @var{v})
## @deftypefnx {Function File} {[@var{cd}] =} isocolors (@var{x}, @var{y}, @var{z}, @var{c}, @var{v})
## @deftypefnx {Function File} {[@var{cd}] =} isocolors (@var{x}, @var{y}, @var{z}, @var{r}, @var{g}, @var{b}, @var{v})
## @deftypefnx {Function File} {[@var{cd}] =} isocolors (@var{r}, @var{g}, @var{b}, @var{v})
## @deftypefnx {Function File} {[@var{cd}] =} isocolors (@dots{}, @var{p})
## @deftypefnx {Function File} {} isocolors (@dots{})
##
## Compute isosurface colors.
##
## If called with one output argument and the first input argument
## @var{c} is a three-dimensional array that contains color values and
## the second input argument @var{v} keeps the vertices of a geometry
## then return a matrix @var{cd} with color data information for the
## geometry at computed points
## @command{[x, y, z] = meshgrid (1:l, 1:m, 1:n)}.  The output argument
## @var{cd} can be taken to manually set FaceVertexCData of a patch.
##
## If called with further input arguments @var{x}, @var{y} and @var{z}
## which are three--dimensional arrays of the same size than @var{c}
## then the color data is taken at those given points.  Instead of the
## color data @var{c} this function can also be called with RGB values
## @var{r}, @var{g}, @var{b}.  If input argumnets @var{x}, @var{y},
## @var{z} are not given then again @command{meshgrid} computed values
## are taken.
##
## Optionally, the patch handle @var{p} can be given as the last input
## argument to all variations of function calls instead of the vertices
## data @var{v}.  Finally, if no output argument is given then directly
## change the colors of a patch that is given by the patch handle
## @var{p}.
##
## For example:
##
## @example
## function [] = isofinish (p)
##   set (gca, "PlotBoxAspectRatioMode", "manual", ...
##             "PlotBoxAspectRatio", [1 1 1]);
##   set (p, "FaceColor", "interp");
##   ## set (p, "FaceLighting", "flat");
##   ## light ("Position", [1 1 5]);  # Available with JHandles
## endfunction
##
## N = 15;    # Increase number of vertices in each direction
## iso = .4;  # Change isovalue to .1 to display a sphere
## lin = linspace (0, 2, N);
## [x, y, z] = meshgrid (lin, lin, lin);
## c = abs ((x-.5).^2 + (y-.5).^2 + (z-.5).^2);
## figure (); # Open another figure window
##
## subplot (2,2,1); view (-38, 20);
## [f, v] = isosurface (x, y, z, c, iso);
## p = patch ("Faces", f, "Vertices", v, "EdgeColor", "none");
## cdat = rand (size (c));       # Compute random patch color data
## isocolors (x, y, z, cdat, p); # Directly set colors of patch
## isofinish (p);                # Call user function isofinish
##
## subplot (2,2,2); view (-38, 20);
## p = patch ("Faces", f, "Vertices", v, "EdgeColor", "none");
## [r, g, b] = meshgrid (lin, 2-lin, 2-lin);
## cdat = isocolors (x, y, z, c, v); # Compute color data vertices
## set (p, "FaceVertexCData", cdat); # Set color data manually
## isofinish (p);
##
## subplot (2,2,3); view (-38, 20);
## p = patch ("Faces", f, "Vertices", v, "EdgeColor", "none");
## cdat = isocolors (r, g, b, c, p); # Compute color data patch
## set (p, "FaceVertexCData", cdat); # Set color data manually
## isofinish (p);
##
## subplot (2,2,4); view (-38, 20);
## p = patch ("Faces", f, "Vertices", v, "EdgeColor", "none");
## r = g = b = repmat ([1:N] / N, [N, 1, N]); # Black to white
## cdat = isocolors (x, y, z, r, g, b, v);
## set (p, "FaceVertexCData", cdat);
## isofinish (p);
## @end example
##
## @seealso{isosurface, isonormals}
## @end deftypefn

## Author: Martin Helm <martin@mhelm.de>

function varargout = isocolors (varargin)
  calc_rgb = false;
  switch (nargin)
    case 2
      c = varargin{1};
      vp = varargin{2};
      x = 1:size (c, 2);
      y = 1:size (c, 1);
      z = 1:size (c, 3);
    case 4
      calc_rgb = true;
      R = varargin{1};
      G = varargin{2};
      B = varargin{3};
      vp = varargin{4};
      x = 1:size (R, 1);
      y = 1:size (R, 2);
      z = 1:size (R, 3);
    case 5
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      c = varargin{4};
      vp = varargin{5};
    case 7
      calc_rgb = true;
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      R = varargin{4};
      G = varargin{5};
      B = varargin{6};
      vp = varargin{7};
    otherwise
      print_usage ();
  endswitch
  if (isnumeric (vp) && columns (vp) == 3)
    pa = [];
    v = vp;
  elseif ( ishandle (vp) )
    pa = vp;
    v = get (pa, "Vertices");
  else
    error ("isocolors: last argument is not a vertex list or patch handle");
  endif
  if (calc_rgb)
    new_col = zeros (rows (v), 3);
    new_col(:,1) = __interp_cube__ (x, y, z, R, v, "values" );
    new_col(:,2) = __interp_cube__ (x, y, z, G, v, "values" );
    new_col(:,3) = __interp_cube__ (x, y, z, B, v, "values" );
  else
    new_col = __interp_cube__ (x, y, z, c, v, "values" );
  endif
  switch (nargout)
    case 0
      if (! isempty (pa))
        set (pa, "FaceVertexCData", new_col);
      endif
    case 1
      varargout = {new_col};
    otherwise
      print_usage ();
  endswitch
endfunction


%!test
%! [x, y, z] = meshgrid (0:.5:2, 0:.5:2, 0:.5:2);
%! c = (x-.5).^2 + (y-.5).^2 + (z-.5).^2;
%! [f, v] = isosurface (x, y, z, c, .4);
%! cdat = isocolors (x, y, z, c, v);
%! assert (rows (cdat) == rows (v));
## Can't create a patch handle for tests without a figure

