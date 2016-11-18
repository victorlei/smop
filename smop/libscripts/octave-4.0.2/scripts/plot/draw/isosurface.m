## Copyright (C) 2009-2016 Martin Helm
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
## @deftypefn  {Function File} {[@var{fv}] =} isosurface (@var{val}, @var{iso})
## @deftypefnx {Function File} {[@var{fv}] =} isosurface (@var{x}, @var{y}, @var{z}, @var{val}, @var{iso})
## @deftypefnx {Function File} {[@var{fv}] =} isosurface (@dots{}, "noshare", "verbose")
## @deftypefnx {Function File} {[@var{fvc}] =} isosurface (@dots{}, @var{col})
## @deftypefnx {Function File} {[@var{f}, @var{v}] =} isosurface (@var{x}, @var{y}, @var{z}, @var{val}, @var{iso})
## @deftypefnx {Function File} {[@var{f}, @var{v}, @var{c}] =} isosurface (@var{x}, @var{y}, @var{z}, @var{val}, @var{iso}, @var{col})
## @deftypefnx {Function File} {} isosurface (@var{x}, @var{y}, @var{z}, @var{val}, @var{iso}, @var{col}, @var{opt})
##
## Calculate isosurface of 3-D data.
##
## If called with one output argument and the first input argument
## @var{val} is a three-dimensional array that contains the data of an
## isosurface geometry and the second input argument @var{iso} keeps the
## isovalue as a scalar value then return a structure array @var{fv}
## that contains the fields @var{Faces} and @var{Vertices} at computed
## points @command{[x, y, z] = meshgrid (1:l, 1:m, 1:n)}.  The output
## argument @var{fv} can directly be taken as an input argument for the
## @command{patch} function.
##
## If called with further input arguments @var{x}, @var{y} and @var{z}
## which are three--dimensional arrays with the same size than @var{val}
## then the volume data is taken at those given points.
##
## The string input argument @qcode{"noshare"} is only for compatibility and
## has no effect.  If given the string input argument
## @qcode{"verbose"} then print messages to the command line interface about the
## current progress.
##
## If called with the input argument @var{col} which is a
## three-dimensional array of the same size than @var{val} then take
## those values for the interpolation of coloring the isosurface
## geometry.  Add the field @var{FaceVertexCData} to the structure
## array @var{fv}.
##
## If called with two or three output arguments then return the
## information about the faces @var{f}, vertices @var{v} and color data
## @var{c} as separate arrays instead of a single structure array.
##
## If called with no output argument then directly process the
## isosurface geometry with the @command{patch} command.
##
## For example,
##
## @example
## @group
## [x, y, z] = meshgrid (1:5, 1:5, 1:5);
## val = rand (5, 5, 5);
## isosurface (x, y, z, val, .5);
## @end group
## @end example
##
## @noindent
## will directly draw a random isosurface geometry in a graphics window.
## Another example for an isosurface geometry with different additional
## coloring
## @c Set example in small font to prevent overfull line
##
## @smallexample
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
## set (gca, "PlotBoxAspectRatioMode", "manual", ...
##           "PlotBoxAspectRatio", [1 1 1]);
## # set (p, "FaceColor", "green", "FaceLighting", "phong");
## # light ("Position", [1 1 5]); # Available with the JHandles package
##
## subplot (2,2,2); view (-38, 20);
## p = patch ("Faces", f, "Vertices", v, "EdgeColor", "blue");
## set (gca, "PlotBoxAspectRatioMode", "manual", ...
##           "PlotBoxAspectRatio", [1 1 1]);
## # set (p, "FaceColor", "none", "FaceLighting", "phong");
## # light ("Position", [1 1 5]);
##
## subplot (2,2,3); view (-38, 20);
## [f, v, c] = isosurface (x, y, z, c, iso, y);
## p = patch ("Faces", f, "Vertices", v, "FaceVertexCData", c, ...
##            "FaceColor", "interp", "EdgeColor", "none");
## set (gca, "PlotBoxAspectRatioMode", "manual", ...
##           "PlotBoxAspectRatio", [1 1 1]);
## # set (p, "FaceLighting", "phong");
## # light ("Position", [1 1 5]);
##
## subplot (2,2,4); view (-38, 20);
## p = patch ("Faces", f, "Vertices", v, "FaceVertexCData", c, ...
##            "FaceColor", "interp", "EdgeColor", "blue");
## set (gca, "PlotBoxAspectRatioMode", "manual", ...
##           "PlotBoxAspectRatio", [1 1 1]);
## # set (p, "FaceLighting", "phong");
## # light ("Position", [1 1 5]);
## @end smallexample
##
## @seealso{isonormals, isocolors}
## @end deftypefn

## Author: Martin Helm <martin@mhelm.de>

function varargout = isosurface (varargin)

  if (nargin < 2 || nargin > 8 || nargout > 3)
    print_usage ();
  endif

  calc_colors = false;
  f = v = c = [];
  verbose = false;
  noshare = false;
  if (nargin >= 5)
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
    val = varargin{4};
    iso = varargin{5};
    if (nargin >= 6 && isnumeric (varargin{6}))
      colors = varargin{6};
      calc_colors = true;
    endif
  else
    val = varargin{1};
    [n2, n1, n3] = size (val);
    [x, y, z] = meshgrid (1:n1, 1:n2, 1:n3);
    iso = varargin{2};
    if (nargin >= 3 && isnumeric (varargin{3}))
        colors = varargin{3};
        calc_colors = true;
    endif
  endif
  if (calc_colors)
    if (nargout == 2)
      warning ("isosurface: colors will be calculated, but no output argument to receive it.");
    endif
    [fvc.faces, fvc.vertices, fvc.facevertexcdata] = ...
      __marching_cube__ (x, y, z, val, iso, colors);
  else
    [fvc.faces, fvc.vertices] = __marching_cube__ (x, y, z, val, iso);
  endif

  if (isempty (fvc.vertices) || isempty (fvc.faces))
    warning ("isosurface: triangulation is empty");
  endif

  switch (nargout)
    case 0
      ## plot the calculated surface
      hax = newplot ();
      if (calc_colors)
        pa = patch ("Faces", fvc.faces, "Vertices", fvc.vertices,
                    "FaceVertexCData", fvc.facevertexcdata,
                    "FaceColor", "flat", "EdgeColor", "none");
      else
        pa = patch ("Faces", fvc.faces, "Vertices", fvc.vertices,
                    "FaceColor", "g", "EdgeColor", "k");
      endif
      if (! ishold ())
        set (hax, "view", [-37.5, 30], "box", "off");
      endif
    case 1
      varargout = {fvc};
    case 2
      varargout = {fvc.faces, fvc.vertices};
    case 3
      varargout = {fvc.faces, fvc.vertices, fvc.facevertexcdata};
    otherwise
      print_usage ();
  endswitch

endfunction


%!demo
%! clf;
%! [x,y,z] = meshgrid (-2:0.5:2, -2:0.5:2, -2:0.5:2);
%! v = x.^2 + y.^2 + z.^2;
%! isosurface (x, y, z, v, 1);
%! axis equal;
%! title ('isosurface of a sphere');

%!shared x, y, z, val
%! [x, y, z]  = meshgrid (0:1, 0:1, 0:1); # Points for single
%! val        = [0, 0; 0, 0];             # cube and a 3-D
%! val(:,:,2) = [0, 0; 1, 0];             # array of values

%!test
%! fv = isosurface (x, y, z, val, 0.3);
%! assert (isfield (fv, "vertices"), true);
%! assert (isfield (fv, "faces"), true);
%! assert (size (fv.vertices), [3 3]);
%! assert (size (fv.faces), [1 3]);

%!test
%! fvc = isosurface (x, y, z, val, .3, y);
%! assert (isfield (fvc, "vertices"), true);
%! assert (isfield (fvc, "faces"), true);
%! assert (isfield (fvc, "facevertexcdata"), true);
%! assert (size (fvc.vertices), [3 3]);
%! assert (size (fvc.faces), [1 3]);
%! assert (size (fvc.facevertexcdata), [3 1]);

%!test
%! [f, v] = isosurface (x, y, z, val, .3);
%! assert (size (f), [1 3]);
%! assert (size (v), [3 3]);

%!test
%! [f, v, c] = isosurface (x, y, z, val, .3, y);
%! assert (size (f), [1 3]);
%! assert (size (v), [3 3]);
%! assert (size (c), [3 1]);

%!test
%! [f, v, c] = isosurface (val, .3, y);
%! assert (size (f), [1 3]);
%! assert (size (v), [3 3]);
%! assert (size (c), [3 1]);

