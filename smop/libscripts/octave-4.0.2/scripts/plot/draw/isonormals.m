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
## @deftypefn  {Function File} {[@var{n}] =} isonormals (@var{val}, @var{v})
## @deftypefnx {Function File} {[@var{n}] =} isonormals (@var{val}, @var{p})
## @deftypefnx {Function File} {[@var{n}] =} isonormals (@var{x}, @var{y}, @var{z}, @var{val}, @var{v})
## @deftypefnx {Function File} {[@var{n}] =} isonormals (@var{x}, @var{y}, @var{z}, @var{val}, @var{p})
## @deftypefnx {Function File} {[@var{n}] =} isonormals (@dots{}, "negate")
## @deftypefnx {Function File} {} isonormals (@dots{}, @var{p})
##
## Calculate normals to an isosurface.
##
## If called with one output argument and the first input argument
## @var{val} is a three-dimensional array that contains the data for an
## isosurface geometry and the second input argument @var{v} keeps the
## vertices of an isosurface then return the normals @var{n} in form of
## a matrix with the same size than @var{v} at computed points
## @command{[x, y, z] = meshgrid (1:l, 1:m, 1:n)}.  The output argument
## @var{n} can be taken to manually set @var{VertexNormals} of a patch.
##
## If called with further input arguments @var{x}, @var{y} and @var{z}
## which are three--dimensional arrays with the same size than @var{val}
## then the volume data is taken at those given points.  Instead of the
## vertices data @var{v} a patch handle @var{p} can be passed to this
## function.
##
## If given the string input argument @qcode{"negate"} as last input argument
## then compute the reverse vector normals of an isosurface geometry.
##
## If no output argument is given then directly redraw the patch that is
## given by the patch handle @var{p}.
##
## For example:
## @c Set example in small font to prevent overfull line
##
## @smallexample
## function [] = isofinish (p)
##   set (gca, "PlotBoxAspectRatioMode", "manual", ...
##             "PlotBoxAspectRatio", [1 1 1]);
##   set (p, "VertexNormals", -get (p,"VertexNormals")); # Revert normals
##   set (p, "FaceColor", "interp");
##   ## set (p, "FaceLighting", "phong");
##   ## light ("Position", [1 1 5]); # Available with JHandles
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
## [f, v, cdat] = isosurface (x, y, z, c, iso, y);
## p = patch ("Faces", f, "Vertices", v, "FaceVertexCData", cdat, ...
##            "FaceColor", "interp", "EdgeColor", "none");
## isofinish (p);  # Call user function isofinish
##
## subplot (2,2,2); view (-38, 20);
## p = patch ("Faces", f, "Vertices", v, "FaceVertexCData", cdat, ...
##            "FaceColor", "interp", "EdgeColor", "none");
## isonormals (x, y, z, c, p); # Directly modify patch
## isofinish (p);
##
## subplot (2,2,3); view (-38, 20);
## p = patch ("Faces", f, "Vertices", v, "FaceVertexCData", cdat, ...
##            "FaceColor", "interp", "EdgeColor", "none");
## n = isonormals (x, y, z, c, v); # Compute normals of isosurface
## set (p, "VertexNormals", n);    # Manually set vertex normals
## isofinish (p);
##
## subplot (2,2,4); view (-38, 20);
## p = patch ("Faces", f, "Vertices", v, "FaceVertexCData", cdat, ...
##            "FaceColor", "interp", "EdgeColor", "none");
## isonormals (x, y, z, c, v, "negate"); # Use reverse directly
## isofinish (p);
## @end smallexample
##
## @seealso{isosurface, isocolors}
## @end deftypefn

## Author: Martin Helm <martin@mhelm.de>

function varargout = isonormals (varargin)
  na = nargin;
  negate = false;
  if (ischar (varargin{nargin}))
    na = nargin-1;
    if (strcmp (lower (varargin{nargin}), "negate"))
      negate = true;
    else
      error ("isonormals: Unknown option '%s'", varargin{nargin});
    endif
  endif
  switch (na)
    case 2
      c = varargin{1};
      vp = varargin{2};
      x = 1:size (c, 2);
      y = 1:size (c, 1);
      z = 1:size (c, 3);
    case 5
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      c = varargin{4};
      vp = varargin{5};
    otherwise
      print_usage ();
  endswitch
  if (isnumeric (vp) && columns (vp) == 3)
    pa = [];
    v = vp;
  elseif (ishandle (vp))
    pa = vp;
    v = get (pa, "Vertices");
  else
    error ("isonormals: Last argument is not a vertex list or a patch handle");
  endif
  if (negate)
    normals = -__interp_cube__ (x, y, z, c, v, "normals");
  else
    normals = __interp_cube__ (x, y, z, c, v, "normals");
  endif
  switch (nargout)
    case 0
      if (! isempty (pa))
        set (pa, "VertexNormals", normals);
      endif
    case 1
      varargout = {normals};
    otherwise
      print_usage ();
  endswitch
endfunction


%!test
%! [x, y, z] = meshgrid (0:.5:2, 0:.5:2, 0:.5:2);
%! c = abs ((x-.5).^2 + (y-.5).^2 + (z-.5).^2);
%! [f, v, cdat] = isosurface (x, y, z, c, .4, y);
%! n = isonormals (x, y, z, c, v);
%! assert (size (v), size (n));
%!test
%! [x, y, z] = meshgrid (0:.5:2, 0:.5:2, 0:.5:2);
%! c = abs ((x-.5).^2 + (y-.5).^2 + (z-.5).^2);
%! [f, v, cdat] = isosurface (x, y, z, c, .4, y);
%! np = isonormals (x, y, z, c, v);
%! nn = isonormals (x, y, z, c, v, "negate");
%! assert (np, -nn);

