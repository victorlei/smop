## Copyright (C) 2005-2015 John W. Eaton
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
## @deftypefn  {Function File} {} patch ()
## @deftypefnx {Function File} {} patch (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} patch (@var{x}, @var{y}, @var{z}, @var{c})
## @deftypefnx {Function File} {} patch (@var{fv})
## @deftypefnx {Function File} {} patch ("Faces", @var{faces}, "Vertices", @var{verts}, @dots{})
## @deftypefnx {Function File} {} patch (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} patch (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} patch (@dots{})
## Create patch object in the current axes with vertices at locations
## (@var{x}, @var{y}) and of color @var{c}.
##
## If the vertices are matrices of size @nospell{MxN} then each polygon patch
## has M vertices and a total of N polygons will be created.  If some polygons
## do not have M vertices use NaN to represent "no vertex".  If the @var{z}
## input is present then 3-D patches will be created.
##
## The color argument @var{c} can take many forms.  To create polygons
## which all share a single color use a string value (e.g., @qcode{"r"} for
## red), a scalar value which is scaled by @code{caxis} and indexed into the
## current colormap, or a 3-element RGB vector with the precise TrueColor.
##
## If @var{c} is a vector of length N then the ith polygon will have a color
## determined by scaling entry @var{c}(i) according to @code{caxis} and then
## indexing into the current colormap.  More complicated coloring situations
## require directly manipulating patch property/value pairs.
##
## Instead of specifying polygons by matrices @var{x} and @var{y}, it is
## possible to present a unique list of vertices and then a list of polygon
## faces created from those vertices.  In this case the
## @qcode{"Vertices"} matrix will be an @nospell{Nx2} (2-D patch) or
## @nospell{Nx3} (3-D patch).  The @nospell{MxN} @qcode{"Faces"} matrix
## describes M polygons having N vertices---each row describes a
## single polygon and each column entry is an index into the
## @qcode{"Vertices"} matrix to identify a vertex.  The patch object
## can be created by directly passing the property/value pairs
## @qcode{"Vertices"}/@var{verts}, @qcode{"Faces"}/@var{faces} as
## inputs.
##
## A third input form is to create a structure @var{fv} with the fields
## @qcode{"vertices"}, @qcode{"faces"}, and optionally
## @qcode{"facevertexcdata"}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created patch
## object.
##
## Implementation Note: Patches are highly configurable objects.  To truly
## customize them requires setting patch properties directly.  Useful patch
## properties are: @qcode{"cdata"}, @qcode{"edgecolor"},
## @qcode{"facecolor"}, @qcode{"faces"}, @qcode{"facevertexcdata"}.
## @seealso{fill, get, set}
## @end deftypefn

## Author: jwe

function h = patch (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("patch", varargin{:});

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  [htmp, failed] = __patch__ (hax, varargin{:});

  if (failed)
    print_usage ();
  endif

  ## FIXME: This is a hack to get 'layer' command to work for 2D patches
  ##        Alternative is much more complicated surgery in graphics.cc.
  ##        of get_children_limits() for 'z' axis and 'patch' object type.
  if (! ishold ())
    if (isempty (get (htmp, "zdata")))
      set (hax, "zlim", [-1 1]);
    endif
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! %% Patches with same number of vertices
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/8:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! patch ([x1,x2], [y1,y2], 'r');

%!demo
%! %% Unclosed patch
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! patch ([[x1;NaN(8,1)],x2], [[y1;NaN(8,1)],y2], 'r');

%!demo
%! %% Specify vertices and faces separately
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! vert = [x1, y1; x2, y2];
%! fac = [1:8,NaN(1,8);9:24];
%! patch ('Faces',fac, 'Vertices',vert, 'FaceColor','r');

%!demo
%! %% Specify vertices and faces separately
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! vert = [x1, y1; x2, y2];
%! fac = [1:8,NaN(1,8);9:24];
%! patch ('Faces',fac, 'Vertices',vert, ...
%!        'FaceVertexCData',[0, 1, 0; 0, 0, 1], 'FaceColor', 'flat');

%!demo
%! %% Property change on multiple patches
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/8:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! h = patch ([x1,x2], [y1,y2], cat (3, [0,0],[1,0],[0,1]));
%! pause (1);
%! set (h, 'FaceColor', 'r');

%!demo
%! clf;
%! vertices = [0, 0, 0;
%!             1, 0, 0;
%!             1, 1, 0;
%!             0, 1, 0;
%!             0.5, 0.5, 1];
%! faces = [1, 2, 5;
%!          2, 3, 5;
%!          3, 4, 5;
%!          4, 1, 5];
%! patch ('Vertices', vertices, 'Faces', faces, ...
%!        'FaceVertexCData', jet (4), 'FaceColor', 'flat');
%! view (-37.5, 30);

%!demo
%! clf;
%! vertices = [0, 0, 0;
%!             1, 0, 0;
%!             1, 1, 0;
%!             0, 1, 0;
%!             0.5, 0.5, 1];
%! faces = [1, 2, 5;
%!          2, 3, 5;
%!          3, 4, 5;
%!          4, 1, 5];
%! patch  ('Vertices', vertices, 'Faces', faces, ...
%!        'FaceVertexCData', jet (5), 'FaceColor', 'interp');
%! view (-37.5, 30);

%!demo
%! clf;
%! colormap (jet (64));
%! x = [0 1 1 0];
%! y = [0 0 1 1];
%! subplot (2,1,1);
%!  title ('Blue, Light-Green, and Red Horizontal Bars');
%!  patch (x, y + 0, 1);
%!  patch (x, y + 1, 2);
%!  patch (x, y + 2, 3);
%! subplot (2,1,2);
%!  title ('Blue, Light-Green, and Red Vertical Bars');
%!  patch (x + 0, y, 1 * ones (size (x)));
%!  patch (x + 1, y, 2 * ones (size (x)));
%!  patch (x + 2, y, 3 * ones (size (x)));

%!demo
%! clf;
%! colormap (jet (64));
%! x = [0 1 1 0];
%! y = [0 0 1 1];
%! subplot (2,1,1);
%!  title ('Blue horizontal bars: Dark to Light');
%!  patch (x, y + 0, 1, 'cdatamapping', 'direct');
%!  patch (x, y + 1, 9, 'cdatamapping', 'direct');
%!  patch (x, y + 2, 17, 'cdatamapping', 'direct');
%! subplot (2,1,2);
%!  title ('Blue vertical bars: Dark to Light');
%!  patch (x + 0, y, 1 * ones (size (x)), 'cdatamapping', 'direct');
%!  patch (x + 1, y, 9 * ones (size (x)), 'cdatamapping', 'direct');
%!  patch (x + 2, y, 17 * ones (size (x)), 'cdatamapping', 'direct');

%!demo
%! clf;
%! colormap (jet (64));
%! x = [ 0 0; 1 1; 1 0 ];
%! y = [ 0 0; 0 1; 1 1 ];
%! p = patch (x, y, 'b');
%! set (p, 'cdatamapping', 'direct', 'facecolor', 'flat', 'cdata', [1 32]);
%! title ('Direct mapping of colors: Light-Green UL and Blue LR triangles');

%!demo
%! clf;
%! colormap (jet (64));
%! x = [ 0 0; 1 1; 1 0 ];
%! y = [ 0 0; 0 1; 1 1 ];
%! p = patch (x, y, [1 32]);
%! title ('Autoscaling of colors: Red UL and Blue LR triangles');

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = patch ();
%!   assert (findobj (hf, "type", "patch"), h);
%!   assert (get (h, "xdata"), [0; 1; 0], eps);
%!   assert (get (h, "ydata"), [1; 1; 0], eps);
%!   assert (isempty (get (h, "zdata")));
%!   assert (isempty (get (h, "cdata")));
%!   assert (get (h, "faces"), [1, 2, 3], eps);
%!   assert (get (h, "vertices"), [0 1; 1 1; 0 0], eps);
%!   assert (get (h, "type"), "patch");
%!   assert (get (h, "facecolor"), [0 0 0]);
%!   assert (get (h, "linestyle"), get (0, "defaultpatchlinestyle"));
%!   assert (get (h, "linewidth"), get (0, "defaultpatchlinewidth"), eps);
%!   assert (get (h, "marker"), get (0, "defaultpatchmarker"));
%!   assert (get (h, "markersize"), get (0, "defaultpatchmarkersize"));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! c = 0.9;
%! unwind_protect
%!   h = patch ([0 1 0], [0 1 1], c);
%!   assert (get (gca, "clim"), [c - 1, c + 1]);
%!   h = patch ([0 1 0], [0 1 1], 2 * c);
%!   assert (get (gca, "clim"), [c, 2 * c]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

