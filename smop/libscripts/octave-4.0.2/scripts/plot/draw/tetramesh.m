## Copyright (C) 2012-2015 Martin Helm
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
## @deftypefn  {Function File} {} tetramesh (@var{T}, @var{X})
## @deftypefnx {Function File} {} tetramesh (@var{T}, @var{X}, @var{C})
## @deftypefnx {Function File} {} tetramesh (@dots{}, @var{property}, @var{val}, @dots{})
## @deftypefnx {Function File} {@var{h} =} tetramesh (@dots{})
## Display the tetrahedrons defined in the m-by-4 matrix @var{T} as 3-D patches.
##
## @var{T} is typically the output of a Delaunay triangulation of a 3-D set
## of points.  Every row of @var{T} contains four indices into the n-by-3
## matrix @var{X} of the vertices of a tetrahedron.  Every row in @var{X}
## represents one point in 3-D space.
##
## The vector @var{C} specifies the color of each tetrahedron as an index
## into the current colormap.  The default value is 1:m where m is the number
## of tetrahedrons; the indices are scaled to map to the full range of the
## colormap.  If there are more tetrahedrons than colors in the colormap then
## the values in @var{C} are cyclically repeated.
##
## Calling @code{tetramesh (@dots{}, "property", "value", @dots{})} passes all
## property/value pairs directly to the patch function as additional arguments.
##
## The optional return value @var{h} is a vector of patch handles where each
## handle represents one tetrahedron in the order given by @var{T}.
## A typical use case for @var{h} is to turn the respective patch
## @qcode{"visible"} property @qcode{"on"} or @qcode{"off"}.
##
## Type @code{demo tetramesh} to see examples on using @code{tetramesh}.
## @seealso{trimesh, delaunay, delaunayn, patch}
## @end deftypefn

## Author: Martin Helm <martin@mhelm.de>

function h = tetramesh (varargin)

  [reg, prop] = parseparams (varargin);

  if (length (reg) < 2 || length (reg) > 3)
    print_usage ();
  endif

  T = reg{1};
  X = reg{2};

  if (! ismatrix (T) || columns (T) != 4)
    error ("tetramesh: T must be an n-by-4 matrix");
  elseif (! ismatrix (X) || columns (X) != 3)
    error ("tetramesh: X must be an n-by-3 matrix");
  endif

  size_T = rows (T);
  cmap = colormap ();

  if (length (reg) < 3)
    size_cmap = rows (cmap);
    C = mod ((1:size_T)' - 1, size_cmap) + 1;
    if (size_T < size_cmap && size_T > 1)
      ## expand to the available range of colors
      C = floor ((C - 1) * (size_cmap - 1) / (size_T - 1)) + 1;
    endif
  else
    C = reg{3};
    if (! isvector (C) || size_T != length (C))
      error ("tetramesh: C must be a vector of the same length as T");
    endif
  endif

  hax = newplot ();

  hvec = zeros (size_T, 1);
  if (strcmp (graphics_toolkit (), "gnuplot"))
    ## Tiny reduction of the tetrahedron size to help gnuplot by
    ## avoiding identical faces with different colors
    for i = 1:size_T
      [th, p] = __shrink__ ([1 2 3 4], X(T(i, :), :), 1 - 1e-7);
      hvec(i) = patch ("Faces", th, "Vertices", p,
                       "FaceColor", cmap(C(i), :), "FaceAlpha", 0.9,
                       prop{:});
    endfor
  else
    ## FLTK does not support FaceAlpha.
    for i = 1:size_T
      th = [1 2 3; 2 3 4; 3 4 1; 4 1 2];
      hvec(i) = patch ("Faces", th, "Vertices", X(T(i, :), :),
                       "FaceColor", cmap(C(i), :), "FaceAlpha", 1.0,
                       prop{:});
    endfor
  endif

  if (! ishold ())
    set (hax, "view", [-37.5, 30], "box", "off");
  endif

  if (nargout > 0)
    h = hvec;
  endif

endfunction

## shrink the tetrahedron relative to its center of gravity
function [tri, p] = __shrink__ (T, X, sf)
  midpoint = repmat (sum (X(T, :), 1) / 4, 12, 1);
  p = [X([1 2 3], :); X([2 3 4], :); X([3 4 1], :); X([4 1 2], :)];
  p = sf * (p - midpoint) + midpoint;
  tri = reshape (1:12, 3, 4)';
endfunction


%!demo
%! clf;
%! d = [-1 1];
%! [x,y,z] = meshgrid (d, d, d);
%! x = [x(:); 0];
%! y = [y(:); 0];
%! z = [z(:); 0];
%! tetra = delaunay (x, y, z);
%! X = [x(:) y(:) z(:)];
%! colormap (jet (64));
%! h = tetramesh (tetra, X);
%! set (h(1:2:end), 'Visible', 'off');
%! axis equal;
%! view (30, 20);
%! title ({'tetramesh() plot', ...
%!         'colormap = jet (64), every other tetrahedron invisible'});

%!demo
%! clf;
%! d = [-1 1];
%! [x,y,z] = meshgrid (d, d, d);
%! x = [x(:); 0];
%! y = [y(:); 0];
%! z = [z(:); 0];
%! tetra = delaunay (x, y, z);
%! X = [x(:) y(:) z(:)];
%! colormap (gray (256));
%! tetramesh (tetra, X, 21:20:241, 'EdgeColor', 'w');
%! axis equal;
%! view (30, 20);
%! title ({'tetramesh() plot', ...
%!         'colormap = gray (256) with white edges'});

