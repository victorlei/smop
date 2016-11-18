## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Function File} {} triplot (@var{tri}, @var{x}, @var{y})
## @deftypefnx {Function File} {} triplot (@var{tri}, @var{x}, @var{y}, @var{linespec})
## @deftypefnx {Function File} {@var{h} =} triplot (@dots{})
## Plot a 2-D triangular mesh.
##
## @var{tri} is typically the output of a Delaunay triangulation over the
## grid of @var{x}, @var{y}.  Every row of @var{tri} represents one triangle
## and contains three indices into [@var{x}, @var{y}] which are the
## vertices of the triangles in the x-y plane.
##
## The linestyle to use for the plot can be defined with the argument
## @var{linespec} of the same format as the @code{plot} command.
##
## The optional return value @var{h} is a graphics handle to the created
## patch object.
## @seealso{plot, trimesh, trisurf, delaunay}
## @end deftypefn

function h = triplot (tri, x, y, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  idx = tri(:, [1, 2, 3, 1]).';
  nt = rows (tri);
  handle = plot ([x(idx); NaN(1, nt)](:),
                 [y(idx); NaN(1, nt)](:), varargin{:});

  if (nargout > 0)
    h = handle;
  endif

endfunction


%!demo
%! clf;
%! old_state = rand ('state');
%! restore_state = onCleanup (@() rand ('state', old_state));
%! rand ('state', 2);
%! N = 20;
%! x = rand (N, 1);
%! y = rand (N, 1);
%! tri = delaunay (x, y);
%! triplot (tri, x, y);

