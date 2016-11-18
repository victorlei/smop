## Copyright (C) 1999-2015 Kai Habel
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
## @deftypefn  {Function File} {@var{tri} =} delaunay (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{tetr} =} delaunay (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {@var{tri} =} delaunay (@var{x})
## @deftypefnx {Function File} {@var{tri} =} delaunay (@dots{}, @var{options})
## Compute the Delaunay triangulation for a 2-D or 3-D set of points.
##
## For 2-D sets, the return value @var{tri} is a set of triangles which
## satisfies the Delaunay circum-circle criterion, i.e., only a single data
## point from [@var{x}, @var{y}] is within the circum-circle of the defining
## triangle.  The set of triangles @var{tri} is a matrix of size [n, 3].  Each
## row defines a triangle and the three columns are the three vertices of the
## triangle.  The value of @code{@var{tri}(i,j)} is an index into @var{x} and
## @var{y} for the location of the j-th vertex of the i-th triangle.
##
## For 3-D sets, the return value @var{tetr} is a set of tetrahedrons which
## satisfies the Delaunay circum-circle criterion, i.e., only a single data
## point from [@var{x}, @var{y}, @var{z}] is within the circum-circle of the
## defining tetrahedron.  The set of tetrahedrons is a matrix of size [n, 4].
## Each row defines a tetrahedron and the four columns are the four vertices of
## the tetrahedron.  The value of @code{@var{tetr}(i,j)} is an index into
## @var{x}, @var{y}, @var{z} for the location of the j-th vertex of the i-th
## tetrahedron.
##
## The input @var{x} may also be a matrix with two or three columns where the
## first column contains x-data, the second y-data, and the optional third
## column contains z-data.
##
## The optional last argument, which must be a string or cell array of strings,
## contains options passed to the underlying qhull command.
## See the documentation for the Qhull library for details
## @url{http://www.qhull.org/html/qh-quick.htm#options}.
## The default options are @code{@{"Qt", "Qbb", "Qc", "Qz"@}}.
##
## If @var{options} is not present or @code{[]} then the default arguments are
## used.  Otherwise, @var{options} replaces the default argument list.
## To append user options to the defaults it is necessary to repeat the
## default arguments in @var{options}.  Use a null string to pass no arguments.
##
## @example
## @group
## x = rand (1, 10);
## y = rand (1, 10);
## tri = delaunay (x, y);
## triplot (tri, x, y);
## hold on;
## plot (x, y, "r*");
## axis ([0,1,0,1]);
## @end group
## @end example
## @seealso{delaunayn, convhull, voronoi, triplot, trimesh, tetramesh, trisurf}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function tri = delaunay (varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  z = [];
  options = [];

  switch (nargin)

    case 1
      if (! ismatrix (varargin{1})
          || (columns (varargin{1}) != 2 && columns (varargin{1}) != 3))
          error ("delaunay: X must be a matrix with 2 or 3 columns");
      else
        x = varargin{1}(:,1);
        y = varargin{1}(:,2);
        if (columns (varargin{1}) == 3)
          z = varargin{1}(:,3);
        endif
      endif

    case 2
      if (isnumeric (varargin{2}))
        x = varargin{1};
        y = varargin{2};
      elseif (! (ischar (varargin{2}) || iscellstr (varargin{2})))
        error ("delaunay: OPTIONS must be a string or cell array of strings");
      else
        options = varargin{2};
        ncols = columns (varargin{1});

        if (! ismatrix (varargin{1}) || (ncols != 2 && ncols != 3))
          error ("delaunay: X must be a matrix with 2 or 3 columns");
        else
          x = varargin{1}(:,1);
          y = varargin{1}(:,2);
          if (ncols == 3)
            z = varargin{1}(:,3);
          endif
        endif
      endif

    case 3
      if (isnumeric (varargin{3}))
        x = varargin{1};
        y = varargin{2};
        z = varargin{3};
      elseif (! (ischar (varargin{3}) || iscellstr (varargin{3})))
        error ("delaunay: OPTIONS must be a string or cell array of strings");
      else
        x = varargin{1};
        y = varargin{2};
        options = varargin{3};
      endif

    case 4
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      options = varargin{4};

      if (! (ischar (options) || iscellstr (options)))
        error ("delaunay: OPTIONS must be a string or cell array of strings");
      endif

  endswitch

  if (isempty (z))
    if (! size_equal (x, y))
      error ("delaunay: X and Y must be the same size");
    endif
    tri = delaunayn ([x(:), y(:)], options);
  else
    if (! size_equal (x, y, z))
      error ("delaunay: X, Y, and Z must be the same size");
    endif
    tri = delaunayn ([x(:), y(:), z(:)], options);
  endif

endfunction


%!demo
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 1);
%! x = rand (1,10);
%! y = rand (1,10);
%! tri = delaunay (x,y);
%! clf;
%! triplot (tri, x, y);
%! hold on;
%! plot (x, y, "r*");
%! axis ([0,1,0,1]);

%!testif HAVE_QHULL
%! x = [-1, 0, 1, 0];
%! y = [0, 1, 0, -1];
%! assert (sortrows (sort (delaunay (x, y), 2)), [1,2,4;2,3,4]);

%!testif HAVE_QHULL
%! x = [-1, 0, 1, 0];
%! y = [0, 1, 0, -1];
%! assert (sortrows (sort (delaunay ([x(:) y(:)]), 2)), [1,2,4;2,3,4]);

%!testif HAVE_QHULL
%! x = [-1, 0, 1, 0, 0];
%! y = [0, 1, 0, -1, 0];
%! assert (sortrows (sort (delaunay (x, y), 2)), [1,2,5;1,4,5;2,3,5;3,4,5]);

%!testif HAVE_QHULL
%! x = [-1, 0; 0, 1; 1, 0; 0, -1; 0, 0];
%! assert (sortrows (sort (delaunay (x), 2)), [1,2,5;1,4,5;2,3,5;3,4,5]);

%!testif HAVE_QHULL
%! x = [1 5 2; 5 6 7];
%! y = [5 7 8; 1 2 3];
%! assert (sortrows (sort (delaunay (x, y), 2)), [1,2,4;1,3,4;1,3,5;3,4,6]);

## Test 3-D input
%!testif HAVE_QHULL
%! x = [-1, -1, 1, 0, -1]; y = [-1, 1, 1, 0, -1]; z = [0, 0, 0, 1, 1];
%! assert (sortrows (sort (delaunay (x, y, z), 2)), [1,2,3,4;1,2,4,5])

## Input validation tests
%!error delaunay ()
%!error delaunay (1,2,3,4,5)
%!error <X must be a matrix with 2 or 3 columns> delaunay (ones (2,4))
%!error <OPTIONS must be a string or cell array> delaunay (ones (2,2), struct())
%!error <X must be a matrix with 2 or 3 columns> delaunay (ones (2,4), "")
%!error <OPTIONS must be a string or cell array> delaunay (ones (2,2), ones (2,2), struct())
%!error <OPTIONS must be a string or cell array> delaunay (ones (2,2), ones (2,2), ones (2,2), struct())
%!error <X and Y must be the same size> delaunay (1, [1 2])
%!error <X, Y, and Z must be the same size> delaunay (1, [1 2], [1 2])

