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
## @deftypefn  {Function File} {@var{tetr} =} delaunay3 (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {@var{tetr} =} delaunay3 (@var{x}, @var{y}, @var{z}, @var{options})
##
## @code{delaunay3} is deprecated and will be removed in Octave version 4.4.
## Please use @code{delaunay} in all new code.
##
## Compute the Delaunay triangulation for a 3-D set of points.
## The return value @var{tetr} is a set of tetrahedrons which satisfies the
## Delaunay circum-circle criterion, i.e., only a single data point from
## [@var{x}, @var{y}, @var{z}] is within the circum-circle of the defining
## tetrahedron.
##
## The set of tetrahedrons @var{tetr} is a matrix of size [n, 4].  Each
## row defines a tetrahedron and the four columns are the four vertices
## of the tetrahedron.  The value of @code{@var{tetr}(i,j)} is an index into
## @var{x}, @var{y}, @var{z} for the location of the j-th vertex of the i-th
## tetrahedron.
##
## An optional fourth argument, which must be a string or cell array of strings,
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
## @seealso{delaunay, delaunayn, convhull, voronoi, tetramesh}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

## Deprecated in 4.0

function tetr = delaunay3 (x, y, z, options)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "delaunay3 is obsolete and will be removed from a future version of Octave, please use delaunay instead");
  endif

  if (nargin < 3 || nargin > 4)
    print_usage ();
  endif

  if (! (isvector (x) && isvector (y) && isvector (z)
         && length (x) == length (y) && length (x) == length (z)))
    error ("delaunay: X, Y, and Z must be the same size");
  elseif (nargin == 4 && ! (ischar (options) || iscellstr (options)))
    error ("delaunay3: OPTIONS must be a string or cell array of strings");
  endif

  if (nargin == 3)
    tetr = delaunayn ([x(:), y(:), z(:)]);
  else
    tetr = delaunayn ([x(:), y(:), z(:)], options);
  endif

endfunction


%!testif HAVE_QHULL
%! x = [-1, -1, 1, 0, -1]; y = [-1, 1, 1, 0, -1]; z = [0, 0, 0, 1, 1];
%! assert (sortrows (sort (delaunay3 (x, y, z), 2)), [1,2,3,4;1,2,4,5])

## FIXME: Need input validation tests

