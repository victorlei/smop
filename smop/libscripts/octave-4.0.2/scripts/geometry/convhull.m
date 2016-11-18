## Copyright (C) 2000-2015 Kai Habel
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
## @deftypefn  {Function File} {@var{H} =} convhull (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{H} =} convhull (@var{x}, @var{y}, @var{options})
## Compute the convex hull of the set of points defined by the
## arrays @var{x} and @var{y}.  The hull @var{H} is an index vector into
## the set of points and specifies which points form the enclosing hull.
##
## An optional third argument, which must be a string or cell array of strings,
## contains options passed to the underlying qhull command.
## See the documentation for the Qhull library for details
## @url{http://www.qhull.org/html/qh-quick.htm#options}.
## The default option is @code{@{"Qt"@}}.
##
## If @var{options} is not present or @code{[]} then the default arguments are
## used.  Otherwise, @var{options} replaces the default argument list.
## To append user options to the defaults it is necessary to repeat the
## default arguments in @var{options}.  Use a null string to pass no arguments.
##
## @seealso{convhulln, delaunay, voronoi}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function H = convhull (x, y, options)

  if (nargin != 2 && nargin != 3)
    print_usage ();
  endif

  ## convhulln expects column vectors
  x = x(:);
  y = y(:);

  if (length (x) != length (y))
    error ("convhull: X and Y must have the same size");
  elseif (nargin == 3 && ! (ischar (options) || iscellstr (options)))
    error ("convhull: OPTIONS must be a string or cell array of strings");
  endif

  if (nargin == 2)
    i = convhulln ([x, y]);
  else
    i = convhulln ([x, y], options);
  endif

  n = rows (i);
  i = i'(:);
  H = zeros (n + 1, 1);

  H(1) = i(1);
  next_i = i(2);
  i(2) = 0;
  for k = 2:n
    next_idx = find (i == next_i);

    if (rem (next_idx, 2) == 0)
      H(k) = i(next_idx);
      next_i = i(next_idx - 1);
      i(next_idx - 1) = 0;
    else
      H(k) = i(next_idx);
      next_i = i(next_idx + 1);
      i(next_idx + 1) = 0;
    endif
  endfor

  H(n + 1) = H(1);

endfunction


%!demo
%! clf;
%! x = -3:0.05:3;
%! y = abs (sin (x));
%! k = convhull (x, y);
%! plot (x(k),y(k),"r-;convex hull;", x,y,"b+;points;");
%! axis ([-3.05, 3.05, -0.05, 1.05]);

%!testif HAVE_QHULL
%! x = -3:0.5:3;
%! y = abs (sin (x));
%! assert (convhull (x, y), [1;7;13;12;11;10;4;3;2;1]);

## FIXME: Need input validation tests

