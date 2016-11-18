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
## @deftypefn  {Function File} {@var{vi} =} griddata3 (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi})
## @deftypefnx {Function File} {@var{vi} =} griddata3 (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi}, @var{method})
## @deftypefnx {Function File} {@var{vi} =} griddata3 (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi}, @var{method}, @var{options})
##
## Generate a regular mesh from irregular data using interpolation.
##
## The function is defined by @code{@var{v} = f (@var{x}, @var{y}, @var{z})}.
## The interpolation points are specified by @var{xi}, @var{yi}, @var{zi}.
##
## The interpolation method can be @qcode{"nearest"} or @qcode{"linear"}.
## If method is omitted it defaults to @qcode{"linear"}.
##
## The optional argument @var{options} is passed directly to Qhull when
## computing the Delaunay triangulation used for interpolation.  See
## @code{delaunayn} for information on the defaults and how to pass different
## values.
## @seealso{griddata, griddatan, delaunayn}
## @end deftypefn

## Author: David Bateman <dbateman@free.fr>

function vi = griddata3 (x, y, z, v, xi, yi, zi, method, varargin)

  if (nargin < 7)
    print_usage ();
  endif

  if (isvector (x) && isvector (y) && isvector (z) && isvector (v))
    if (! isequal (length (x), length (y), length (z), length (v)))
      error ("griddata: X, Y, Z, and V must be vectors of the same length");
    endif
  elseif (! size_equal (x, y, z, v))
    error ("griddata: X, Y, Z, and V must have equal sizes");
  endif

  ## meshgrid xi, yi and zi if they are vectors unless
  ## they are vectors of the same length.
  if (isvector (xi) && isvector (yi) && isvector (zi))
    if (! isequal (length (xi), length (yi), length (zi)))
      [xi, yi, zi] = meshgrid (xi, yi, zi);
    else
      ## Otherwise, convert to column vectors
      xi = xi(:);
      yi = yi(:);
      zi = zi(:);
    endif
  endif

  if (! size_equal (xi, yi, zi))
    error ("griddata3: XI, YI, and ZI must be vectors or matrices of the same size");
  endif

  vi = griddatan ([x(:), y(:), z(:)], v(:), [xi(:), yi(:), zi(:)], varargin{:});
  vi = reshape (vi, size (xi));

endfunction


%!testif HAVE_QHULL
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 0);
%! x = 2 * rand (1000, 1) - 1;
%! y = 2 * rand (1000, 1) - 1;
%! z = 2 * rand (1000, 1) - 1;
%! v = x.^2 + y.^2 + z.^2;
%! [xi, yi, zi] = meshgrid (-0.8:0.2:0.8);
%! vi = griddata3 (x, y, z, v, xi, yi, zi, "linear");
%! vv = vi - xi.^2 - yi.^2 - zi.^2;
%! assert (max (abs (vv(:))), 0, 0.1);

%!testif HAVE_QHULL
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 0);
%! x = 2 * rand (1000, 1) - 1;
%! y = 2 * rand (1000, 1) - 1;
%! z = 2 * rand (1000, 1) - 1;
%! v = x.^2 + y.^2 + z.^2;
%! [xi, yi, zi] = meshgrid (-0.8:0.2:0.8);
%! vi = griddata3 (x, y, z, v, xi, yi, zi, "nearest");
%! vv = vi - xi.^2 - yi.^2 - zi.^2;
%! assert (max (abs (vv(:))), 0, 0.1)

