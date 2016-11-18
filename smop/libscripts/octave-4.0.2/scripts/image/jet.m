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
## @deftypefn  {Function File} {@var{map} =} jet ()
## @deftypefnx {Function File} {@var{map} =} jet (@var{n})
## Create color colormap.  This colormap ranges from dark blue through blue,
## cyan, green, yellow, red, to dark red.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

## PKG_ADD: colormap ("register", "jet");
## PKG_DEL: colormap ("unregister", "jet");

function map = jet (n = rows (colormap ()))

  if (nargin > 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("jet: N must be a scalar");
  endif

  if (n == 1)
    map = [0, 1, 1];
  elseif (n == 2)
    map = [0, 0, 1
           0, 1, 1];
  elseif (n > 2)
    nel = ceil (n/4);           # number of elements
    idx1 = ceil (3/8 * n) + 1;  # ~3/8*n for large n
    if (mod (n, 8) == 2)
      idx1++;
    endif
    idx2 = idx1 + nel - 1;      # ~5/8*n for large n
    idx3 = min (idx2 + nel, n); # ~7/8*n for large n

    r = zeros (n, 1);
    r(idx1:idx2, 1) = [1:nel] / nel;
    r(idx2:idx3, 1) = 1;
    nel2 = n - idx3;
    r(idx3:(idx3+nel2), 1) = [nel:-1:(nel - nel2)] / nel;

    idx1 = idx1 - nel;          # ~1/8*n for large n
    idx2 = idx1 + nel - 1;      # ~3/8*n for large n
    idx3 = min (idx2 + nel, n); # ~5/8*n for large n

    g = zeros (n, 1);
    g(idx1:idx2, 1) = [1:nel] / nel;
    g(idx2:idx3, 1) = 1;
    nel2 = min (nel, n - idx3);
    g(idx3:(idx3+nel2), 1) = [nel:-1:(nel - nel2)] / nel;

    idx1 = max (idx2 - nel, 1); # ~1/8*n for large n
    idx2 = idx2;                # ~3/8*n for large n
    idx3 = idx3;                # ~5/8*n for large n

    b = zeros (n, 1);
    nel2 = min (nel, idx1-1);
    b(1:idx1, 1) = [(nel - nel2):nel] / nel;
    b(idx1:idx2, 1) = 1;
    nel2 = min (nel, n - idx3);
    b(idx2:(idx2+nel2), 1) = [nel:-1:(nel - nel2)] / nel;

    map = [r, g, b];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'jet' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! colormap (jet (64));

