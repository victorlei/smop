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
## @deftypefn  {Function File} {@var{map} =} hot ()
## @deftypefnx {Function File} {@var{map} =} hot (@var{n})
## Create color colormap.  This colormap ranges from black through dark red,
## red, orange, yellow, to white.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

## PKG_ADD: colormap ("register", "hot");
## PKG_DEL: colormap ("unregister", "hot");

function map = hot (n = rows (colormap ()))

  if (nargin > 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("hot: N must be a scalar");
  endif

  if (n == 1)
    map = [1, 1, 1];
  elseif (n == 2)
    map = [1, 1, 1/2
           1, 1,  1 ];
  elseif (n > 2)
    idx = floor (3/8 * n);
    nel = idx;

    r = ones (n, 1);
    r(1:idx, 1) = [1:nel]' / nel;

    g = zeros (n, 1);
    g(idx+1:2*idx, 1) = r(1:idx);
    g(2*idx+1:end, 1) = 1;

    idx = 2*idx + 1;   # approximately 3/4 *n
    nel = n - idx + 1;

    b = zeros (n, 1);
    b(idx:end, 1) = [1:nel]' / nel;

    map = [r, g, b];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'hot' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! colormap (hot (64));

