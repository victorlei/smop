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
## @deftypefn  {Function File} {@var{map} =} rainbow ()
## @deftypefnx {Function File} {@var{map} =} rainbow (@var{n})
## Create color colormap.  This colormap ranges from red through orange,
## yellow, green, blue, to violet.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

## this colormap is not part of matlab, it is like the prism
## colormap map but with a continuous map

## PKG_ADD: colormap ("register", "rainbow");
## PKG_DEL: colormap ("unregister", "rainbow");

function map = rainbow (n = rows (colormap ()))

  if (nargin > 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("rainbow: N must be a scalar");
  endif
  n = double (n);

  if (n == 1)
    map = [1, 0, 0];
  elseif (n > 1)
    x = [0:(n-1)]' / (n - 1);

    r = (  (x < 2/5)
         + (x >= 2/5 & x < 3/5) .* (-5 * x + 3)
         + (x >= 4/5) .* (10/3 * x - 8/3));

    g = (  (x < 2/5) .* (5/2 * x)
         + (x >= 2/5 & x < 3/5)
         + (x >= 3/5 & x < 4/5) .* (-5 * x + 4));

    b = (  (x >= 3/5 & x < 4/5) .* (5 * x - 3)
         + (x >= 4/5));

    map = [r, g, b];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'rainbow' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! colormap (rainbow (64));

