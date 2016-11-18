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
## @deftypefn  {Function File} {@var{map} =} copper ()
## @deftypefnx {Function File} {@var{map} =} copper (@var{n})
## Create color colormap.  This colormap varies from black to a light copper
## tone.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

## PKG_ADD: colormap ("register", "copper");
## PKG_DEL: colormap ("unregister", "copper");

function map = copper (n = rows (colormap ()))

  if (nargin > 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("copper: N must be a scalar");
  endif
  n = double (n);

  if (n == 1)
    map = [0, 0, 0];
  elseif (n > 1)
    x = [0:(n-1)]' / (n - 1);
    r = (x < 4/5) .* (5/4 * x) ...
      + (x >= 4/5);
    g = 0.7812 * x;
    b = 0.4975 * x;
    map = [r, g, b];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'copper' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! colormap (copper (64));

