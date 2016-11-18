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
## @deftypefn {Function File} {} hsv (@var{n})
## Create color colormap.  This colormap begins with red, changes through
## yellow, green, cyan, blue, and magenta, before returning to red.
##
## It is useful for displaying periodic functions.  The map is obtained by
## linearly varying the hue through all possible values while keeping constant
## maximum saturation and value.  The equivalent code is
## @code{hsv2rgb ([(0:N-1)'/N, ones(N,2)])}.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

## PKG_ADD: colormap ("register", "hsv");
## PKG_DEL: colormap ("unregister", "hsv");

function map = hsv (n = rows (colormap ()))

  if (nargin > 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("hsv: N must be a scalar");
  endif
  n = double (n);

  if (n == 1)
    map = [1, 0, 0];
  elseif (n > 1)
    hue = [0:n-1]' / n;
    map = hsv2rgb ([hue, ones(n,1), ones(n,1)]);
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'hsv' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! colormap (hsv (64));

