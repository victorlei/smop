## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn  {Function File} {@var{map} =} gray ()
## @deftypefnx {Function File} {@var{map} =} gray (@var{n})
## Create gray colormap.  This colormap varies from black to white with shades
## of gray.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

## PKG_ADD: colormap ("register", "gray");
## PKG_DEL: colormap ("unregister", "gray");

function map = gray (n = rows (colormap ()))

  if (nargin > 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("gray: N must be a scalar");
  endif
  n = double (n);

  if (n == 1)
    map = [0, 0, 0];
  elseif (n > 1)
    gr = [0:(n-1)]' / (n - 1);
    map = [gr, gr, gr];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'gray' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! colormap (gray (64));

