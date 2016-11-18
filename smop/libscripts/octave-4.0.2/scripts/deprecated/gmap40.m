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
## @deftypefn  {Function File} {@var{map} =} gmap40 ()
## @deftypefnx {Function File} {@var{map} =} gmap40 (@var{n})
##
## @code{gmap40} is deprecated and will be removed in Octave version 4.4.
##
## Create color colormap.  The colormap consists of red, green, blue, yellow,
## magenta and cyan.
##
## This colormap is specifically designed for users of gnuplot 4.0 where these
## 6 colors are the allowable ones for patch objects.
##
## The argument @var{n} must be a scalar.
## If unspecified, a length of 6 is assumed.  Larger values of @var{n} result
## in a repetition of the above colors.
## @seealso{colormap}
## @end deftypefn

## PKG_ADD: colormap ("register", "gmap40");
## PKG_DEL: colormap ("unregister", "gmap40");

## Deprecated in 4.0

function map = gmap40 (n = rows (colormap ()))

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "gmap40 is obsolete and will be removed from a future version of Octave");
  endif

  if (nargin > 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("gmap40: N must be a scalar");
  endif

  if (n > 0)
    C = [1, 0, 0; 0, 1, 0; 0, 0, 1; 1, 1, 0; 1, 0, 1; 0, 1, 1];
    map = C(rem (0:(n-1), 6) + 1, :);
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'gmap40' colormap as an image
%! image (1:6, linspace (0, 1, 6), repmat ((1:6)', 1, 6));
%! axis ([1, 6, 0, 1], "ticy", "xy");
%! colormap (gmap40 (6));

