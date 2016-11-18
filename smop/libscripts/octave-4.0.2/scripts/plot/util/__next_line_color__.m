## Copyright (C) 2013-2015 Carl Osterwisch
## Copyright (C) 2007-2015 John W. Eaton
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
## @deftypefn {Function File} {@var{rgb} =} __next_line_color__ (@var{reset})
## Undocumented internal function.
## @end deftypefn

## Return the next line color in the rotation.

## Author: Carl Osterwisch
## Author: jwe

function rgb = __next_line_color__ (reset)

  persistent reset_colors = true;

  if (nargin == 1)
    ## Indicates whether the next call will increment or not
    reset_colors = reset;
  else
    ## Find and return the next line color
    ca = gca ();
    colororder = get (ca, "colororder");
    if (reset_colors)
      color_index = 1;
      reset_colors = false;
    else
      ## Executed when "hold all" is active
      n_kids = length (get (ca, "children"));
      n_colors = rows (colororder);
      color_index = mod (n_kids, n_colors) + 1;
    endif
    rgb = colororder(color_index,:);
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   set (hax, "colororder", [1 0 0; 0 1 0; 0 0 1]);
%!   hold on;
%!   h = plot (1:5,1:5,'o', 1:4,1:4, "x", 1:3,1:3, "d");
%!   assert (get (h, "color"), {[1 0 0]; [0 1 0]; [0 0 1]});
%!   cla (hax);
%!   hold all;
%!   h1 = plot (1:5,1:5,'o');
%!   h2 = plot (1:4,1:4, "x");
%!   h3 = plot (1:3,1:3, "d");
%!   assert (get ([h1;h2;h3], "color"), {[1 0 0]; [0 1 0]; [0 0 1]});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

