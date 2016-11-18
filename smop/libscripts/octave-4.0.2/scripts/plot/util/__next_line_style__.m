## Copyright (C) 2010-2015 David Bateman
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
## @deftypefn {Function File} {@var{style} =} __next_line_style__ (@var{reset})
## Undocumented internal function.
## @end deftypefn

## Return the next line style in the rotation.


function [linestyle, marker] = __next_line_style__ (reset)

  persistent reset_style = true;

  if (nargin == 1)
    ## Indicates whether the next call will increment or not
    reset_style = reset;
  else
    ## Find and return the next line style
    ca = gca ();
    style_rotation = get (ca, "linestyleorder");
    if (ischar (style_rotation))
      style_rotation = strsplit (style_rotation, "|");
    endif
    nStyles = length (style_rotation);
    if (reset_style || (nStyles < 2))
      style_index = 1;
      reset_style = false;
    else
      ## Executed when "hold all" is active
      nChildren = length (get (ca, "Children"));
      nColors = rows (get (ca, "ColorOrder"));
      style_index = mod (floor (nChildren/nColors), nStyles) + 1;
    endif
    options = __pltopt__ ("__next_line_style__",
                          style_rotation(style_index));
    linestyle = options.linestyle;
    marker = options.marker;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   set (hax, "colororder", [0 0 1]);
%!   set (hax, "linestyleorder", {"-", ":", "--"});
%!   hold on;
%!   h = plot (1:5,1:5, 1:4,1:4, 1:3,1:3);
%!   assert (get (h, "linestyle"), {"-"; ":"; "--"});
%!   cla (hax);
%!   hold all;
%!   h1 = plot (1:5,1:5);
%!   h2 = plot (1:4,1:4);
%!   h3 = plot (1:3,1:3);
%!   assert (get ([h1;h2;h3], "linestyle"), {"-"; ":"; "--"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

