## Copyright (C) 2007-2015 Kai Habel
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
## @deftypefn  {Function File} {} spinmap ()
## @deftypefnx {Function File} {} spinmap (@var{t})
## @deftypefnx {Function File} {} spinmap (@var{t}, @var{inc})
## @deftypefnx {Function File} {} spinmap ("inf")
## Cycle the colormap for @var{t} seconds with a color increment of @var{inc}.
##
## Both parameters are optional.  The default cycle time is 5 seconds and the
## default increment is 2.  If the option @qcode{"inf"} is given then cycle
## continuously until @kbd{Control-C} is pressed.
##
## When rotating, the original color 1 becomes color 2, color 2 becomes
## color 3, etc.  A positive or negative increment is allowed and a higher
## value of @var{inc} will cause faster cycling through the colormap.
## @seealso{colormap}
## @end deftypefn

## Author: Kai Habel <kai.habel at gmx.de>

function spinmap (t = 5, inc = 2)

  if (nargin > 2)
    print_usage ();
  elseif (ischar (t))
    if (strcmpi (t, "inf"))
      t = Inf;
    else
      error ('spinmap: time T must be a real scalar or "inf"');
    endif
  elseif (! isscalar (t) || ! isreal (t))
    error ("spinmap: time T must be a real scalar");
  endif

  cmap = cmap_orig = get (gcf (), "colormap");

  t0 = clock;
  while (etime (clock, t0) < t)
    cmap = shift (cmap, inc, 1);
    set (gcf (), "colormap", cmap);
    drawnow ();
  endwhile

  set (gcf (), "colormap", cmap_orig);

endfunction


%!demo
%! clf;
%! colormap (rainbow (128));
%! imagesc (1:8);
%! axis off;
%! title ("Rotate color bars to the right");
%! spinmap (3, 1);

