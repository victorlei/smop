## Copyright (C) 2007-2013 Shai Ayal
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
## @deftypefn {Loadable Function} {@var{old_val} =} mouse_wheel_zoom (@var{new_val})
## Query or set the mouse wheel zoom factor.
##
## The zoom factor is a number in the range (0,1) which is the
## percentage of the current axis limits that will be used when zooming.
## For example, if the current x-axis limits are [0, 50] and
## @code{mouse_wheel_zoom} is 0.4 (40%), then a zoom operation will
## change the limits by 20.
#### @end deftypefn

## Deprecated in 4.0

function retval = mouse_wheel_zoom (val)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "mouse_wheel_zoom is obsolete and will be removed from a future version of Octave, please use the mousehweelzoom axes property instead");
  endif

  if (nargin != 1)
    print_usage ();
  endif

  fig = get (0, "currentfigure");
  if (isempty (fig))
    retval = get (0, "defaultaxesmousewheelzoom");
    set (0, "defaultaxesmousewheelzoom", val);
  else
    retval = get (gca, "mousewheelzoom");
    set (gca, "mousewheelzoom", val);
  endif

endfunction

