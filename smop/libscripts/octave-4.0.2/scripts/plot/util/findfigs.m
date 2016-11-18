## Copyright (C) 2008 Bill Denney
## Copyright (C) 2012-2015 Carnë Draug
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
## @deftypefn {Function File} {} findfigs ()
## Find all visible figures that are currently off the screen and move them
## onto the screen.
## @seealso{allchild, figure, get, set}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>
## Modified by: Carnë Draug <carandraug+dev@gmail.com>

function findfigs ()

  hfigs = allchild (0);
  units = get (0, "units");
  unwind_protect
    set (0, "units", "pixels");
    screensize = get (0, "screensize");
  unwind_protect_cleanup
    set (0, "units", units);
  end_unwind_protect

  ## give the monitor a margin so that the figure must not just
  ## marginally be on the monitor.
  margin = 30;
  screensize(1:2) += margin;
  screensize(3:4) -= margin;

  hfigs = hfigs(strcmp (get (hfigs, "visible"), "on"));
  for hf = hfigs'
    units = get (hf, "units");
    unwind_protect
      set (hf, "units", "pixels");
      pos = get (hf, "position");
      ## Test if (in order):
      ## The left side is outside the right side of the screen
      ## The bottom is above the top of the screen
      ## The right side is outside the left of the screen
      ## the top is below the bottom of the screen
      if (pos(1) > screensize(3)
          || pos(2) > screensize(4)
          || pos(1)+pos(3) < screensize(1)
          || pos(2)+pos(4) < screensize(2))

        ## the new position will be at the top left of the screen
        ## (all moved figures will overlap).  The bottom left is chosen
        ## instead of the top left because that allows for the unknown
        ## amount of space for the menu bar and the title bar.
        pos(1) = screensize(1);
        pos(2) = screensize(2);
        set (hf, "position", pos);
      endif
    unwind_protect_cleanup
      set (hf, "units", units);
    end_unwind_protect
  endfor

endfunction

