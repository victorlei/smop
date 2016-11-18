## Copyright (C) 2007-2015 Michael Goffioul
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
## @deftypefn  {Command} {} hidden
## @deftypefnx {Command} {} hidden on
## @deftypefnx {Command} {} hidden off
## @deftypefnx {Function File} {@var{mode} =} hidden (@dots{})
## Control mesh hidden line removal.
##
## When called with no argument the hidden line removal state is toggled.
##
## When called with one of the modes @qcode{"on"} or @qcode{"off"} the state
## is set accordingly.
##
## The optional output argument @var{mode} is the current state.
##
## Hidden Line Removal determines what graphic objects behind a mesh plot
## are visible.  The default is for the mesh to be opaque and lines behind
## the mesh are not visible.  If hidden line removal is turned off then
## objects behind the mesh can be seen through the faces (openings) of the
## mesh, although the mesh grid lines are still opaque.
##
## @seealso{mesh, meshc, meshz, ezmesh, ezmeshc, trimesh, waterfall}
## @end deftypefn

function state = hidden (mode = "toggle")

  if (nargin > 2)
    print_usage ();
  elseif (nargin == 1)
    if (! ischar (mode))
      error ("hidden: expecting MODE to be a string");
    elseif (! any (strcmpi (mode, {"on", "off"})))
      error ('hidden: MODE must be "on" or "off"');
    endif
  endif

  for h = (get (gca (), "children")).';
    htype = get (h, "type");
    htag = get (h, "tag");
    if (strcmp (htype, "surface") || strcmp (htag, "trimesh"))
      fc = get (h, "facecolor");
      if ((! ischar (fc) && is_white (fc))
          || (ischar (fc) && strcmp (fc, "none")))
        switch (mode)
          case "on"
            set (h, "facecolor", "w");
          case "off"
            set (h, "facecolor", "none");
          case "toggle"
            if (ischar (fc))
              set (h, "facecolor", "w");
              mode = "on";
            else
              set (h, "facecolor", "none");
              mode = "off";
            endif
        endswitch
      endif
    endif
  endfor

  if (nargout > 0)
    state = mode;
  endif

endfunction

function retval = is_white (color)
  retval = all (color == 1);
endfunction

