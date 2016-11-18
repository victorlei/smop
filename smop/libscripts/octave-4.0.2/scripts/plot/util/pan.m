## Copyright (C) 2014-2015 Andreas Weber
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
## @deftypefn  {Command} {} pan
## @deftypefnx {Command} {} pan on
## @deftypefnx {Command} {} pan off
## @deftypefnx {Command} {} pan xon
## @deftypefnx {Command} {} pan yon
## @deftypefnx {Function File} {} pan (@var{hfig}, @var{option})
## Control the interactive panning mode of a figure in the GUI.
##
## Given the option @qcode{"on"} or @qcode{"off"}, set the interactive
## pan mode on or off.
##
## With no arguments, toggle the current pan mode on or off.
##
## Given the option @qcode{"xon"} or @qcode{"yon"}, enable pan mode
## for the x or y axis only.
##
## If the first argument @var{hfig} is a figure, then operate on the given
## figure rather than the current figure as returned by @code{gcf}.
##
## @seealso{rotate3d, zoom}
## @end deftypefn

function pan (varargin)

  hfig = NaN;

  nargs = nargin;

  if (nargs > 2)
    print_usage ();
  endif

  if (nargin == 1 && nargout > 0 && isfigure (varargin{1}))
    error ("pan_object_handle = pan (hfig): not implemented");
  endif

  if (nargs == 2)
    hfig = varargin{1};
    if (isfigure (hfig))
      varargin(1) = [];
      nargs--;
    else
      error ("pan: expecting figure handle as first argument");
    endif
  endif

  if (isnan (hfig))
    hfig = gcf ();
  endif

  if (nargs == 0)
    pm = get (hfig, "__pan_mode__");
    if (strcmp (pm.Enable, "on"))
      pm.Enable = "off";
    else
      pm.Enable = "on";
    endif
    set (hfig, "__pan_mode__", pm);
    update_mouse_mode (hfig, pm.Enable);
  elseif (nargs == 1)
    arg = varargin{1};
    if (ischar (arg))
      switch (arg)
        case {"on", "off", "xon", "yon"}
          pm = get (hfig, "__pan_mode__");
          switch (arg)
            case {"on", "off"}
              pm.Enable = arg;
              pm.Motion = "both";
            case "xon"
              pm.Enable = "on";
              pm.Motion = "horizontal";
            case "yon"
              pm.Enable = "on";
              pm.Motion = "vertical";
          endswitch
          set (hfig, "__pan_mode__", pm);
          update_mouse_mode (hfig, arg);
        otherwise
          error ("pan: unrecognized option '%s'", arg);
      endswitch
    else
      error ("pan: wrong type argument '%s'", class (arg));
    endif
  endif

endfunction

function update_mouse_mode (hfig, arg)
  if (strcmp (arg, "off"))
    set (hfig, "__mouse_mode__", "none");
  else
    ## FIXME: Is there a better way other than calling these
    ## functions to set the other mouse mode Enable fields to
    ## "off"?
    rotate3d ("off");
    zoom ("off");
    set (hfig, "__mouse_mode__", "pan");
  endif
endfunction
