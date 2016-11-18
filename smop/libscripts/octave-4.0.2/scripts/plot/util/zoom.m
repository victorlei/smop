## Copyright (C) 2014-2015 John W. Eaton
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
## @deftypefn  {Command} {} zoom
## @deftypefnx {Command} {} zoom (@var{factor})
## @deftypefnx {Command} {} zoom on
## @deftypefnx {Command} {} zoom off
## @deftypefnx {Command} {} zoom xon
## @deftypefnx {Command} {} zoom yon
## @deftypefnx {Command} {} zoom out
## @deftypefnx {Command} {} zoom reset
## @deftypefnx {Command} {} zoom (@var{hfig}, @var{option})
## Zoom the current axes object or control the interactive zoom mode of a
## figure in the GUI.
##
## Given a numeric argument greater than zero, zoom by the given factor.  If
## the zoom factor is greater than one, zoom in on the plot.  If the factor
## is less than one, zoom out.  If the zoom factor is a two- or three-element
## vector, then the elements specify the zoom factors for the x, y, and z
## axes respectively.
##
## Given the option @qcode{"on"} or @qcode{"off"}, set the interactive zoom
## mode on or off.
##
## With no arguments, toggle the current zoom mode on or off.
##
## Given the option @qcode{"xon"} or @qcode{"yon"}, enable zoom mode for the
## x or y-axis only.
##
## Given the option @qcode{"out"}, zoom to the initial zoom setting.
##
## Given the option @qcode{"reset"}, store the current zoom setting so that
## @code{zoom out} will return to this zoom level.
##
## If the first argument @var{hfig} is a figure, then operate on the given
## figure rather than the current figure as returned by @code{gcf}.
##
## @seealso{pan, rotate3d}
## @end deftypefn

## Eventually we need to also support these features:
## @deftypefnx {Command} {zoom_object_handle =} zoom (@var{hfig})

function zoom (varargin)

  nargs = nargin;
  if (nargs > 2)
    print_usage ();
  endif

  if (nargs == 1 && nargout > 0 && isfigure (varargin{1}))
    error ("zoom_object_handle = zoom (hfig): not implemented");
  endif

  hfig = NaN;
  if (nargs == 2)
    hfig = varargin{1};
    if (isfigure (hfig))
      varargin(1) = [];
      nargs--;
    else
      error ("zoom: expecting figure handle as first argument");
    endif
  endif

  if (isnan (hfig))
    hfig = gcf ();
  endif

  if (nargs == 0)
    zm = get (hfig, "__zoom_mode__");
    if (strcmp (zm.Enable, "on"))
      zm.Enable = "off";
    else
      zm.Enable = "on";
    endif
    set (hfig, "__zoom_mode__", zm);
    update_mouse_mode (hfig, zm.Enable);
  elseif (nargs == 1)
    arg = varargin{1};
    if (isnumeric (arg))
      factor = arg;
      switch (numel (factor))
        case 2
          xfactor = factor(1);
          yfactor = factor(2);
        case 1
          xfactor = yfactor = factor;
        otherwise
          error ("zoom: invalid factor");
      endswitch
      if (xfactor < 0 || yfactor < 0)
        error ("zoom: factor must be greater than 1");
      elseif (xfactor == 1 && yfactor == 1)
        return;
      endif
      cax = get (hfig, "currentaxes");
      if (! isempty (cax))
        if (xfactor != 1)
          if (yfactor != 1)
            mode = "both";
          else
            mode = "horizontal";
          endif
        else
          if (yfactor != 1)
            mode = "vertical";
          endif
        endif
        __zoom__ (cax, mode, factor);
      endif
    elseif (ischar (arg))
      switch (arg)
        case {"on", "off", "xon", "yon"}
          zm = get (hfig, "__zoom_mode__");
          switch (arg)
            case {"on", "off"}
              zm.Enable = arg;
              zm.Motion = "both";
            case "xon"
              zm.Enable = "on";
              zm.Motion = "horizontal";
            case "yon"
              zm.Enable = "on";
              zm.Motion = "vertical";
          endswitch
          set (hfig, "__zoom_mode__", zm);
          update_mouse_mode (hfig, arg);
        case "out"
          cax = get (hfig, "currentaxes");
          if (! isempty (cax))
            __zoom__ (cax, "out");
          endif
        case "reset"
          cax = get (hfig, "currentaxes");
          if (! isempty (cax))
            __zoom__ (cax, "reset");
          endif
        otherwise
          error ("zoom: unrecognized option '%s'", arg);
      endswitch
    else
      error ("zoom: wrong type argument '%s'", class (arg));
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
    pan ("off");
    rotate3d ("off");
    set (hfig, "__mouse_mode__", "zoom");
  endif
endfunction


%!demo
%! clf;
%! sombrero ();
%! pause (1);
%! %% zoom in by a factor of 2
%! zoom (2);
%! pause (1);
%! %% return to original zoom level
%! zoom out;
%! pause (1);
%! %% zoom in by a factor of 2
%! zoom (2);
%! pause (1);
%! %% set this zoom level as the "initial zoom level"
%! %% and zoom in some more
%! zoom reset;
%! zoom (2);
%! pause (1);
%! %% return to zoom level set by last call to "zoom reset"
%! zoom out;

