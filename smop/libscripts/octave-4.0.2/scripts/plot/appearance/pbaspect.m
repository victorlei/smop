## Copyright (C) 2010-2015 Ben Abbott
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
## @deftypefn  {Function File} {@var{plot_box_aspect_ratio} =} pbaspect ( )
## @deftypefnx {Function File} {} pbaspect (@var{plot_box_aspect_ratio})
## @deftypefnx {Function File} {} pbaspect (@var{mode})
## @deftypefnx {Function File} {@var{plot_box_aspect_ratio_mode} =} pbaspect ("mode")
## @deftypefnx {Function File} {} pbaspect (@var{hax}, @dots{})
##
## Query or set the plot box aspect ratio of the current axes.
##
## The aspect ratio is a normalized 3-element vector representing the rendered
## lengths of the x, y, and z axes.
##
## @code{pbaspect(@var{mode})}
##
## Set the plot box aspect ratio mode of the current axes.  @var{mode} is
## either @qcode{"auto"} or @qcode{"manual"}.
##
## @code{pbaspect ("mode")}
##
## Return the plot box aspect ratio mode of the current axes.
##
## @code{pbaspect (@var{hax}, @dots{})}
##
## Operate on the axes in handle @var{hax} instead of the current axes.
##
## @seealso{axis, daspect, xlim, ylim, zlim}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-01-26

function pbratio = pbaspect (varargin)

  ## Grab axes handle if present
  if (nargin > 0)
    if (isscalar (varargin{1}) && isaxes (varargin{1}))
      hax = varargin{1};
      varargin = varargin(2:end);
    else
      hax = gca ();
    endif
  else
    hax = gca ();
  endif

  nargin = numel (varargin);
  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0)
    pbratio = get (hax, "plotboxaspectratio");
  else
    arg = varargin{1};
    if (isnumeric (arg))
      if (numel (arg) == 2)
        set (hax, "plotboxaspectratio", [arg, 1]);
      elseif (numel (arg) == 3)
        set (hax, "plotboxaspectratio", arg);
      else
        error ("pbaspect: PLOT_BOX_ASPECT_RATIO must be a 2 or 3 element vector");
      endif
    elseif (ischar (arg))
      arg = tolower (arg);
      switch (arg)
        case "auto"
          set (hax, "plotboxaspectratiomode", "auto");
        case "manual"
          set (hax, "plotboxaspectratiomode", "manual");
        case "mode"
          pbratio = get (hax, "plotboxaspectratiomode");
        otherwise
          error ("pbaspect: Invalid mode <%s>", arg);
      endswitch
    else
      print_usage ();
    endif
  endif

endfunction


%!demo
%! clf;
%! x = 0:0.01:4;
%! plot (x,cos(x), x,sin(x));
%! pbaspect ([1 1 1]);
%! title ('plot box is square');

%!demo
%! clf;
%! x = 0:0.01:4;;
%! plot (x,cos(x), x,sin(x));
%! pbaspect ([2 1 1]);
%! title ('plot box aspect ratio is 2x1');

%!demo
%! clf;
%! x = 0:0.01:4;
%! plot (x,cos(x), x,sin(x));
%! daspect ([1 1 1]);
%! pbaspect ([2 1 1]);
%! title ('plot box is 2x1, and axes [0 4 -1 1]');

