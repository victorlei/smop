## Copyright (C) 2008-2015 Ben Abbott
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
## @deftypefn  {Function File} {} comet (@var{y})
## @deftypefnx {Function File} {} comet (@var{x}, @var{y})
## @deftypefnx {Function File} {} comet (@var{x}, @var{y}, @var{p})
## @deftypefnx {Function File} {} comet (@var{hax}, @dots{})
## Produce a simple comet style animation along the trajectory provided by
## the input coordinate vectors (@var{x}, @var{y}).
##
## If @var{x} is not specified it defaults to the indices of @var{y}.
##
## The speed of the comet may be controlled by @var{p}, which represents the
## time each point is displayed before moving to the next one.  The default for
## @var{p} is 0.1 seconds.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
## @seealso{comet3}
## @end deftypefn

## Author: Ben Abbott bpabbott@mac.com
## Created: 2008-09-21

function comet (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("comet", varargin{:});

  if (nargin == 0)
    print_usage ();
  elseif (nargin == 1)
    y = varargin{1};
    x = 1:numel (y);
    p = 0.1;
  elseif (nargin == 2)
    x = varargin{1};
    y = varargin{2};
    p = 0.1;
  elseif (nargin == 3)
    x = varargin{1};
    y = varargin{2};
    p = varargin{3};
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    limits = [min(x), max(x), min(y), max(y)];
    num = numel (y);
    dn = round (num/10);

    hl = plot (x(1), y(1), "color", "r", "marker", "none",
               x(1), y(1), "color", "g", "marker", "none",
               x(1), y(1), "color", "b", "marker", "o");
    axis (limits);  # set manual limits to speed up plotting

    for n = 2:(num+dn);
      m = n - dn;
      m = max ([m, 1]);
      k = min ([n, num]);
      set (hl(1), "xdata", x(1:m), "ydata", y(1:m));
      set (hl(2), "xdata", x(m:k), "ydata", y(m:k));
      set (hl(3), "xdata", x(k),   "ydata", y(k));
      drawnow ();
      pause (p);
    endfor

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

endfunction


%!demo
%! clf;
%! title ('comet() animation');
%! hold on;
%! t = 0:.1:2*pi;
%! x = cos (2*t) .* (cos (t).^2);
%! y = sin (2*t) .* (sin (t).^2);
%! comet (x, y, 0.05);
%! hold off;

