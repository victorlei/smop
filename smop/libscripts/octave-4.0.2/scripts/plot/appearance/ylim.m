## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Function File} {@var{ylimits} =} ylim ()
## @deftypefnx {Function File} {@var{xmode} =} ylim ("mode")
## @deftypefnx {Function File} {} ylim ([@var{y_lo} @var{y_hi}])
## @deftypefnx {Function File} {} ylim ("auto")
## @deftypefnx {Function File} {} ylim ("manual")
## @deftypefnx {Function File} {} ylim (@var{hax}, @dots{})
## Query or set the limits of the y-axis for the current plot.
##
## Called without arguments @code{ylim} returns the y-axis limits of the
## current plot.
##
## With the input query @qcode{"mode"}, return the current
## y-limit calculation mode which is either @qcode{"auto"} or @qcode{"manual"}.
##
## If passed a 2-element vector [@var{y_lo} @var{y_hi}], the limits of the
## y-axis are set to these values and the mode is set to @qcode{"manual"}.
##
## The current plotting mode can be changed by using either @qcode{"auto"}
## or @qcode{"manual"} as the argument.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
## @seealso{xlim, zlim, axis, set, get, gca}
## @end deftypefn

function retval = ylim (varargin)
  ret = __axis_limits__ ("ylim", varargin{:});

  if (! isempty (ret))
    retval = ret;
  endif
endfunction


%!demo
%! clf;
%! line ();
%! ylim ([0.2, 0.8]);
%! title ('ylim is [0.2, 0.8]');
%! assert (ylim (), [0.2, 0.8]);

%!demo
%! clf;
%! line ();
%! ylim ('auto');
%! title ('ylim is auto');
%! assert (ylim ('mode'), 'auto');

%!demo
%! clf;
%! plot3 ([0,1], [0,1], [0,1]);
%! ylim ([0.2, 0.8]);
%! title ('ylim is [0.2, 0.8]');
%! assert (ylim (), [0.2, 0.8]);

%!demo
%! clf;
%! plot3 ([0,1], [0,1], [0,1]);
%! ylim ('auto');
%! title ('ylim is auto');
%! assert (ylim ('mode'), 'auto');

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   limy = [0, 1.1];
%!   plot3 ([0,1], [0,1], [0,1]);
%!   ylim (limy);
%!   assert (get (gca, "ylim"), limy, eps);
%!   assert (ylim ("mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot3 ([0,1], [0,1.1], [0, 1]);
%!   assert (get (gca, "ylim"), [0, 1.4], eps);
%!   assert (ylim ("mode"), "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

