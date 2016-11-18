## Copyright (C) 2007-2015 David BAteman
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
## @deftypefn  {Function File} {} contour3 (@var{z})
## @deftypefnx {Function File} {} contour3 (@var{z}, @var{vn})
## @deftypefnx {Function File} {} contour3 (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} contour3 (@var{x}, @var{y}, @var{z}, @var{vn})
## @deftypefnx {Function File} {} contour3 (@dots{}, @var{style})
## @deftypefnx {Function File} {} contour3 (@var{hax}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contour3 (@dots{})
## Create a 3-D contour plot.
##
## @code{contour3} plots level curves (contour lines) of the matrix @var{z}
## at a Z level corresponding to each contour.  This is in contrast to
## @code{contour} which plots all of the contour lines at the same Z level
## and produces a 2-D plot.
##
## The level curves are taken from the contour matrix @var{c} computed by
## @code{contourc} for the same arguments; see the latter for their
## interpretation.
##
## The appearance of contour lines can be defined with a line style @var{style}
## in the same manner as @code{plot}.  Only line style and color are used;
## Any markers defined by @var{style} are ignored.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional output @var{c} are the contour levels in @code{contourc} format.
##
## The optional return value @var{h} is a graphics handle to the hggroup
## comprising the contour lines.
##
## Example:
##
## @example
## @group
## contour3 (peaks (19));
## colormap cool;
## hold on;
## surf (peaks (19), "facecolor", "none", "edgecolor", "black");
## @end group
## @end example
##
## @seealso{contour, contourc, contourf, clabel, meshc, surfc, caxis, colormap, plot}
## @end deftypefn

function [c, h] = contour3 (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("contour3", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    [ctmp, htmp] = __contour__ (hax, "auto", varargin{:});
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (! ishold ())
    set (hax, "view", [-37.5, 30], "box", "off",
              "xgrid", "on", "ygrid", "on", "zgrid", "on");
  endif

  if (nargout > 0)
    c = ctmp;
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap (cool (64));
%! surf (peaks (19), 'facecolor', 'none', 'edgecolor', [0.85 0.85 0.85]);
%! hold on;
%! contour3 (peaks (19));
%! hold off;
%! axis tight;
%! zlim auto;
%! view (315, 17);
%! title ({'contour3 of peaks() function', 'gray surf() shows peaks function'});

