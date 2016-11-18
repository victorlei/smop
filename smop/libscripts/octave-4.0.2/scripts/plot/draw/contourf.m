## Copyright (C) 2007-2015 Kai Habel
## Copyright (C) 2003 Shai Ayal
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
## @deftypefn  {Function File} {} contourf (@var{z})
## @deftypefnx {Function File} {} contourf (@var{z}, @var{vn})
## @deftypefnx {Function File} {} contourf (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} contourf (@var{x}, @var{y}, @var{z}, @var{vn})
## @deftypefnx {Function File} {} contourf (@dots{}, @var{style})
## @deftypefnx {Function File} {} contourf (@var{hax}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contourf (@dots{})
## Create a 2-D contour plot with filled intervals.
##
## Plot level curves (contour lines) of the matrix @var{z} and fill the region
## between lines with colors from the current colormap.
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
## The optional output @var{c} contains the contour levels in @code{contourc}
## format.
##
## The optional return value @var{h} is a graphics handle to the hggroup
## comprising the contour lines.
##
## The following example plots filled contours of the @code{peaks} function.
##
## @example
## @group
## [x, y, z] = peaks (50);
## contourf (x, y, z, -7:9)
## @end group
## @end example
## @seealso{ezcontourf, contour, contourc, contour3, clabel, meshc, surfc, caxis, colormap, plot}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Author: Shai Ayal <shaiay@users.sourceforge.net>

function [c, h] = contourf (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("contour", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    [ctmp, htmp] = __contour__ (hax, "none", "fill", "on",
                                     "linecolor", "black", varargin{:});
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    c = ctmp;
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! [x, y, z] = peaks (50);
%! contourf (x, y, z, -7:9);
%! title ({'contourf() plot (filled contour lines)'; 'Z = peaks()'});

%!demo
%! clf;
%! colormap ('default');
%! [theta, r] = meshgrid (linspace (0,2*pi,64), linspace (0,1,64));
%! [X, Y] = pol2cart (theta, r);
%! Z = sin (2*theta) .* (1-r);
%! contourf (X, Y, abs (Z), 10);
%! title ({'contourf() plot'; 'polar fcn: Z = sin (2*theta) * (1-r)'});

%!demo
%! clf;
%! colormap ('default');
%! x = linspace (-2, 2);
%! [x, y] = meshgrid (x);
%! z = sqrt (x.^2 + y.^2) ./ (x.^2 + y.^2 + 1);
%! contourf (x, y, z, [0.4, 0.4]);
%! title ('Hole should be filled with the background color');

