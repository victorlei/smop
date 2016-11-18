## Copyright (C) 1993-2015 Shai Ayal
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
## @deftypefn  {Function File} {} contour (@var{z})
## @deftypefnx {Function File} {} contour (@var{z}, @var{vn})
## @deftypefnx {Function File} {} contour (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} contour (@var{x}, @var{y}, @var{z}, @var{vn})
## @deftypefnx {Function File} {} contour (@dots{}, @var{style})
## @deftypefnx {Function File} {} contour (@var{hax}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contour (@dots{})
## Create a 2-D contour plot.
##
## Plot level curves (contour lines) of the matrix @var{z}, using the
## contour matrix @var{c} computed by @code{contourc} from the same
## arguments; see the latter for their interpretation.
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
## Example:
##
## @example
## @group
## x = 0:2;
## y = x;
## z = x' * y;
## contour (x, y, z, 2:3)
## @end group
## @end example
##
## @seealso{ezcontour, contourc, contourf, contour3, clabel, meshc, surfc, caxis, colormap, plot}
##
## @end deftypefn

## Author: Shai Ayal <shaiay@users.sourceforge.net>

function [c, h] = contour (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("contour", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    [ctmp, htmp] = __contour__ (hax, "none", varargin{:});
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
%! [x, y, z] = peaks ();
%! contour (x, y, z);
%! title ({'contour() plot (isolines of constant Z)'; 'Z = peaks()'});

%!demo
%! clf;
%! colormap ('default');
%! [theta, r] = meshgrid (linspace (0,2*pi,64), linspace (0,1,64));
%! [X, Y] = pol2cart (theta, r);
%! Z = sin (2*theta) .* (1-r);
%! contour (X, Y, abs (Z), 10);
%! title ({'contour() plot'; 'polar fcn: Z = sin (2*theta) * (1-r)'});

%!demo
%! clf;
%! colormap ('default');
%! z = peaks ();
%! contour (z, [0 0]);
%! title ({'contour() plot with single isoline at Z == 0'; 'Z = peaks()'});

%!test
%! hf = figure ("visible", "off");
%! clf (hf);
%! unwind_protect
%!   [x, y, z] = peaks ();
%!   [c, h] = contour (x, y, z);
%!   levellist = -6:6;
%!   set (h, "levellist", levellist);
%!   assert (get (h, "levellist"), levellist)
%!   assert (get (h, "levellistmode"), "manual")
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! clf (hf);
%! unwind_protect
%!   [x, y, z] = peaks ();
%!   [c, h] = contour (x, y, z);
%!   levelstep = 3;
%!   set (h, "levelstep", levelstep);
%!   assert (get (h, "levelstep"), levelstep)
%!   assert (get (h, "levelstepmode"), "manual")
%!   assert (get (h, "levellist"), -6:levelstep:6)
%!   assert (get (h, "levellistmode"), "auto")
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

