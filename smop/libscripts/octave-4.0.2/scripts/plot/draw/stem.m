## Copyright (C) 2006-2015 Michel D. Schmid
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
## @deftypefn  {Function File} {} stem (@var{y})
## @deftypefnx {Function File} {} stem (@var{x}, @var{y})
## @deftypefnx {Function File} {} stem (@dots{}, @var{linespec})
## @deftypefnx {Function File} {} stem (@dots{}, "filled")
## @deftypefnx {Function File} {} stem (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} stem (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} stem (@dots{})
## Plot a 2-D stem graph.
##
## If only one argument is given, it is taken as the y-values and the
## x-coordinates are taken from the indices of the elements.
##
## If @var{y} is a matrix, then each column of the matrix is plotted as
## a separate stem graph.  In this case @var{x} can either be a vector,
## the same length as the number of rows in @var{y}, or it can be a
## matrix of the same size as @var{y}.
##
## The default color is @qcode{"b"} (blue), the default line style is
## @qcode{"-"}, and the default marker is @qcode{"o"}.  The line style can
## be altered by the @code{linespec} argument in the same manner as the
## @code{plot} command.  If the @qcode{"filled"} argument is present the
## markers at the top of the stems will be filled in.  For example,
##
## @example
## @group
## x = 1:10;
## y = 2*x;
## stem (x, y, "r");
## @end group
## @end example
##
## @noindent
## plots 10 stems with heights from 2 to 20 in red;
##
## Optional property/value pairs may be specified to control the appearance
## of the plot.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a handle to a @nospell{"stem series"}
## hggroup.  The single hggroup handle has all of the graphical elements
## comprising the plot as its children; This allows the properties of
## multiple graphics objects to be changed by modifying just a single
## property of the @nospell{"stem series"} hggroup.
##
## For example,
##
## @example
## @group
## x = [0:10]';
## y = [sin(x), cos(x)]
## h = stem (x, y);
## set (h(2), "color", "g");
## set (h(1), "basevalue", -1)
## @end group
## @end example
##
## @noindent
## changes the color of the second @nospell{"stem series"} and moves the base
## line of the first.
##
## Stem Series Properties
##
## @table @asis
## @item linestyle
## The linestyle of the stem.  (Default: @qcode{"-"})
##
## @item linewidth
## The width of the stem.  (Default: 0.5)
##
## @item color
## The color of the stem, and if not separately specified, the marker.
## (Default: @qcode{"b"} [blue])
##
## @item marker
## The marker symbol to use at the top of each stem.  (Default: @qcode{"o"})
##
## @item markeredgecolor
## The edge color of the marker.  (Default: @qcode{"color"} property)
##
## @item markerfacecolor
## The color to use for @nospell{"filling"} the marker.
## (Default: @qcode{"none"} [unfilled])
##
## @item markersize
## The size of the marker.  (Default: 6)
##
## @item baseline
## The handle of the line object which implements the baseline.  Use @code{set}
## with the returned handle to change graphic properties of the baseline.
##
## @item basevalue
## The y-value where the baseline is drawn.  (Default: 0)
## @end table
## @seealso{stem3, bar, hist, plot, stairs}
## @end deftypefn

## Author: Michel D. Schmid <michaelschmid@users.sourceforge.net>
## Adapted-by: jwe

function h = stem (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  htmp = __stem__ (false, varargin{:});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! y = 1:10;
%! stem (y);
%! title ('stem plot of y-values only');

%!demo
%! clf;
%! x = 1:10;
%! y = 2*x;
%! stem (x, y);
%! title ('stem plot of x and y-values');

%!demo
%! clf;
%! x = 1:10;
%! y = 2*x;
%! h = stem (x, y, 'r');
%! title ('stem plot with modified color');

%!demo
%! clf;
%! x = 1:10;
%! y = 2*x;
%! h = stem (x, y, '-.k');
%! title ('stem plot with modified line style and color');

%!demo
%! clf;
%! x = 1:10;
%! y = 2*x;
%! h = stem (x, y, '-.ks');
%! title ('stem plot with modified line style, color, and marker');

%!demo
%! clf;
%! x = 1:10;
%! y = 2*x;
%! h = stem (x, y, 'filled');
%! title ('stem plot with "filled" markers');

%!demo
%! clf;
%! x = 1:10;
%! y = 2*x;
%! h = stem (x, y, 'markerfacecolor', [1 0 1]);
%! title ('stem plot modified with property/value pair');

%!demo
%! clf;
%! x = (0 : 10)';
%! y = [sin(x), cos(x)];
%! h = stem (x, y);
%! set (h(2), 'color', 'g');
%! set (h(1), 'basevalue', -0.75);
%! title ('stem plots modified through hggroup handle');

%!demo
%! clf;
%! N = 11;
%! x = 0:(N-1);
%! y = rand (1, N);
%! hs = stem (x(1), y(1));
%! set (gca (), 'xlim', [1, N-1], 'ylim', [0, 1]);
%! title ('stem plot data modified through hggroup handle');
%! for k=2:N
%!   set (hs, 'xdata', x(1:k), 'ydata', y(1:k))
%!   drawnow ();
%!   pause (0.2);
%! end

%!test
%! ## stemseries share the same baseline and basevalue
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = stem ([1 2; 1.5 2.5], [1 1;2 2]);
%!   assert (get (h(1), "baseline"), get (h(2), "baseline"))
%!   bv = 0.3;
%!   set (h(1), "basevalue", bv)
%!   assert (get (get (h(1), "baseline"), "basevalue"), bv)
%!   assert (get (h(1), "basevalue"), bv)
%!   assert (get (h(2), "basevalue"), bv)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error stem ()
%!error <can not define Z for 2-D stem plot> stem (1,2,3)
%!error <Y must be a vector or 2-D array> stem (ones (2,2,2))
%!error <X and Y must be numeric> stem ({1})
%!error <X and Y must be numeric> stem (1, {1})
%!error <inconsistent sizes for X and Y> stem (1:2, 1:3)
%!error <inconsistent sizes for X and Y> stem (1:2, ones (3,3))
%!error <inconsistent sizes for X and Y> stem (ones (2,2), ones (3,3))
%!error <No value specified for property "FOO"> stem (1, "FOO")

