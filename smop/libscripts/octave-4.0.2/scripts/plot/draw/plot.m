## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn  {Function File} {} plot (@var{y})
## @deftypefnx {Function File} {} plot (@var{x}, @var{y})
## @deftypefnx {Function File} {} plot (@var{x}, @var{y}, @var{fmt})
## @deftypefnx {Function File} {} plot (@dots{}, @var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} plot (@var{x1}, @var{y1}, @dots{}, @var{xn}, @var{yn})
## @deftypefnx {Function File} {} plot (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} plot (@dots{})
## Produce 2-D plots.
##
## Many different combinations of arguments are possible.  The simplest
## form is
##
## @example
## plot (@var{y})
## @end example
##
## @noindent
## where the argument is taken as the set of @var{y} coordinates and the
## @var{x} coordinates are taken to be the range @code{1:numel (@var{y})}.
##
## If more than one argument is given, they are interpreted as
##
## @example
## plot (@var{y}, @var{property}, @var{value}, @dots{})
## @end example
##
## @noindent
## or
##
## @example
## plot (@var{x}, @var{y}, @var{property}, @var{value}, @dots{})
## @end example
##
## @noindent
## or
##
## @example
## plot (@var{x}, @var{y}, @var{fmt}, @dots{})
## @end example
##
## @noindent
## and so on.  Any number of argument sets may appear.  The @var{x} and
## @var{y} values are interpreted as follows:
##
## @itemize @bullet
## @item
## If a single data argument is supplied, it is taken as the set of @var{y}
## coordinates and the @var{x} coordinates are taken to be the indices of
## the elements, starting with 1.
##
## @item
## If @var{x} and @var{y} are scalars, a single point is plotted.
##
## @item
## @code{squeeze()} is applied to arguments with more than two dimensions,
## but no more than two singleton dimensions.
##
## @item
## If both arguments are vectors, the elements of @var{y} are plotted versus
## the elements of @var{x}.
##
## @item
## If @var{x} is a vector and @var{y} is a matrix, then
## the columns (or rows) of @var{y} are plotted versus @var{x}.
## (using whichever combination matches, with columns tried first.)
##
## @item
## If the @var{x} is a matrix and @var{y} is a vector,
## @var{y} is plotted versus the columns (or rows) of @var{x}.
## (using whichever combination matches, with columns tried first.)
##
## @item
## If both arguments are matrices, the columns of @var{y} are plotted
## versus the columns of @var{x}.  In this case, both matrices must have
## the same number of rows and columns and no attempt is made to transpose
## the arguments to make the number of rows match.
## @end itemize
##
## Multiple property-value pairs may be specified, but they must appear
## in pairs.  These arguments are applied to the line objects drawn by
## @code{plot}.  Useful properties to modify are @qcode{"linestyle"},
## @qcode{"linewidth"}, @qcode{"color"}, @qcode{"marker"},
## @qcode{"markersize"}, @qcode{"markeredgecolor"}, @qcode{"markerfacecolor"}.  
## @xref{Line Properties}.  
##
## The @var{fmt} format argument can also be used to control the plot style.
## It is a string composed of four optional parts:
## "<linestyle><marker><color><;displayname;>".
## When a marker is specified, but no linestyle, only the markers are
## plotted.  Similarly, if a linestyle is specified, but no marker, then
## only lines are drawn.  If both are specified then lines and markers will
## be plotted.  If no @var{fmt} and no @var{property}/@var{value} pairs are
## given, then the default plot style is solid lines with no markers and the
## color determined by the @qcode{"colororder"} property of the current axes.
##
## Format arguments:
##
## @table @asis
## @item linestyle
##
## @multitable @columnfractions 0.06 0.94
## @item @samp{-}  @tab Use solid lines (default).
## @item @samp{--} @tab Use dashed lines.
## @item @samp{:}  @tab Use dotted lines.
## @item @samp{-.} @tab Use dash-dotted lines.
## @end multitable
##
## @item marker
##
## @multitable @columnfractions 0.06 0.94
## @item @samp{+} @tab crosshair
## @item @samp{o} @tab circle
## @item @samp{*} @tab star
## @item @samp{.} @tab point
## @item @samp{x} @tab cross
## @item @samp{s} @tab square
## @item @samp{d} @tab diamond
## @item @samp{^} @tab upward-facing triangle
## @item @samp{v} @tab downward-facing triangle
## @item @samp{>} @tab right-facing triangle
## @item @samp{<} @tab left-facing triangle
## @item @samp{p} @tab pentagram
## @item @samp{h} @tab hexagram
## @end multitable
##
## @item color
##
## @multitable @columnfractions 0.06 0.94
## @item @samp{k} @tab blacK
## @item @samp{r} @tab Red
## @item @samp{g} @tab Green
## @item @samp{b} @tab Blue
## @item @samp{m} @tab Magenta
## @item @samp{c} @tab Cyan
## @item @samp{w} @tab White
## @end multitable
##
## @item @qcode{";displayname;"}
## Here @qcode{"displayname"} is the label to use for the plot legend.
## @end table
##
## The @var{fmt} argument may also be used to assign legend labels.
## To do so, include the desired label between semicolons after the
## formatting sequence described above, e.g., @qcode{"+b;Key Title;"}.
## Note that the last semicolon is required and Octave will generate
## an error if it is left out.
##
## Here are some plot examples:
##
## @example
## plot (x, y, "or", x, y2, x, y3, "m", x, y4, "+")
## @end example
##
## This command will plot @code{y} with red circles, @code{y2} with solid
## lines, @code{y3} with solid magenta lines, and @code{y4} with points
## displayed as @samp{+}.
##
## @example
## plot (b, "*", "markersize", 10)
## @end example
##
## This command will plot the data in the variable @code{b},
## with points displayed as @samp{*} and a marker size of 10.
##
## @example
## @group
## t = 0:0.1:6.3;
## plot (t, cos(t), "-;cos(t);", t, sin(t), "-b;sin(t);");
## @end group
## @end example
##
## This will plot the cosine and sine functions and label them accordingly
## in the legend.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to
## the created line objects.
##
## To save a plot, in one of several image formats such as PostScript
## or PNG, use the @code{print} command.
##
## @seealso{axis, box, grid, hold, legend, title, xlabel, ylabel, xlim, ylim, ezplot, errorbar, fplot, line, plot3, polar, loglog, semilogx, semilogy, subplot}
## @end deftypefn

## Author: jwe

function h = plot (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("plot", varargin{:});

  if (nargs < 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    htmp = __plt__ ("plot", hax, varargin{:});
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! x = 1:5;  y = 1:5;
%! plot (x,y,'g');
%! title ('plot() of green line at 45 degrees');

%!demo
%! x = 1:5;  y = 1:5;
%! plot (x,y,'g*');
%! title ('plot() of green stars along a line at 45 degrees');

%!demo
%! x1 = 1:5;  y1 = 1:5;
%! x2 = 5:9; y2 = 5:-1:1;
%! plot (x1,y1,'bo-', x2,y2,'rs-');
%! axis ('tight');
%! title ({'plot() of blue circles ascending and red squares descending';
%!         'connecting lines drawn'});

%!demo
%! x = 0:10;
%! plot (x, rand (numel (x), 3))
%! axis ([0 10 0 1])
%! title ({'Three random variables', 'x[1x11], y[11x3]'})

%!demo
%! x = 0:10;
%! plot (x, rand (3, numel (x)))
%! axis ([0 10 0 1])
%! title ({'Three random variables', 'x[1x11], y[3x11]'})

%!demo
%! x = 0:10;
%! plot (repmat (x, 2, 1), rand (2, numel (x)), '-s')
%! axis ([0 10 0 1])
%! title ({'Vertical lines with random height and lengths', ...
%!         'x[2x11], y[2,11]'})

%!demo
%! x = 0:10;
%! plot (repmat (x(:), 1, 2), rand (numel (x), 2))
%! axis ([0 10 0 1])
%! title ({'Two random variables', 'x[11x2], y[11x2]'})

%!demo
%! x = 0:10;
%! shape = [1, 1, numel(x), 2];
%! x = reshape (repmat (x(:), 1, 2), shape);
%! y = rand (shape);
%! plot (x, y)
%! axis ([0 10 0 1])
%! title ({'Two random variables', 'squeezed from 4-d arrays'})

