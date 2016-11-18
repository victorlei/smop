## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn  {Function File} {} barh (@var{y})
## @deftypefnx {Function File} {} barh (@var{x}, @var{y})
## @deftypefnx {Function File} {} barh (@dots{}, @var{w})
## @deftypefnx {Function File} {} barh (@dots{}, @var{style})
## @deftypefnx {Function File} {} barh (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} barh (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} barh (@dots{}, @var{prop}, @var{val}, @dots{})
## Produce a horizontal bar graph from two vectors of X-Y data.
##
## If only one argument is given, it is taken as a vector of Y values
## and the X coordinates are the range @code{1:numel (@var{y})}.
##
## The optional input @var{w} controls the width of the bars.  A value of
## 1.0 will cause each bar to exactly touch any adjacent bars.
## The default width is 0.8.
##
## If @var{y} is a matrix, then each column of @var{y} is taken to be a
## separate bar graph plotted on the same graph.  By default the columns
## are plotted side-by-side.  This behavior can be changed by the @var{style}
## argument which can take the following values:
##
## @table @asis
## @item @qcode{"grouped"} (default)
## Side-by-side bars with a gap between bars and centered over the Y-coordinate.
##
## @item  @qcode{"stacked"}
## Bars are stacked so that each Y value has a single bar composed of
## multiple segments.
##
## @item @qcode{"hist"}
## Side-by-side bars with no gap between bars and centered over the
## Y-coordinate.
##
## @item @qcode{"histc"}
## Side-by-side bars with no gap between bars and left-aligned to the
## Y-coordinate.
## @end table
##
## Optional property/value pairs are passed directly to the underlying patch
## objects.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## bar series hggroup.  For a description of the use of the
## bar series, @pxref{XREFbar,,bar}.
## @seealso{bar, hist, pie, plot, patch}
## @end deftypefn

## Author: jwe

function varargout = barh (varargin)
  varargout = cell (nargout, 1);
  [varargout{:}] = __bar__ (false, "barh", varargin{:});
endfunction


%!demo
%! clf;
%! x = rand (10, 1);
%! barh (x);
%! title ('barh() graph')

%!demo
%! clf;
%! h = barh (rand (5, 3));
%! set (h(1), 'facecolor', 'r')
%! set (h(2), 'facecolor', 'g')
%! set (h(3), 'facecolor', 'b')
%! title ('barh() graph w/multiple bars')

