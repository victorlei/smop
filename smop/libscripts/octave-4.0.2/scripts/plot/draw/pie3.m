## Copyright (C) 2007-2015 David Bateman
## Copyright (C) 2010 Kai Habel
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
## @deftypefn  {Function File} {} pie3 (@var{x})
## @deftypefnx {Function File} {} pie3 (@dots{}, @var{explode})
## @deftypefnx {Function File} {} pie3 (@dots{}, @var{labels})
## @deftypefnx {Function File} {} pie3 (@var{hax}, @dots{});
## @deftypefnx {Function File} {@var{h} =} pie3 (@dots{});
## Plot a 3-D pie chart.
##
## Called with a single vector argument, produces a 3-D pie chart of the
## elements in @var{x}.  The size of the ith slice is the percentage that the
## element @var{x}i represents of the total sum of @var{x}:
## @code{pct = @var{x}(i) / sum (@var{x})}.
##
## The optional input @var{explode} is a vector of the same length as @var{x}
## that, if nonzero, "explodes" the slice from the pie chart.
##
## The optional input @var{labels} is a cell array of strings of the same
## length as @var{x} specifying the label for each slice.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a list of graphics handles to the
## patch, surface, and text objects generating the plot.
##
## Note: If @code{sum (@var{x}) @leq{} 1} then the elements of @var{x} are
## interpreted as percentages directly and are not normalized by @code{sum (x)}.
## Furthermore, if the sum is less than 1 then there will be a missing slice
## in the pie plot to represent the missing, unspecified percentage.
##
## @seealso{pie, bar, hist, rose}
## @end deftypefn

## Very roughly based on pie.m from octave-forge whose author was
## Daniel Heiserer <Daniel.heiserer@physik.tu-muenchen.de>

function h = pie3 (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("pie3", varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    htmp = __pie__ ("pie3", hax, varargin{:});
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
%! clf;
%! pie3 ([5:-1:1], [0, 0, 1, 0, 0]);
%! colormap ([1,0,0;0,1,0;0,0,1;1,1,0;1,0,1;0,1,1]);
%! title ('pie3() with exploded wedge');

%!demo
%! clf;
%! pie3 ([3, 2, 1], [0, 0, 1], {'Cheddar', 'Swiss', 'Camembert'});
%! colormap ([1,0,0;0,1,0;0,0,1;1,1,0;1,0,1;0,1,1]);
%! axis ([-2,2,-2,2]);
%! title ('pie3() with labels');

%!demo
%! clf;
%! pie3 ([0.17, 0.34, 0.41], {'Cheddar', 'Swiss', 'Camembert'});
%! colormap ([1,0,0;0,1,0;0,0,1;1,1,0;1,0,1;0,1,1]);
%! axis ([-2,2,-2,2]);
%! title ('pie3() with missing slice');

