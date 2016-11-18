## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn  {Function File} {} plotmatrix (@var{x}, @var{y})
## @deftypefnx {Function File} {} plotmatrix (@var{x})
## @deftypefnx {Function File} {} plotmatrix (@dots{}, @var{style})
## @deftypefnx {Function File} {} plotmatrix (@var{hax}, @dots{})
## @deftypefnx {Function File} {[@var{h}, @var{ax}, @var{bigax}, @var{p}, @var{pax}] =} plotmatrix (@dots{})
## Scatter plot of the columns of one matrix against another.
##
## Given the arguments @var{x} and @var{y} that have a matching number of
## rows, @code{plotmatrix} plots a set of axes corresponding to
##
## @example
## plot (@var{x}(:, i), @var{y}(:, j))
## @end example
##
## When called with a single argument @var{x} this is equivalent to
##
## @example
## plotmatrix (@var{x}, @var{x})
## @end example
##
## @noindent
## except that the diagonal of the set of axes will be replaced with the
## histogram @code{hist (@var{x}(:, i))}.
##
## The marker to use can be changed with the @var{style} argument, that is a
## string defining a marker in the same manner as the @code{plot} command.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} provides handles to the individual
## graphics objects in the scatter plots, whereas @var{ax} returns the
## handles to the scatter plot axis objects.
##
## @var{bigax} is a hidden axis object that surrounds the other axes, such
## that the commands @code{xlabel}, @code{title}, etc., will be associated
## with this hidden axis.
##
## Finally, @var{p} returns the graphics objects associated with the histogram
## and @var{pax} the corresponding axes objects.
##
## Example:
##
## @example
## plotmatrix (randn (100, 3), "g+")
## @end example
##
## @seealso{scatter, plot}
## @end deftypefn

function [h, ax, bigax, p, pax] = plotmatrix (varargin)

  [bigax2, varargin, nargin] = __plt_get_axis_arg__ ("plotmatrix", varargin{:});

  if (nargin > 3 || nargin < 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (bigax2))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    bigax2 = newplot (bigax2);

    [h2, ax2, p2, pax2] = __plotmatrix__ (bigax2, varargin{:});

    axes (bigax2);
    ctext = text (0, 0, "", "visible", "off",
                  "handlevisibility", "off", "xliminclude", "off",
                  "yliminclude", "off", "zliminclude", "off",
                  "deletefcn", {@plotmatrixdelete, [ax2; pax2]});
    set (bigax2, "visible", "off");

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = h2;
    ax = ax2;
    bigax = bigax2;
    p = p2;
    pax = pax2;
  endif

endfunction


%!demo
%! clf;
%! plotmatrix (randn (100, 3), 'g+');


function plotmatrixdelete (h, d, ax)
  for i = 1 : numel (ax)
    hc = ax(i);
    if (isaxes (hc) && strcmpi (get (hc, "beingdeleted"), "off"))
      parent = get (hc, "parent");
      ## If the parent is invalid or being deleted, then do nothing
      if (ishandle (parent) && strcmpi (get (parent, "beingdeleted"), "off"))
        delete (hc);
      endif
    endif
  endfor
endfunction

function [h, ax, p, pax] = __plotmatrix__ (bigax, varargin)
  have_line_spec = false;
  have_hist = false;
  parent = get (bigax, "parent");
  for i = 1 : nargin - 1
    arg = varargin{i};
    if (ischar (arg) || iscellstr (arg))
      [linespec, valid] = __pltopt__ ("plotmatrix", varargin{i}, false);
      if (valid)
        have_line_spec = true;
        linespec = varargin(i);
        varargin(i) = [];
        nargin = nargin - 1;
        break;
      else
        print_usage ("plotmatrix");
      endif
    endif
  endfor

  if (nargin == 2)
    X = varargin{1};
    Y = X;
    have_hist = true;
  elseif (nargin == 3)
    X = varargin{1};
    Y = varargin{2};
  else
    print_usage ("plotmatrix");
  endif

  if (rows (X) != rows (Y))
    error ("plotmatrix: dimension mismatch in the arguments");
  endif

  [dummy, m] = size (X);
  [dummy, n] = size (Y);

  h = [];
  ax = [];
  p = [];
  pax = [];

  xsize = 0.9 / m;
  ysize = 0.9 / n;
  xoff = 0.05;
  yoff = 0.05;
  border = [0.130, 0.110, 0.225, 0.185] .* [xsize, ysize, xsize, ysize];
  border(3:4) = - border(3:4) - border(1:2);

  for i = 1 : m
    for j = 1 : n
      pos = [xsize * (i - 1) + xoff, ysize * (n - j) + yoff, xsize, ysize];
      tmp = axes ("outerposition", pos, "position", pos + border,
                  "parent", parent);
      if (i == j && have_hist)
        pax = [pax ; tmp];
        [nn, xx] = hist (X(:, i));
        tmp = bar (xx, nn, 1.0);
        p = [p; tmp];
      else
        ax = [ax ; tmp];
        if (have_line_spec)
          tmp = plot (X (:, i), Y (:, j), linespec);
        else
          tmp = plot (X (:, i), Y (:, j), ".");
        endif
        h = [h ; tmp];
      endif
    endfor
  endfor
endfunction

