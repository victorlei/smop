## Copyright (C) 1995-2015 John W. Eaton
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
## @deftypefn  {Function File} {} subplot (@var{rows}, @var{cols}, @var{index})
## @deftypefnx {Function File} {} subplot (@var{rcn})
## @deftypefnx {Function File} {} subplot (@var{hax})
## @deftypefnx {Function File} {} subplot (@dots{}, "align")
## @deftypefnx {Function File} {} subplot (@dots{}, "replace")
## @deftypefnx {Function File} {} subplot (@dots{}, "position", @var{pos})
## @deftypefnx {Function File} {} subplot (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {@var{hax} =} subplot (@dots{})
## Set up a plot grid with @var{rows} by @var{cols} subwindows and set the
## current axes for plotting (@code{gca}) to the location given by @var{index}.
##
## If only one numeric argument is supplied, then it must be a three digit
## value specifying the number of rows in digit 1, the number of columns in
## digit 2, and the plot index in digit 3.
##
## The plot index runs row-wise; First, all columns in a row are numbered
## and then the next row is filled.
##
## For example, a plot with 2x3 grid will have plot indices running as follows:
## @tex
## \vskip 10pt
## \hfil\vbox{\offinterlineskip\hrule
## \halign{\vrule#&&\qquad\hfil#\hfil\qquad\vrule\cr
## height13pt&1&2&3\cr height12pt&&&\cr\noalign{\hrule}
## height13pt&4&5&6\cr height12pt&&&\cr\noalign{\hrule}}}
## \hfil
## \vskip 10pt
## @end tex
## @ifnottex
##
## @example
## @group
## +-----+-----+-----+
## |  1  |  2  |  3  |
## +-----+-----+-----+
## |  4  |  5  |  6  |
## +-----+-----+-----+
## @end group
## @end example
##
## @end ifnottex
##
## @var{index} may also be a vector.  In this case, the new axis will enclose
## the grid locations specified.  The first demo illustrates this:
##
## @example
## demo ("subplot", 1)
## @end example
##
## The index of the subplot to make active may also be specified by its axes
## handle, @var{hax}, returned from a previous @code{subplot} command.
##
## If the option @qcode{"align"} is given then the plot boxes of the subwindows
## will align, but this may leave no room for axis tick marks or labels.
##
## If the option @qcode{"replace"} is given then the subplot axis will be
## reset, rather than just switching the current axis for plotting to the
## requested subplot.
##
## The @qcode{"position"} property can be used to exactly position the subplot
## axes within the current figure.  The option @var{pos} is a 4-element vector
## [x, y, width, height] that determines the location and size of the axes.
## The values in @var{pos} are normalized in the range [0,1].
##
## Any property/value pairs are passed directly to the underlying axes object.
##
## If the output @var{hax} is requested, subplot returns the axis handle for
## the subplot.  This is useful for modifying the properties of a subplot
## using @code{set}.
## @seealso{axes, plot, gca, set}
## @end deftypefn

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Adapted-By: jwe

function h = subplot (varargin)

  align_axes = false;
  replace_axes = false;
  have_position = false;
  initial_args_decoded = false;

  if (nargin >= 3)
    ## R, C, N?
    arg1 = varargin{1};
    arg2 = varargin{2};
    arg3 = varargin{3};
    if (   isnumeric (arg1) && isscalar (arg1)
        && isnumeric (arg2) && isscalar (arg2)
        && isnumeric (arg3))
      rows = arg1;
      cols = arg2;
      index = arg3;
      varargin(1:3) = [];
      initial_args_decoded = true;
    endif
  endif

  if (! initial_args_decoded && nargin > 1)
    ## check for "position", pos, ...
    if (strcmpi (varargin{1}, "position"))
      arg = varargin{2};
      if (isnumeric (arg) && numel (arg) == 4)
        pos = arg;
        varargin(1:2) = [];
        have_position = true;
        initial_args_decoded = true;
      else
        error ("subplot: POSITION must be a 4-element numeric array");
      endif
    endif
  endif

  if (! initial_args_decoded && nargin > 0)
    arg = varargin{1};
    if (nargin == 1 && isaxes (arg))
      ## Axes handle
      axes (arg);
      cf = get (0, "currentfigure");
      set (cf, "nextplot", "add");
      return;
    elseif (isscalar (arg) && arg >= 0)
      ## RCN?
      index = rem (arg, 10);
      arg = (arg - index) / 10;
      cols = rem (arg, 10);
      arg = (arg - cols) / 10;
      rows = rem (arg, 10);
      varargin(1) = [];
      initial_args_decoded = true;
    else
      error ("subplot: expecting axes handle or RCN argument");
    endif
  endif

  if (! initial_args_decoded)
    print_usage ();
  endif

  if (! have_position)
    cols = round (cols);
    rows = round (rows);
    index = round (index);

    if (any (index < 1) || any (index > rows*cols))
      error ("subplot: INDEX value must be >= 1 and <= ROWS*COLS");
    endif

    if (rows < 1 || cols < 1 || index < 1)
      error ("subplot: ROWS, COLS, and INDEX must be be positive");
    endif
  endif

  ## Process "align" and "replace" options
  idx = strcmpi (varargin, "align");
  if (any (idx))
    align_axes = true;
    varargin(idx) = [];
  endif

  idx = strcmpi (varargin, "replace");
  if (any (idx))
    replace_axes = true;
    varargin(idx) = [];
  endif

  axesunits = get (0, "defaultaxesunits");
  cf = gcf ();
  figureunits = get (cf, "units");
  unwind_protect
    set (0, "defaultaxesunits", "normalized");
    set (cf, "units", "pixels");

    ## FIXME: At the moment we force gnuplot to use the aligned mode
    ##        which will set "activepositionproperty" to "position".
    ##        This can yield to text overlap between labels and titles.
    ##        See bug #31610.
    if (strcmp (get (cf, "__graphics_toolkit__"), "gnuplot"))
      align_axes = true;
    endif

    if (! have_position)
      ## Subplots that cover more that one base subplot are not updated
      align_axes = (align_axes || (! isscalar (index)));
      ## Normal case where subplot indices have been given
      [pos, opos, li] = subplot_position (cf, rows, cols, index);
    else
      ## Position is specified by the user.
      li = zeros (1,4);
      align_axes = true;
    endif

    set (cf, "nextplot", "add");

    found = false;
    kids = get (cf, "children");
    for child = kids(:)'
      ## Check whether this child is still valid; this might not be the
      ## case anymore due to the deletion of previous children (due to
      ## "deletefcn" callback or for legends/colorbars that are deleted
      ## with their corresponding axes).
      if (! ishandle (child))
        continue;
      endif
      if (strcmp (get (child, "type"), "axes"))
        ## Skip legend and colorbar objects.
        if (any (strcmp (get (child, "tag"), {"legend", "colorbar"})))
          continue;
        endif

        if (isappdata (child, "__subplotposition__"))
          objpos = getappdata (child, "__subplotposition__");
        else
          objpos = get (child, "position");
        endif
        if (all (abs (objpos - pos) < eps) && ! replace_axes)
          ## If the new axes are in exactly the same position
          ## as an existing axes object, or if they share the same
          ## appdata "__subplotposition__", use the existing axes.
          found = true;
          hsubplot = child;
        else
          ## If the new axes overlap an old axes object, delete the old axes.
          objpos = get (child, "position");

          x0 = pos(1);
          x1 = x0 + pos(3);
          y0 = pos(2);
          y1 = y0 + pos(4);
          objx0 = objpos(1);
          objx1 = objx0 + objpos(3);
          objy0 = objpos(2);
          objy1 = objy0 + objpos(4);
          if (! (x0 >= objx1 || x1 <= objx0 || y0 >= objy1 || y1 <= objy0))
            delete (child);
          endif
        endif
      endif
    endfor

    if (found)
      ## Switch to existing subplot and set requested properties
      set (cf, "currentaxes", hsubplot);
      if (! isempty (varargin))
        set (hsubplot, varargin{:});
      endif
    else
      hsubplot = axes ("box", "off",
                       "activepositionproperty", "position",
                       "position", pos, "looseinset", li,
                       varargin{:});

      if (! align_axes)
        ## base position (no ticks, no annotation, no cumbersome neighbor)
        setappdata (hsubplot, "__subplotposition__", pos);
        ## max outerposition
        setappdata (hsubplot, "__subplotouterposition__", opos);
        addlistener (hsubplot, "outerposition", @subplot_align);
        addlistener (hsubplot, "xaxislocation", @subplot_align);
        addlistener (hsubplot, "yaxislocation", @subplot_align);
        addlistener (hsubplot, "position", {@subplot_align, true});
        subplot_align (hsubplot);
      endif

    endif
  unwind_protect_cleanup
    set (0, "defaultaxesunits", axesunits);
    set (cf, "units", figureunits);
  end_unwind_protect

  if (nargout > 0)
    h = hsubplot;
  endif

endfunction

function [pos, opos, li] = subplot_position (hf, nrows, ncols, idx)

  if (nrows == 1 && ncols == 1)
    ## Trivial result for subplot (1,1,1)
    pos = get (0, "defaultaxesposition");
    opos = get (0, "defaultaxesouterposition");
    li = get (0, "defaultaxeslooseinset");
    return;
  endif

  ## Row/Column inside the axes array
  row = ceil (idx / ncols);
  col = idx .- (row - 1) * ncols;
  row = [min(row) max(row)];
  col = [min(col) max(col)];

  ## Minimal margins around subplots defined in points
  fig_units = get (hf, "units");
  set (hf, "units", "points");
  pts_size = get (gcf, "position")(3:4);
  xbasemargin = 6 / pts_size(1);
  ybasemargin = 6 / pts_size(2);

  ## Column/row separation
  margin.column = .2 / ncols + 2 * xbasemargin;
  margin.row = .2 / nrows + 2 * ybasemargin;

  set (hf, "units", fig_units);
  margin.left = xbasemargin;
  margin.right = xbasemargin;
  margin.bottom = ybasemargin;
  margin.top = ybasemargin;

  ## Boundary axes have default margins
  borders = get (0, "defaultaxesposition");
  if (col(1) == 1)
    margin.left = borders(1);
  else
    margin.left = margin.column - margin.right;
  endif
  if (col(2) == ncols)
    margin.right = 1 - borders(1) - borders(3);
  endif


  if (row(2) == nrows)
    margin.bottom = borders(2);
  else
    margin.bottom = margin.row - margin.top;
  endif
  if (row(1) == 1)
    margin.top = 1 - borders(2) - borders(4);
  endif


  ## Compute base width and height
  width = (borders(3) - (ncols - 1) * margin.column) / ncols;
  height = (borders(4) - (nrows - 1) * margin.row) /nrows;

  ## Position, outerposition and looseinset
  x0 = borders(1) + (col(1) - 1) * (width + margin.column);
  y0 = borders(2) + (nrows - row(2)) * (height + margin.row);
  width += diff (col) * (width + margin.column);
  height += diff (row) * (height + margin.row);

  pos = [x0 y0 width height];
  opos = [(x0 - margin.left), (y0 - margin.bottom), ...
          (width + margin.left + margin.right), ...
          (height + margin.bottom + margin.top)];
  li = [margin.left, margin.bottom, margin.right, margin.top];

endfunction

function subplot_align (h, d, rmupdate = false)
  persistent updating = false;
  if (! updating)
    if (rmupdate)
      ## The "position" property has been changed from outside this
      ## routine. Don't update anymore.
      if (isappdata (h, "__subplotposition__"))
        rmappdata (h, "__subplotposition__");
        rmappdata (h, "__subplotouterposition__");
      endif
      return
    endif

    unwind_protect
      updating = true;
      hf = ancestor (h, "figure");
      children = get (hf, "children");

      ## Base position of the subplot
      pos = getappdata (children, "__subplotposition__");

      if (iscell (pos))
        do_align = ! cellfun (@isempty, pos);
        pos = cell2mat (pos(do_align));
      else
        return
      endif
      hsubplots = children(do_align);


      ## There may be mixed subplot series (e.g. 2-by-6 and 1-by-6) in
      ## the same figure. Only subplots that have the same width and
      ## height as this one are updated.
      if (any (h == hsubplots))
        width = pos(h == hsubplots, 3);
        height = pos(h == hsubplots, 4);
        do_align = (pos(:,3) == width) & (pos(:,4) == height);
        hsubplots(! do_align) = [];
        pos(! do_align,:) = [];
      else
        return
      endif

      ## Reset outerpositions to their default value
      opos = getappdata (hsubplots, "__subplotouterposition__");
      if (iscell (opos))
        opos = cell2mat (opos);
      endif
      for ii = 1:numel (hsubplots);
        set (hsubplots(ii), "outerposition", opos(ii,:), ...
             "activepositionproperty", "position");
      endfor

      ## Compare current positions to default and compute the new ones
      curpos = get (hsubplots, "position");
      if (iscell (curpos))
        curpos = cell2mat (curpos);
      endif
      dx0 = max (curpos(:,1) - pos(:,1));
      dx0(dx0<0) = 0;
      dx1 = max ((pos(:,1) + pos(:,3)) - (curpos(:,1) + curpos(:,3)));
      dx1(dx1<0) = 0;
      dy0 = max (curpos(:,2) - pos(:,2));
      dy0(dy0<0) = 0;
      dy1 = max ((pos(:,2) + pos(:,4)) - (curpos(:,2) + curpos(:,4)));
      dy1(dy1<0) = 0;

      pos(:,1) += dx0;
      pos(:,2) += dy0;
      pos(:,3) -= dx0 + dx1;
      pos(:,4) -= dy0 + dy1;

      for ii = 1:numel (hsubplots)
        set (hsubplots(ii), "position", pos(ii,:));
      endfor

    unwind_protect_cleanup
      updating = false;
    end_unwind_protect
  endif

endfunction


%!demo
%! clf;
%! r = 3;
%! c = 3;
%! fmt = {'horizontalalignment', 'center', 'verticalalignment', 'middle'};
%! for n = 1 : r*c
%!   subplot (r, c, n);
%!   xlabel (sprintf ('xlabel #%d', n));
%!   ylabel (sprintf ('ylabel #%d', n));
%!   title (sprintf ('title #%d', n));
%!   text (0.5, 0.5, sprintf ('subplot(%d,%d,%d)', r, c, n), fmt{:});
%!   axis ([0 1 0 1]);
%! end
%! subplot (r, c, 1:3);
%! xlabel (sprintf ('xlabel #%d:%d', 1, 3));
%! ylabel (sprintf ('ylabel #%d:%d', 1, 3));
%! title (sprintf ('title #%d:%d', 1, 3));
%! text (0.5, 0.5, sprintf ('subplot(%d,%d,%d:%d)', r, c, 1, 3), fmt{:});
%! axis ([0 1 0 1]);

%!demo
%! clf;
%! x = 0:1;
%! for n = 1:4
%!   subplot (2,2,n, 'align');
%!   plot (x, x);
%!   xlabel (sprintf ('xlabel (2,2,%d)', n));
%!   ylabel (sprintf ('ylabel (2,2,%d)', n));
%!   title (sprintf ('title (2,2,%d)', n));
%! end
%! subplot (1,2,1, 'align');
%! plot (x, x);
%! xlabel ('xlabel (1,2,1)');
%! ylabel ('ylabel (1,2,1)');
%! title ('title (1,2,1)');

%!demo
%! clf;
%! x = 0:10;
%! ax(1) = subplot (221);
%! set (ax(1), 'tag', '1');
%! plot (x, rand (3, 11))
%! title ('x & y labels & ticklabels');
%! xlabel xlabel
%! ylabel ylabel
%! ax(2) = subplot (222);
%! set (ax(2), 'tag', '2');
%! plot (x, rand (3, 11))
%! title ('no labels');
%! axis ('nolabel','tic')
%! ax(3) = subplot (223);
%! set (ax(3), 'tag', '3');
%! plot (x, rand (3, 11))
%! title ('no labels');
%! axis ('nolabel','tic')
%! ax(4) = subplot (224);
%! set (ax(4), 'tag', '4');
%! plot (x, rand (3, 11))
%! title ('x & y labels & ticklabels');
%! xlabel xlabel
%! ylabel ylabel

%!demo
%! x = 0:10;
%! subplot (221);
%! plot (x, rand (3, 11))
%! ylim ([0, 1]);
%! text (0.5, 0.5, '{x,y}labels & {x,y}ticklabels', ...
%!       'horizontalalignment', 'center', ...
%!       'units', 'normalized');
%! xlabel xlabel
%! ylabel ylabel
%! title title
%! subplot (222);
%! plot (x, rand (3, 11))
%! axis ('labely');
%! ylabel ylabel
%! text (0.5, 0.5, 'no xlabels, xticklabels', ...
%!       'horizontalalignment', 'center', ...
%!       'units', 'normalized');
%! subplot (223);
%! plot (x, rand (3, 11))
%! axis ('labelx');
%! text (0.5, 0.5, 'no ylabels, yticklabels', ...
%!       'horizontalalignment', 'center', ...
%!       'units', 'normalized');
%! xlabel xlabel
%! title title
%! subplot (224);
%! plot (x, rand (3, 11))
%! axis ('nolabel','tic');
%! text (0.5, 0.5, 'no {x,y}labels, {x,y}ticklabels', ...
%!       'horizontalalignment', 'center', ...
%!       'units', 'normalized');

## Test recognition/deletion of previous axes
## Default mode
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   for ii = 1:9
%!     hax(ii) = subplot (3,3,ii);
%!   endfor
%!   subplot (3,3,1);
%!   assert (gca (), hax(1));
%!   subplot (2,1,1);
%!   assert (ishandle (hax),[false(1,6), true(1,3)]);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

## Position mode
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = subplot ("position", [0.1 0.1 0.3 0.3]);
%!   h2 = subplot ("position", [0.5 0.5 0.3 0.3]);
%!   subplot ("position", [0.1 0.1 0.3 0.3]);
%!   assert (gca (), h1);
%!   subplot ("position", [0.5 0.5 0.3 0.3]);
%!   assert (gca (), h2);
%!   subplot ("position", [0.5 0.5 0.3 0.2]);
%!   assert (! ishandle (h2));
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

## Align mode
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = subplot (3,5,1, "align");
%!   h2 = subplot (3,5,2, "align");
%!   subplot (3,5,1, "align");
%!   assert (gca (), h1);
%!   subplot (3,2,1, "align");
%!   assert (! ishandle (h1));
%!   assert (! ishandle (h2));
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect
