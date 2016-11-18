## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn  {Function File} {} axis ()
## @deftypefnx {Function File} {} axis ([@var{x}_lo @var{x}_hi])
## @deftypefnx {Function File} {} axis ([@var{x}_lo @var{x}_hi @var{y}_lo @var{y}_hi])
## @deftypefnx {Function File} {} axis ([@var{x}_lo @var{x}_hi @var{y}_lo @var{y}_hi @var{z}_lo @var{z}_hi])
## @deftypefnx {Function File} {} axis (@var{option})
## @deftypefnx {Function File} {} axis (@dots{}, @var{option})
## @deftypefnx {Function File} {} axis (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{limits} =} axis ()
## Set axis limits and appearance.
##
## The argument @var{limits} should be a 2-, 4-, or 6-element vector.  The
## first and second elements specify the lower and upper limits for the
## x-axis.  The third and fourth specify the limits for the y-axis, and the
## fifth and sixth specify the limits for the z-axis.  The special values
## -Inf and Inf may be used to indicate that the limit should automatically be
## computed based on the data in the axis.
##
## Without any arguments, @code{axis} turns autoscaling on.
##
## With one output argument, @code{@var{limits} = axis} returns the current
## axis limits.
##
## The vector argument specifying limits is optional, and additional string
## arguments may be used to specify various axis properties.  For example,
##
## @example
## axis ([1, 2, 3, 4], "square");
## @end example
##
## @noindent
## forces a square aspect ratio, and
##
## @example
## axis ("tic", "labely");
## @end example
##
## @noindent
## turns tic marks on for all axes and tic mark labels on for the y-axis only.
##
## @noindent
## The following options control the aspect ratio of the axes.
##
## @table @asis
## @item @qcode{"square"}
## Force a square aspect ratio.
##
## @item @qcode{"equal"}
## Force x distance to equal y-distance.
##
## @item @qcode{"normal"}
## Restore default aspect ratio.
## @end table
##
## @noindent
## The following options control the way axis limits are interpreted.
##
## @table @asis
## @item @qcode{"auto"}
## Set the specified axes to have nice limits around the data or all if no
## axes are specified.
##
## @item @qcode{"manual"}
## Fix the current axes limits.
##
## @item @qcode{"tight"}
## Fix axes to the limits of the data.
##
## @item @qcode{"image"}
## Equivalent to @qcode{"tight"} and @qcode{"equal"}.
## @end table
##
## @noindent
## The following options affect the appearance of tic marks.
##
## @table @asis
## @item @qcode{"on"}
## Turn tic marks and labels on for all axes.
##
## @item @qcode{"off"}
## Turn tic marks off for all axes.
##
## @item @qcode{"tic[xyz]"}
## Turn tic marks on for all axes, or turn them on for the specified axes and
## off for the remainder.
##
## @item @qcode{"label[xyz]"}
## Turn tic labels on for all axes, or turn them on for the specified axes
## and off for the remainder.
##
## @item @qcode{"nolabel"}
## Turn tic labels off for all axes.
## @end table
##
## Note, if there are no tic marks for an axis, there can be no labels.
##
## @noindent
## The following options affect the direction of increasing values on the axes.
##
## @table @asis
## @item @qcode{"ij"}
## Reverse y-axis, so lower values are nearer the top.
##
## @item @qcode{"xy"}
## Restore y-axis, so higher values are nearer the top.
## @end table
##
## If the first argument @var{hax} is an axes handle, then operate on this
## axes rather than the current axes returned by @code{gca}.
##
## @seealso{xlim, ylim, zlim, daspect, pbaspect, box, grid}
## @end deftypefn

## Author: jwe

function limits = axis (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("axis", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    if (isempty (hax))
      hax = gca ();
    endif
    if (nargin == 0)
      limits = __axis__ (hax, varargin{:});
    else
      __axis__ (hax, varargin{:});
    endif
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

endfunction

function limits = __axis__ (ca, ax, varargin)

  if (nargin == 1)
    if (nargout == 0)
      set (ca, "xlimmode", "auto", "ylimmode", "auto", "zlimmode", "auto");
    else
      xlim = get (ca, "xlim");
      ylim = get (ca, "ylim");
      view = get (ca, "view");
      if (view(2) == 90)
        limits = [xlim, ylim];
      else
        zlim = get (ca, "zlim");
        limits = [xlim, ylim, zlim];
      endif
    endif

  elseif (ischar (ax))
    len = length (ax);

    ## 'matrix mode' to reverse the y-axis
    if (strcmpi (ax, "ij"))
      set (ca, "ydir", "reverse");
    elseif (strcmpi (ax, "xy"))
      set (ca, "ydir", "normal");

      ## aspect ratio
    elseif (strcmpi (ax, "image"))
      __axis__ (ca, "equal");
      set (ca, "plotboxaspectratiomode", "auto");
      __do_tight_option__ (ca);
    elseif (strcmpi (ax, "square"))
      set (ca, "dataaspectratiomode", "auto",
               "plotboxaspectratio", [1, 1, 1]);
    elseif (strcmp (ax, "equal"))
      if (strcmp (get (ancestor (ca, "figure"), "__graphics_toolkit__"), "gnuplot"))
        ## FIXME: gnuplot applies the aspect ratio activepostionproperty.
        set (ca, "activepositionproperty", "position");
        ## The following line is a trick used to trigger the recalculation of
        ## aspect related magnitudes even if the aspect ratio is the same
        ## (useful with the x11 gnuplot terminal after a window resize)
        set (ca, "dataaspectratiomode", "auto");
      endif
      set (ca, "dataaspectratio", [1, 1, 1], "plotboxaspectratio", [5 4 4]);

    elseif (strcmpi (ax, "normal"))
      ## Set plotboxaspectratio to something obtuse so that switching
      ## back to "auto" will force a re-calculation.
      set (ca, "plotboxaspectratio", [3 2 1]);
      set (ca, "plotboxaspectratiomode", "auto",
               "dataaspectratiomode", "auto");

      ## axis limits
    elseif (len >= 4 && strcmpi (ax(1:4), "auto"))
      if (len > 4)
        if (any (ax == "x"))
          set (ca, "xlimmode", "auto");
        endif
        if (any (ax == "y"))
          set (ca, "ylimmode", "auto");
        endif
        if (any (ax == "z"))
          set (ca, "zlimmode", "auto");
        endif
      else
        set (ca, "xlimmode", "auto", "ylimmode", "auto", "zlimmode", "auto");
      endif
    elseif (strcmpi (ax, "manual"))
      ## fixes the axis limits, like axis(axis) should;
      set (ca, "xlimmode", "manual", "ylimmode", "manual", "zlimmode", "manual");
    elseif (strcmpi (ax, "tight"))
      ## sets the axis limits to the min and max of all data.
      __do_tight_option__ (ca);
      ## tic marks
    elseif (strcmpi (ax, "on") || strcmpi (ax, "tic"))
      set (ca, "xtickmode", "auto", "ytickmode", "auto", "ztickmode", "auto");
      if (strcmpi (ax, "on"))
        set (ca, "xticklabelmode", "auto", "yticklabelmode", "auto",
           "zticklabelmode", "auto");
      endif
      set (ca, "visible", "on");
    elseif (strcmpi (ax, "off"))
      set (ca, "xtick", [], "ytick", [], "ztick", []);
      set (ca, "visible", "off");
    elseif (len > 3 && strcmpi (ax(1:3), "tic"))
      if (any (ax == "x"))
        set (ca, "xtickmode", "auto");
      else
        set (ca, "xtick", []);
      endif
      if (any (ax == "y"))
        set (ca, "ytickmode", "auto");
      else
        set (ca, "ytick", []);
      endif
      if (any (ax == "z"))
        set (ca, "ztickmode", "auto");
      else
        set (ca, "ztick", []);
      endif
    elseif (strcmpi (ax, "label"))
      set (ca, "xticklabelmode", "auto", "yticklabelmode", "auto",
           "zticklabelmode", "auto");
    elseif (strcmpi (ax, "nolabel"))
      set (ca, "xticklabel", "", "yticklabel", "", "zticklabel", "");
    elseif (len > 5 && strcmpi (ax(1:5), "label"))
      if (any (ax == "x"))
        set (ca, "xticklabelmode", "auto");
      else
        set (ca, "xticklabel", "");
      endif
      if (any (ax == "y"))
        set (ca, "yticklabelmode", "auto");
      else
        set (ca, "yticklabel", "");
      endif
      if (any (ax == "z"))
        set (ca, "zticklabelmode", "auto");
      else
        set (ca, "zticklabel", "");
      endif

    else
      warning ("unknown axis option '%s'", ax);
    endif

  elseif (isvector (ax))

    len = length (ax);

    if (len != 2 && len != 4 && len != 6)
      error ("axis: expecting vector with 2, 4, or 6 elements");
    endif

    for i = 1:2:len
      if (ax(i) >= ax(i+1))
        error ("axis: limits(%d) must be less than limits(%d)", i, i+1);
      endif
    endfor

    if (len > 1)
      xlim (ca, ax(1:2));
    endif

    if (len > 3)
      ylim (ca, ax(3:4));
    endif

    if (len > 5)
      zlim (ca, ax(5:6));
    endif

  else
    error ("axis: expecting no args, or a vector with 2, 4, or 6 elements");
  endif

  if (! isempty (varargin))
    __axis__ (ca, varargin{:});
  endif

endfunction

function lims = __get_tight_lims__ (ca, ax)

  ## Get the limits for axis ("tight").
  ## AX should be one of "x", "y", or "z".
  kids = findobj (ca, "-property", strcat (ax, "data"));
  ## The data properties for hggroups mirror their children.
  ## Exclude the redundant hgroup values.
  hg_kids = findobj (kids, "type", "hggroup");
  kids = setdiff (kids, hg_kids);
  if (isempty (kids))
    ## Return the current limits.
    lims = get (ca, strcat (ax, "lim"));
  else
    data = get (kids, strcat (ax, "data"));
    types = get (kids, "type");

    scale = get (ca, strcat (ax, "scale"));
    if (! iscell (data))
      data = {data};
    endif

    ## Extend image data one pixel
    idx = strcmp (types, "image");
    if (any (idx) && (ax == "x" || ax == "y"))
      imdata = data(idx);
      px = arrayfun (@__image_pixel_size__, kids(idx), "uniformoutput", false);
      ipx = ifelse (ax == "x", 1, 2);
      imdata = cellfun (@(x,dx) [(min (x) - dx(ipx)), (max (x) + dx(ipx))],
                        imdata, px, "uniformoutput", false);
      data(idx) = imdata;
    endif

    if (strcmp (scale, "log"))
      tmp = data;
      data = cellfun (@(x) x(x>0), tmp, "uniformoutput", false);
      n = cellfun ("isempty", data);
      data(n) = cellfun (@(x) x(x<0), tmp(n), "uniformoutput", false);
    endif
    data = cellfun (@(x) x(isfinite (x)), data, "uniformoutput", false);
    data = data(! cellfun ("isempty", data));
    if (! isempty (data))
      ## Change data from cell array of various sizes to a single column vector
      data = cat (1, cellindexmat (data, ":"){:});
      lims = [min(data), max(data)];
    else
      lims = [0, 1];
    endif
  endif

endfunction

function __do_tight_option__ (ca)

  xlim = __get_tight_lims__ (ca, "x");
  if (all (xlim == 0))
    xlim = eps () * [-1 1];
  elseif (diff (xlim == 0))
    xlim = xlim .* (1 + eps () * [-1, 1]);
  endif
  ylim = __get_tight_lims__ (ca, "y");
  if (all (ylim == 0))
    ylim = eps () * [-1 1];
  elseif (diff (ylim == 0))
    ylim = ylim .* (1 + eps () * [-1, 1]);
  endif
  set (ca, "xlim", xlim, "ylim", ylim)
  nd = __calc_dimensions__ (ca);
  is3dview = (get (ca, "view")(2) != 90);
  if (nd > 2 && is3dview)
    zlim = __get_tight_lims__ (ca, "z");
    if (all (zlim == 0))
      zlim = eps () * [-1 1];
    elseif (diff (zlim == 0))
      zlim = zlim .* (1 + eps () * [-1, 1]);
    endif
    set (ca, "zlim", zlim);
  endif

endfunction


%!demo
%! clf;
%! t = 0:0.01:2*pi;
%! x = sin (t);
%!
%! subplot (221);
%!  plot (t, x);
%!  title ('normal plot');
%!
%! subplot (222);
%!  plot (t, x);
%!  title ('square plot');
%!  axis ('square');
%!
%! subplot (223);
%!  plot (t, x);
%!  title ('equal plot');
%!  axis ('equal');
%!
%! subplot (224);
%!  plot (t, x);
%!  title ('normal plot again');
%!  axis ('normal');

%!demo
%! clf;
%! t = 0:0.01:2*pi;
%! x = sin (t);
%!
%! subplot (121);
%!  plot (t, x);
%!  title ('ij plot');
%!  axis ('ij');
%!
%! subplot (122);
%!  plot (t, x);
%!  title ('xy plot');
%!  axis ('xy');

%!demo
%! clf;
%! t = 0:0.01:2*pi;
%! x = sin (t);
%!
%! subplot (331);
%!  plot (t, x);
%!  title ('x tics and labels');
%!  axis ('ticx');
%!
%! subplot (332);
%!  plot (t, x);
%!  title ('y tics and labels');
%!  axis ('ticy');
%!
%! subplot (333);
%!  plot (t, x);
%!  title ('axis off');
%!  axis ('off');
%!
%! subplot (334);
%!  plot (t, x);
%!  title ('x and y tics, x labels');
%!  axis ('labelx','tic');
%!
%! subplot (335);
%!  plot (t, x);
%!  title ('x and y tics, y labels');
%!  axis ('labely','tic');
%!
%! subplot (336);
%!  plot (t, x);
%!  title ('all tics but no labels');
%!  axis ('nolabel','tic');
%!
%! subplot (337);
%!  plot (t, x);
%!  title ('x tics, no labels');
%!  axis ('nolabel','ticx');
%!
%! subplot (338);
%!  plot (t, x);
%!  title ('y tics, no labels');
%!  axis ('nolabel','ticy');
%!
%! subplot (339);
%!  plot (t, x);
%!  title ('all tics and labels');
%!  axis ('on');

%!demo
%! clf;
%! t = 0:0.01:2*pi;
%! x = sin (t);
%!
%! subplot (321);
%!  plot (t, x);
%!  title ('axes at [0 3 0 1]');
%!  axis ([0,3,0,1]);
%!
%! subplot (322);
%!  plot (t, x);
%!  title ('auto');
%!  axis ('auto');
%!
%! subplot (323);
%!  plot (t, x, ';sine [0:2pi];'); hold on;
%!  plot (-3:3,-3:3, ';line (-3,-3)->(3,3);'); hold off;
%!  title ('manual');
%!  axis ('manual');
%!
%! subplot (324);
%!  plot (t, x, ';sine [0:2pi];');
%!  title ('axes at [0 3 0 1], then autox');
%!  axis ([0,3,0,1]);
%!  axis ('autox');
%!
%! subplot (325);
%!  plot (t, x, ';sine [0:2pi];');
%!  title ('axes at [3 6 0 1], then autoy');
%!  axis ([3,6,0,1]);
%!  axis ('autoy');
%!
%! subplot (326);
%!  plot (t, sin(t), t, -2*sin(t/2));
%!  axis ('tight');
%!  title ('tight');

%!demo
%! clf;
%! x = 0:0.1:10;
%! plot (x, sin(x));
%! axis image;
%! title ({'image', 'equivalent to "tight" & "equal"'});

%!demo
%! clf;
%! colormap ('default');
%! [x,y,z] = peaks (50);
%! x1 = max (x(:));
%! pcolor (x-x1, y-x1/2, z);
%! hold on;
%! [x,y,z] = sombrero ();
%! s = x1 / max (x(:));
%! pcolor (s*x+x1, s*y+x1/2, 5*z);
%! axis tight;

%!demo
%! clf;
%! x = -10:10;
%! plot (x,x, x,-x);
%! set (gca, 'yscale', 'log');
%! legend ({'x >= 1', 'x <= 1'}, 'location', 'north');
%! title ('ylim = [1, 10]');

%!demo
%! clf;
%! loglog (1:20, '-s');
%! axis tight;

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1 + abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! set (gca, 'xaxislocation', 'zero');
%! set (gca, 'yaxislocation', 'zero');
%! box off;
%! title ({'no plot box', 'xaxislocation = zero, yaxislocation = zero'});

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1+abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! set (gca, 'xaxislocation', 'zero');
%! set (gca, 'yaxislocation', 'left');
%! box off;
%! title ({'no plot box', 'xaxislocation = zero, yaxislocation = left'});

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1+abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! title ('no plot box');
%! set (gca, 'xaxislocation', 'zero');
%! set (gca, 'yaxislocation', 'right');
%! box off;
%! title ({'no plot box', 'xaxislocation = zero, yaxislocation = right'});

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1+abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! set (gca, 'xaxislocation', 'bottom');
%! set (gca, 'yaxislocation', 'zero');
%! box off;
%! title ({'no plot box', 'xaxislocation = bottom, yaxislocation = zero'});

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1+abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! set (gca, 'xaxislocation', 'top');
%! set (gca, 'yaxislocation', 'zero');
%! box off;
%! title ({'no plot box', 'xaxislocation = top, yaxislocation = zero'});

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (11:20, [21:24, NaN, -Inf, 27:30]);
%!   hold all;
%!   plot (11:20, 25.5 + rand (10));
%!   axis tight;
%!   assert (axis (), [11 20 21 30]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   a = logspace (-5, 1, 10);
%!   loglog (a, -a);
%!   axis tight;
%!   assert (axis (), [1e-5, 10, -10, -1e-5]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test 'axis tight' with differently oriented, differently numbered data vecs
## Bug #40036.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   Z = peaks (linspace (-3, 3, 49), linspace (-2, 2, 29));
%!   surf (Z);
%!   axis tight;
%!   assert (axis (), [1 49 1 29 min(Z(:)) max(Z(:))]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

