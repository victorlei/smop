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
## @deftypefn  {Function File} {} stairs (@var{y})
## @deftypefnx {Function File} {} stairs (@var{x}, @var{y})
## @deftypefnx {Function File} {} stairs (@dots{}, @var{style})
## @deftypefnx {Function File} {} stairs (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} stairs (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} stairs (@dots{})
## @deftypefnx {Function File} {[@var{xstep}, @var{ystep}] =} stairs (@dots{})
## Produce a stairstep plot.
##
## The arguments @var{x} and @var{y} may be vectors or matrices.
## If only one argument is given, it is taken as a vector of Y values
## and the X coordinates are taken to be the indices of the elements.
##
## The style to use for the plot can be defined with a line style @var{style}
## of the same format as the @code{plot} command.
##
## Multiple property/value pairs may be specified, but they must appear in
## pairs.
##
## If the first argument @var{hax} is an axis handle, then plot into this axis,
## rather than the current axis handle returned by @code{gca}.
##
## If one output argument is requested, return a graphics handle to the
## created plot.  If two output arguments are specified, the data are generated
## but not plotted.  For example,
##
## @example
## stairs (x, y);
## @end example
##
## @noindent
## and
##
## @example
## @group
## [xs, ys] = stairs (x, y);
## plot (xs, ys);
## @end group
## @end example
##
## @noindent
## are equivalent.
## @seealso{bar, hist, plot, stem}
## @end deftypefn

## Author: jwe

function [xs, ys] = stairs (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("stairs", varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  if (nargout < 2)
    oldfig = [];
    if (! isempty (hax))
      oldfig = get (0, "currentfigure");
    endif
    unwind_protect
      hax = newplot (hax);
      [htmp, xxs, yys] = __stairs__ (true, varargin{:});
    unwind_protect_cleanup
      if (! isempty (oldfig))
        set (0, "currentfigure", oldfig);
      endif
    end_unwind_protect
    if (nargout == 1)
      xs = htmp;
    endif
  else
    [~, xs, ys] = __stairs__ (false, varargin{:});
  endif

endfunction

function [h, xs, ys] = __stairs__ (doplot, varargin)

  if (nargin == 2 || ischar (varargin{2}))
    y = varargin{1};
    varargin(1) = [];
    if (! isnumeric (y) || ndims (y) > 2)
      error ("stairs: Y must be a numeric 2-D vector or matrix");
    endif
    if (isvector (y))
      y = y(:);
    endif
    x = 1:rows (y);
  else
    x = varargin{1};
    y = varargin{2};
    varargin(1:2) = [];
    if (! isnumeric (x) || ! isnumeric (y) || ndims (x) > 2 || ndims (y) > 2)
      error ("stairs: X and Y must be numeric 2-D vectors or matrices");
    endif
  endif

  vec_x = isvector (x);
  if (vec_x)
    x = x(:);
  endif

  if (isvector (y))
    y = y(:);
  elseif (isnumeric (y) && vec_x)
    x = repmat (x, [1, columns(y)]);
  endif

  if (! size_equal (x, y))
    error ("stairs: X and Y sizes must match");
  endif

  [nr, nc] = size (y);

  len = 2*nr - 1;

  xs = ys = zeros (len, nc);

  xs(1,:) = x(1,:);
  ys(1,:) = y(1,:);

  xtmp = x(2:nr,:);
  ridx = 2:2:len-1;
  xs(ridx,:) = xtmp;
  ys(ridx,:) = y(1:nr-1,:);

  ridx = 3:2:len;
  xs(ridx,:) = xtmp;
  ys(ridx,:) = y(2:nr,:);

  have_line_spec = false;
  for i = 1:2:numel (varargin)
    arg = varargin{i};
    if (ischar (arg) || iscellstr (arg))
      [linespec, valid] = __pltopt__ ("stairs", arg, false);
      if (valid)
        have_line_spec = true;
        varargin(i) = [];
        break;
      endif
    endif
  endfor

  if (doplot)
    h = [];
    hold_state = get (gca (), "nextplot");
    unwind_protect
      for i = 1 : columns (y)

        if (have_line_spec)
          lc = linespec.color;
          if (isempty (lc))
            lc = __next_line_color__ ();
          endif
          ls = linespec.linestyle;
          if (isempty (ls))
            ls = "-";
          endif
          mk = linespec.marker;
          if (isempty (mk))
            mk = "none";
          endif
        else
          lc = __next_line_color__ ();
          ls = "-";
          mk = "none";
        endif

        ## Must occur after __next_line_color__ in order to work correctly.
        hg = hggroup ();
        h = [h; hg];
        args = __add_datasource__ ("stairs", hg, {"x", "y"}, varargin{:});

        addproperty ("xdata", hg, "data", x(:,i).');
        addproperty ("ydata", hg, "data", y(:,i).');

        addlistener (hg, "xdata", @update_data);
        addlistener (hg, "ydata", @update_data);

        htmp = line (xs(:,i).', ys(:,i).', "color", lc, "linestyle", ls,
                                           "marker", mk, "parent", hg);

        addproperty ("color", hg, "linecolor", get (htmp, "color"));
        addproperty ("linestyle", hg, "linelinestyle", get (htmp, "linestyle"));
        addproperty ("linewidth", hg, "linelinewidth", get (htmp, "linewidth"));

        addproperty ("marker", hg, "linemarker", get (htmp, "marker"));
        addproperty ("markeredgecolor", hg, "linemarkeredgecolor",
                     get (htmp, "markeredgecolor"));
        addproperty ("markerfacecolor", hg, "linemarkerfacecolor",
                     get (htmp, "markerfacecolor"));
        addproperty ("markersize", hg, "linemarkersize",
                     get (htmp, "markersize"));

        addlistener (hg, "color", @update_props);
        addlistener (hg, "linestyle", @update_props);
        addlistener (hg, "linewidth", @update_props);
        addlistener (hg, "marker", @update_props);
        addlistener (hg, "markeredgecolor", @update_props);
        addlistener (hg, "markerfacecolor", @update_props);
        addlistener (hg, "markersize", @update_props);

        ## Matlab property, although Octave does not implement it.
        addproperty ("hittestarea", hg, "radio", "on|{off}", "off");

        if (! isempty (args))
          set (hg, args{:});
        endif
      endfor
    unwind_protect_cleanup
      set (gca (), "nextplot", hold_state);
    end_unwind_protect
  else
    h = 0;
  endif

endfunction

function update_props (h, ~)
  set (get (h, "children"),
       {"color", "linestyle", "linewidth", "marker", ...
        "markeredgecolor", "markerfacecolor", "markersize"},
       get (h, {"color", "linestyle", "linewidth", "marker", ...
                "markeredgecolor", "markerfacecolor", "markersize"}));
endfunction

function update_data (h, ~)
  x = get (h, "xdata");
  y = get (h, "ydata");

  sz = min ([size(x); size(y)]);
  x = x(1:sz(1), 1:sz(2));
  y = y(1:sz(1), 1:sz(2));

  nr = length (x);
  len = 2 * nr - 1;
  xs = ys = zeros (1, len);

  xs(1) = x(1);
  ys(1) = y(1);

  xtmp = x(2:nr);
  ridx = 2:2:len-1;
  xs(ridx) = xtmp;
  ys(ridx) = y(1:nr-1);

  ridx = 3:2:len;
  xs(ridx) = xtmp;
  ys(ridx) = y(2:nr);

  set (get (h, "children"), "xdata", xs, "ydata", ys);
endfunction


%!demo
%! clf;
%! rand_1x10_data1 = [0.073, 0.455, 0.837, 0.124, 0.426, 0.781, 0.004, 0.024, 0.519, 0.698];
%! y = rand_1x10_data1;
%! stairs (y);
%! title ('stairs() plot of y-data');

%!demo
%! clf;
%! x = 1:10;
%! rand_1x10_data2 = [0.014, 0.460, 0.622, 0.394, 0.531, 0.378, 0.466, 0.788, 0.342, 0.893];
%! y = rand_1x10_data2;
%! [xs, ys] = stairs (x, y);
%! plot (xs, ys);
%! title ('plot() of stairs() generated data');

%!demo
%! clf;
%! stairs (1:9, '-o');
%! title ('stairs() plot with linespec to modify marker');

%!demo
%! clf;
%! stairs (9:-1:1, 'marker', 's', 'markersize', 10, 'markerfacecolor', 'm');
%! title ('stairs() plot with prop/val pairs to modify appearance');

%!demo
%! clf;
%! N = 11;
%! x = 0:(N-1);
%! y = rand (1, N);
%! hs = stairs (x(1), y(1));
%! axis ([1, N-1 0, 1]);
%! title ('stairs plot data modified through handle');
%! for k = 2:N
%!   set (hs, 'xdata', x(1:k), 'ydata', y(1:k));
%!   drawnow ();
%!   pause (0.2);
%! end

## Invisible figure used for tests
%!shared hf, hax
%! hf = figure ("visible", "off");
%! hax = axes;

%!error stairs ()
%!error <Y must be a numeric 2-D vector> stairs (hax, {1})
%!error <Y must be a numeric 2-D vector> stairs (ones (2,2,2))
%!error <X and Y must be numeric 2-D vector> stairs ({1}, 1)
%!error <X and Y must be numeric 2-D vector> stairs (1, {1})
%!error <X and Y must be numeric 2-D vector> stairs (ones (2,2,2), 1)
%!error <X and Y must be numeric 2-D vector> stairs (1, ones (2,2,2))
%!error <X and Y sizes must match> stairs (1:2, 1:3)

## Close figure used for testing
%!test
%! close (hf);

