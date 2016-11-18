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
## @deftypefn {Function File} {@var{h} =} __stem__ (@var{have_z}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

## Author: Michel D. Schmid <michaelschmid@users.sourceforge.net>
## Adapted-by: jwe

function h = __stem__ (have_z, varargin)

  if (have_z)
    caller = "stem3";
  else
    caller = "stem";
  endif

  [hax, varargin, nargin] = __plt_get_axis_arg__ (caller, varargin{:});

  [x, y, z, dofill, llc, ls, mmc, ms, varargin] = ...
                                           check_stem_arg (have_z, varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    hold_state = get (hax, "nextplot");
    set (hax, "nextplot", "add");

    h = [];
    nx = rows (x);
    h_baseline = [];

    for i = 1 : columns (x)
      if (have_z)
        xt = x(:)';
        xt = [xt; xt; NaN(1, nx)](:);
        yt = y(:)';
        yt = [yt; yt; NaN(1, nx)](:);
        zt = z(:)';
        zt = [zeros(1, nx); zt; NaN(1, nx)](:);
      else
        xt = x(:, i)';
        xt = [xt; xt; NaN(1, nx)](:);
        yt = y(:, i)';
        yt = [zeros(1, nx); yt; NaN(1, nx)](:);
      endif

      if (isempty (llc))
        lc = __next_line_color__ ();
      else
        lc = llc;
      endif

      if (isempty (mmc))
        mc = lc;
      else
        mc = mmc;
      endif

      if (dofill)
        fc = mc;
      else
        fc = "none";
      endif

      ## Must occur after __next_line_color__ in order to work correctly.
      hg = hggroup ();
      h = [h; hg];
      args = __add_datasource__ (caller, hg, {"x", "y", "z"}, varargin{:});

      if (have_z)
        __line__ (hax, xt, yt, zt, "color", lc, "linestyle", ls, "parent", hg);
        __line__ (hax, x, y, z, "color", mc, "linestyle", "none",
                       "marker", ms, "markerfacecolor", fc, "parent", hg);
      else
        __line__ (hax, xt, yt, "color", lc, "linestyle", ls, "parent", hg);
        __line__ (hax, x(:,i), y(:, i), "color", mc, "linestyle", "none",
                       "marker", ms, "markerfacecolor", fc, "parent", hg);

        x_axis_range = get (hax, "xlim");
        if (isempty (h_baseline))
          h_baseline = line (hax, x_axis_range, [0, 0], "color", [0, 0, 0]);
          set (h_baseline, "handlevisibility", "off", "xliminclude", "off");
          addproperty ("basevalue", h_baseline, "data", 0);
        else
          set (h_baseline, "xdata", x_axis_range);
        endif
      endif

      ## Setup the hggroup and listeners.
      addproperty ("showbaseline", hg, "radio", "{on}|off");
      addproperty ("baseline", hg, "data", h_baseline);
      addproperty ("basevalue", hg, "data", 0);

      addproperty ("color", hg, "linecolor", lc);
      addproperty ("linestyle", hg, "linelinestyle", ls);
      addproperty ("linewidth", hg, "linelinewidth", 0.5);
      addproperty ("marker", hg, "linemarker", ms);
      addproperty ("markeredgecolor", hg, "linemarkerfacecolor", mc);
      addproperty ("markerfacecolor", hg, "linemarkerfacecolor", fc);
      addproperty ("markersize", hg, "linemarkersize", 6);

      addlistener (hg, "color", @update_props);
      addlistener (hg, "linestyle", @update_props);
      addlistener (hg, "linewidth", @update_props);
      addlistener (hg, "marker", @update_props);
      addlistener (hg, "markeredgecolor", @update_props);
      addlistener (hg, "markerfacecolor", @update_props);
      addlistener (hg, "markersize", @update_props);

      if (islogical (x))
        x = double (x);
      endif
      addproperty ("xdata", hg, "data", x(:, i));
      if (islogical (y))
        y = double (y);
      endif
      addproperty ("ydata", hg, "data", y(:, i));
      if (have_z)
        addproperty ("zdata", hg, "data", z(:, i));
      else
        addproperty ("zdata", hg, "data", []);
      endif

      addlistener (hg, "xdata", @update_data);
      addlistener (hg, "ydata", @update_data);
      addlistener (hg, "zdata", @update_data);

      ## Matlab property, although Octave does not implement it.
      addproperty ("hittestarea", hg, "radio", "on|{off}", "off");

    endfor

    ## baseline listeners
    if (! isempty (h_baseline))
      addlistener (hax, "xlim", @update_xlim);
      for hg = h'
        addlistener (hg, "showbaseline", @show_baseline);
        addlistener (hg, "visible", {@show_baseline, h});
        addlistener (hg, "basevalue", @move_baseline);
      endfor

      addlistener (h_baseline, "basevalue", {@update_baseline, 0});
      addlistener (h_baseline, "ydata", {@update_baseline, 1});
      addlistener (h_baseline, "visible", {@update_baseline, 2});
      set (h_baseline, "parent", get (hg(1), "parent"));
    endif

    ## property/value pairs
    if (! isempty (args))
        set (h, args{:});
    endif

    if (! strcmp (hold_state, "add") && have_z)
      set (hax, "view", [-37.5 30],
                "xgrid", "on", "ygrid", "on", "zgrid", "on");
    endif
    set (hax, "nextplot", hold_state);

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

endfunction

function [x, y, z, dofill, lc, ls, mc, ms, args] = check_stem_arg (have_z, varargin)

  if (have_z)
    caller = "stem3";
  else
    caller = "stem";
  endif
  nargin = nargin - 1;  # account for have_z argument

  num_numeric = find (cellfun ("isclass", varargin, "char"), 1) - 1;
  if (isempty (num_numeric))
    num_numeric = nargin;
  endif

  if (num_numeric < 1 || num_numeric > 3)
    print_usage (caller);
  endif

  x = y = z = [];
  if (num_numeric == 1)
    if (have_z)
      z = varargin{1};
    else
      y = varargin{1};
    endif
  elseif (num_numeric == 2)
    if (have_z)
      error ("stem3: must define X, Y, and Z");
    else
      x = varargin{1};
      y = varargin{2};
    endif
  else  # nun_numeric == 3
    if (have_z)
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
    else
      error ("stem: can not define Z for 2-D stem plot");
    endif
  endif

  ## Validate numeric data
  if (have_z)
    if (isempty (x))
      [nr, nc] = size (z);
      if (nr >= nc)
        x = repmat ([1:nc], nr, 1);
        y = repmat ([1:nr]', 1, nc);
      else
        x = repmat ([1:nc], nr, 1);
        y = repmat ([1:nr]', 1, nc);
      endif
    endif
    if (! (isnumeric (x) || islogical (x))
        || ! (isnumeric (y) || islogical (y))
        || ! (isnumeric (z) || islogical (z)))
      error ("stem3: X, Y, and Z must be numeric");
    endif
  else
    if (isempty (x))
      if (isvector (y))
        x = 1:length (y);
      elseif (ismatrix (y))
        x = 1:rows (y);
      else
        error ("stem: Y must be a vector or 2-D array");
      endif
    endif
    if (! (isnumeric (x) || islogical (x))
        || ! (isnumeric (y) || islogical (y)))
      error ("stem: X and Y must be numeric");
    endif
  endif

  ## Check sizes of x, y and z.
  if (have_z)
    if (! size_equal (x, y, z))
      error ("stem3: inconsistent sizes for X, Y, and Z");
    endif
    x = x(:);
    y = y(:);
    z = z(:);
  else
    if (isvector (x))
      x = x(:);
      if (isvector (y))
        if (length (x) != length (y))
          error ("stem: inconsistent sizes for X and Y");
        endif
        y = y(:);
      else
        if (length (x) == rows (y))
          x = repmat (x(:), 1, columns (y));
        else
          error ("stem: inconsistent sizes for X and Y");
        endif
      endif
    elseif (! size_equal (x, y))
      error ("stem: inconsistent sizes for X and Y");
    endif
  endif

  dofill = false;
  have_line_spec = false;
  ## set specifiers to default values.
  [lc, ls, mc, ms] = set_default_values ();

  args = {};
  ioff = num_numeric + 1;
  while (ioff <= nargin)
    arg = varargin{ioff++};
    if (ischar (arg) && any (strcmpi (arg, {"fill", "filled"})))
      dofill = true;
    elseif ((ischar (arg) || iscellstr (arg)) && ! have_line_spec)
      [linespec, valid] = __pltopt__ (caller, arg, false);
      if (valid)
        have_line_spec = true;
        [lc, ls, mc, ms] = stem_line_spec (linespec);
      else
        args{end+1} = arg;
        if (ioff <= nargin)
          args{end+1} = varargin{ioff++};
        else
          error ('%s: No value specified for property "%s"', caller, arg);
        endif
      endif
    else
      args{end+1} = arg;
      if (ioff <= nargin)
        args{end+1} = varargin{ioff++};
      else
        error ('%s: No value specified for property "%s"', caller, arg);
      endif
    endif
  endwhile

endfunction

function [lc, ls, mc, ms] = stem_line_spec (lspec)

  [lc, ls, mc, ms] = set_default_values ();

  if (! isempty (lspec.color))
    lc = mc = lspec.color;
  endif

  if (! isempty (lspec.linestyle) && ! strcmp (lspec.linestyle, "none"))
    ls = lspec.linestyle;
  endif

  if (! isempty (lspec.marker) && ! strcmp (lspec.marker, "none"))
    ms = lspec.marker;
  endif

endfunction

function [lc, ls, mc, ms] = set_default_values ()
  mc = [];
  lc = [];
  ls = "-";
  ms = "o";
endfunction

function update_xlim (h, ~)
  kids = get (h, "children");
  xlim = get (h, "xlim");

  for i = 1 : length (kids)
    obj = get (kids(i));
    if (strcmp (obj.type, "hggroup") && isfield (obj, "baseline"))
      if (any (get (obj.baseline, "xdata") != xlim))
        set (obj.baseline, "xdata", xlim);
      endif
    endif
  endfor
endfunction

function update_baseline (h, ~, src)
  visible = get (h, "visible");
  if (src == 0)
    basevalue = get (h, "basevalue");
  else
    basevalue = get (h, "ydata")(1);
  endif

  kids = get (get (h, "parent"), "children");
  for i = 1 : length (kids)
    obj = get (kids(i));
    if (strcmp (obj.type, "hggroup") && isfield (obj, "baseline")
        && obj.baseline == h)
      ## Avoid lots of unnecessary listener updates
      if (! strcmp (get (kids(i), "showbaseline"), visible))
        set (kids(i), "showbaseline", visible);
      endif
      if (get (kids(i), "basevalue") != basevalue)
        set (kids(i), "basevalue", basevalue);
      endif
    endif
  endfor
endfunction

function show_baseline (h, ~, hg = [])
  if (isempty (hg))
    set (get (h, "baseline"), "visible", get (h, "showbaseline"));
  else
    if (all (strcmp (get (hg, "visible"), "off")))
      set (get (h, "baseline"), "visible", "off");
    else
      set (get (h, "baseline"), "visible", "on");
    endif
  endif
endfunction

function move_baseline (h, ~)
  b0 = get (h, "basevalue");
  bl = get (h, "baseline");

  set (bl, "ydata", [b0, b0]);
  set (bl, "basevalue", b0);

  kids = get (h, "children");
  yt = get (h, "ydata")(:)';
  ny = length (yt);
  yt = [b0 * ones(1, ny); yt; NaN(1, ny)](:);
  set (kids(2), "ydata", yt);
endfunction

function update_props (h, ~)
  kids = get (h, "children");
  set (kids(2), "color", get (h, "color"),
                "linestyle", get (h, "linestyle"),
                "linewidth", get (h, "linewidth"));
  set (kids(1), "color", get (h, "markeredgecolor"),
                "marker", get (h, "marker"),
                "markerfacecolor", get (h, "markerfacecolor"),
                "markersize", get (h, "markersize"));
endfunction

function update_data (h, ~)
  x = get (h, "xdata");
  y = get (h, "ydata");
  z = get (h, "zdata");

  if (! isempty (z) && size_equal (x, y, z))
    sz = min ([size(x); size(y); size(z)]);
    x = x(1:sz(1),1:sz(2));
    y = y(1:sz(1),1:sz(2));
    z = z(1:sz(1),1:sz(2));
  elseif (numel (x) != numel (y));
    sz = min ([size(x); size(y)]);
    x = x(1:sz(1),1:sz(2));
    y = y(1:sz(1),1:sz(2));
  endif
  bl = get (h, "basevalue");
  nx = numel (x);
  x = x(:)';
  xt = [x; x; NaN(1, nx)](:);
  if (! isempty (z))
    y = y(:)';
    yt = [y; y; NaN(1, nx)](:);
    z = z(:)';
    zt = [bl * ones(1, nx); z; NaN(1, nx)](:);
  else
    y = y(:)';
    yt = [bl * ones(1, nx); y; NaN(1, nx)](:);
    zt = [];
  endif

  kids = get (h, "children");
  set (kids(2), "xdata", xt, "ydata", yt, "zdata", zt);
  set (kids(1), "xdata", x, "ydata", y, "zdata", z);
endfunction

