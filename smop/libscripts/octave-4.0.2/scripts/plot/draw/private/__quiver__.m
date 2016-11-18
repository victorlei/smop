## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn {Function File} {@var{hg} =} __quiver__ (@dots{})
## Undocumented internal function.
## @end deftypefn

function hg = __quiver__ (varargin)

  h = varargin{1};
  is3d = varargin{2};

  autoscale = 0.9;
  ## Matlab uses 0.2, but Octave's algorithm produces equivalent visual
  ## results if arrowsize=0.33.  Since this is just a non-dimensional
  ## scaling factor we scale the arrowsize property value by 0.33/0.20
  ## in order to get equivalent visual results while keeping equivalent
  ## property values.
  arrowsize = 0.20;

  firstnonnumeric = find (! cellfun ("isnumeric", varargin(3:nargin)), 1);
  if (isempty (firstnonnumeric))
    firstnonnumeric = Inf;
  else
    firstnonnumeric += 2;
  endif

  ioff = 3;
  if (nargin < (6 + is3d) || firstnonnumeric < (6 + is3d))
    u = varargin{ioff++};
    v = varargin{ioff++};
    if (is3d)
      w = varargin{ioff++};
      [x, y, z] = meshgrid (1:columns (u), 1:rows (u), 1:max (size (w)));
    else
      [x, y] = meshgrid (1:columns (u), 1:rows (u));
    endif
    if (nargin >= ioff && isnumeric (varargin{ioff})
        && isscalar (varargin{ioff}))
      autoscale = varargin{ioff++};
    endif
  else
    x = varargin{ioff++};
    y = varargin{ioff++};
    if (is3d)
      z = varargin{ioff++};
    endif
    u = varargin{ioff++};
    v = varargin{ioff++};
    if (is3d)
      w = varargin{ioff++};
      if (isvector (x) && isvector (y) && isvector (z)
          && (! isvector (u) || ! isvector (v) || ! isvector (w)))
        [x, y, z] = meshgrid (x, y, z);
      endif
    else
      if (isvector (x) && isvector (y) && (! isvector (u) || ! isvector (v)))
        [x, y] = meshgrid (x, y);
      endif
    endif
    if (nargin >= ioff && isnumeric (varargin{ioff})
        && isscalar (varargin{ioff}))
      autoscale = varargin{ioff++};
    endif
  endif

  have_filled = false;
  have_line_spec = false;
  args = {};
  while (ioff <= nargin)
    arg = varargin{ioff++};
    if (ischar (arg) && strcmpi (arg, "filled"))
      have_filled = true;
    elseif ((ischar (arg) || iscellstr (arg))
            && ! have_line_spec)
      [linespec, valid] = __pltopt__ ("quiver", arg, false);
      if (valid)
        have_line_spec = true;
        if (isempty (linespec.linestyle) || strcmp (linespec.linestyle, "none"))
          linespec.linestyle = "-";
        endif
      else
        args{end+1} = arg;
        if (ioff <= nargin)
          args{end+1} = varargin{ioff++};
        endif
      endif
    else
      args{end+1} = arg;
      if (ioff <= nargin)
        args{end+1} = varargin{ioff++};
      endif
    endif
  endwhile

  ## Normalize 0.20 to 1/3 for plotting
  arrowsize /= 0.20 * 3;

  ## Scale the arrows to fit in the grid
  uu = u;
  vv = v;
  if (is3d)
    ww = w;
    len = max (sqrt (u(:).^2 + v(:).^2 + w(:).^2));
  else
    len = max (sqrt (u(:).^2 + v(:).^2));
  endif
  if (len > 0 && autoscale && numel (u) > 1)
    if (isvector (x))
      nx = ny = sqrt (length (x));
    else
      [ny, nx] = size (x);  # assume meshgrid fmt, x in columns, y in rows
    endif
    dx = (max (x(:)) - min (x(:))) / nx;
    dy = (max (y(:)) - min (y(:))) / ny;
    if (is3d)
      dz = (max (z(:)) - min (z(:))) / max (nx, ny);
    else
      dz = 0;
    endif
    sd = sqrt (dx.^2 + dy.^2 + dz.^2) / len;
    if (sd != 0)
      s = autoscale * sd;
    else  # special case of identical points with multiple vectors
      s = autoscale;
    endif
    uu = s * u;
    vv = s * v;
    if (is3d)
      ww = s * w;
    endif
  endif

  hstate = get (h, "nextplot");
  unwind_protect

    if (have_line_spec)
      ls = linespec.linestyle;
      lc = linespec.color;
    else
      ls = "-";
      lc = __next_line_color__ ();
    endif

    ## Must occur after __next_line_color__ in order to work correctly.
    hg = hggroup ();
    if (is3d)
      args = __add_datasource__ ("quiver3", hg,
                                 {"x", "y", "z", "u", "v", "w"}, args{:});
    else
      args = __add_datasource__ ("quiver", hg,
                                 {"x", "y", "z", "u", "v", "w"}, args{:});
    endif
    hold on;

    addproperty ("xdata", hg, "data", x);
    addproperty ("ydata", hg, "data", y);

    addproperty ("udata", hg, "data", u);
    addproperty ("vdata", hg, "data", v);
    if (is3d)
      addproperty ("zdata", hg, "data", z);
      addproperty ("wdata", hg, "data", w);
    else
      addproperty ("zdata", hg, "data", []);
      addproperty ("wdata", hg, "data", []);
    endif

    addlistener (hg, "xdata", @update_data);
    addlistener (hg, "ydata", @update_data);
    addlistener (hg, "zdata", @update_data);
    addlistener (hg, "udata", @update_data);
    addlistener (hg, "vdata", @update_data);
    addlistener (hg, "wdata", @update_data);

    x = x(:);
    y = y(:);
    xend = x + uu(:);
    yend = y + vv(:);
    if (is3d)
      z = z(:);
      zend = z + ww(:);
    endif

    ## Draw arrow shaft as one line object
    if (is3d)
      h1 = plot3 ([x.'; xend.'; NaN(1, length (x))](:),
                  [y.'; yend.'; NaN(1, length (y))](:),
                  [z.'; zend.'; NaN(1, length (z))](:),
                  "linestyle", ls, "color", lc, "parent", hg);
    else
      h1 = plot ([x.'; xend.'; NaN(1, length (x))](:),
                 [y.'; yend.'; NaN(1, length (y))](:),
                 "linestyle", ls, "color", lc, "parent", hg);
    endif

    xtmp = x + uu(:) * (1 - arrowsize);
    ytmp = y + vv(:) * (1 - arrowsize);

    if (is3d)
      xydist = sqrt (uu(:).^2 + vv(:).^2 + ww(:).^2) ./ ...
                 (sqrt (uu(:).^2 + vv(:).^2) + eps);
      xarrw1 = xtmp + vv(:) .* xydist * arrowsize / 4;
      xarrw2 = xtmp - vv(:) .* xydist * arrowsize / 4;
      yarrw1 = ytmp - uu(:) .* xydist * arrowsize / 4;
      yarrw2 = ytmp + uu(:) .* xydist * arrowsize / 4;
      zarrw1 = zarrw2 = zend - ww(:) * arrowsize;
    else
      xarrw1 = xtmp + vv(:) * arrowsize / 3;
      xarrw2 = xtmp - vv(:) * arrowsize / 3;
      yarrw1 = ytmp - uu(:) * arrowsize / 3;
      yarrw2 = ytmp + uu(:) * arrowsize / 3;
    endif

    ## Draw arrowhead as one line object
    if (have_line_spec)
      if (! isempty (linespec.marker) && ! strcmp (linespec.marker, "none"))
        ls = "none";  # No arrowhead drawn when marker present
      endif
    endif

    if (is3d)
      h2 = plot3 ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
                  [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
                  [zarrw1.'; zend.'; zarrw2.'; NaN(1, length (z))](:),
                  "linestyle", ls, "color", lc, "parent", hg);
    else
      h2 = plot ([xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:),
                 [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:),
                  "linestyle", ls, "color", lc, "parent", hg);
    endif

    ## Draw arrow base marker as a third line object
    if (! have_line_spec || isempty (linespec.marker))
      mk = "none";
    else
      mk = linespec.marker;
    endif
    if (is3d)
      h3 = plot3 (x, y, z, "linestyle", "none", "marker", mk, "parent", hg);
    else
      h3 = plot (x, y, "linestyle", "none", "marker", mk, "parent", hg);
    endif
    if (have_filled)
      ## FIXME: gnuplot doesn't respect the markerfacecolor field
      set (h3, "markerfacecolor", get (h1, "color"));
    endif

    ## Set up the hggroup properties and listeners
    if (autoscale)
      addproperty ("autoscale", hg, "radio", "{on}|off", "on");
      addproperty ("autoscalefactor", hg, "data", autoscale);
    else
      addproperty ("autoscale", hg, "radio", "{on}|off", "off");
      addproperty ("autoscalefactor", hg, "data", 1.0);
    endif
    addlistener (hg, "autoscale", @update_data);
    addlistener (hg, "autoscalefactor", @update_data);

    addproperty ("maxheadsize", hg, "data", arrowsize * .20*3);
    addlistener (hg, "maxheadsize", @update_data);

    addproperty ("showarrowhead", hg, "radio", "{on}|off", "on");
    addlistener (hg, "showarrowhead", @update_props);

    addproperty ("color", hg, "linecolor", get (h1, "color"));
    addproperty ("linestyle", hg, "linelinestyle", get (h1, "linestyle"));
    addproperty ("linewidth", hg, "linelinewidth", get (h1, "linewidth"));
    addproperty ("marker", hg, "linemarker", get (h3, "marker"));
    addproperty ("markerfacecolor", hg, "linemarkerfacecolor",
                 get (h3, "markerfacecolor"));
    addproperty ("markersize", hg, "linemarkersize", get (h3, "markersize"));

    addlistener (hg, "color", @update_props);
    addlistener (hg, "linestyle", @update_props);
    addlistener (hg, "linewidth", @update_props);
    addlistener (hg, "marker", @update_props);
    addlistener (hg, "markerfacecolor", @update_props);
    addlistener (hg, "markersize", @update_props);

    ## Matlab property, although Octave does not implement it.
    addproperty ("hittestarea", hg, "radio", "on|{off}", "off");

    if (! isempty (args))
      set (hg, args{:});
    endif
  unwind_protect_cleanup
    set (h, "nextplot", hstate);
  end_unwind_protect

endfunction

function update_data (h, ~)

  x = get (h, "xdata");
  y = get (h, "ydata");
  z = get (h, "zdata");

  u = get (h, "udata");
  v = get (h, "vdata");
  w = get (h, "wdata");

  s = get (h, "autoscalefactor");
  arrowsize = get (h, "maxheadsize");
  arrowsize /= 0.20 * 3;

  kids = get (h, "children");

  if (isempty (z) || isempty (w))
    is3d = false;
  else
    is3d = true;
  endif

  if (strcmp (get (h, "autoscale"), "on") && s != 0)
    ## Scale the arrows to fit in the grid
    if (isvector (x))
      nx = ny = sqrt (length (x));
    else
      [ny, nx] = size (x);
    endif
    dx = (max (x(:)) - min (x(:))) / nx;
    dy = (max (y(:)) - min (y(:))) / ny;
    if (is3d)
      dz = (max (z(:)) - min (z(:))) / max (nx, ny);
      len = max (sqrt (u(:).^2 + v(:).^2 + w(:).^2));
    else
      dz = 0;
      len = max (sqrt (u(:).^2 + v(:).^2));
    endif
    if (len > 0)
      sd = sqrt (dx.^2 + dy.^2 + dz.^2) / len;
      if (sd != 0)
        s *= sd;
      endif
      u = s * u;
      v = s * v;
      if (is3d)
        w = s * w;
      endif
    endif
  endif

  x = x(:);
  y = y(:);
  xend = x + u(:);
  yend = y + v(:);
  if (is3d)
    z = z(:);
    zend = z + w(:);
  endif

  set (kids(3), "xdata", [x.'; xend.'; NaN(1, length (x))](:));
  set (kids(3), "ydata", [y.'; yend.'; NaN(1, length (y))](:));
  if (is3d)
    set (kids(3), "zdata", [z.'; zend.'; NaN(1, length (z))](:));
  endif

  xtmp = x + u(:) * (1 - arrowsize);
  ytmp = y + v(:) * (1 - arrowsize);

  if (is3d)
    xydist = sqrt (u(:).^2 + v(:).^2 + w(:).^2) ./ ...
               (sqrt (u(:).^2 + v(:).^2) + eps);
    xarrw1 = xtmp + v(:) .* xydist * arrowsize / 4;
    xarrw2 = xtmp - v(:) .* xydist * arrowsize / 4;
    yarrw1 = ytmp - u(:) .* xydist * arrowsize / 4;
    yarrw2 = ytmp + u(:) .* xydist * arrowsize / 4;
    zarrw1 = zarrw2 = zend - w(:) * arrowsize;
  else
    xarrw1 = xtmp + v(:) * arrowsize / 3;
    xarrw2 = xtmp - v(:) * arrowsize / 3;
    yarrw1 = ytmp - u(:) * arrowsize / 3;
    yarrw2 = ytmp + u(:) * arrowsize / 3;
  endif

  set (kids(2), "xdata", [x.'; xend.'; NaN(1, length (x))](:));
  set (kids(2), "ydata", [y.'; yend.'; NaN(1, length (y))](:));
  if (is3d)
    set (kids(2), "zdata", [z.'; zend.'; NaN(1, length (z))](:));
  endif

  set (kids(2), "xdata", [xarrw1.'; xend.'; xarrw2.'; NaN(1, length (x))](:));
  set (kids(2), "ydata", [yarrw1.'; yend.'; yarrw2.'; NaN(1, length (y))](:));
  if (is3d)
    set (kids(2), "zdata", [zarrw1.'; zend.'; zarrw2.'; NaN(1, length (z))](:));
  endif

  set (kids(1), "xdata", x);
  set (kids(1), "ydata", y);
  if (is3d)
    set (kids(1), "zdata", z);
  endif

endfunction

function update_props (h, ~)
  kids = get (h, "children");

  set (kids([3 2]), {"color", "linestyle", "linewidth"},
            get (h, {"color", "linestyle", "linewidth"}));
  set (kids(2), "visible", get (h, "showarrowhead"));
  set (kids(1), {"color", "marker", "markerfacecolor", "markersize"},
        get (h, {"color", "marker", "markerfacecolor", "markersize"}));
endfunction

