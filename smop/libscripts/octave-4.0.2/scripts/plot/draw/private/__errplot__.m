## Copyright (C) 2000-2015 Teemu Ikonen
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
## @deftypefn {Function File} {@var{h} =} __errplot__ (@var{fstr}, @var{hax}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Created: 18.7.2000
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function h = __errplot__ (fstr, hax, varargin)

  fmt = __pltopt__ ("__errplot__", fstr);

  ## Set the plot type based on linestyle.
  switch (fmt.errorstyle)
    case "~"
      ifmt = "yerr";
    case ">"
      ifmt = "xerr";
    case "~>"
      ifmt = "xyerr";
    case "#"
      ifmt = "box";
    case "#~"
      ifmt = "boxy";
    case "#~>"
      ifmt = "boxxy";
    otherwise
      ifmt = "yerr";
  endswitch

  h = [];
  nplots = ifelse (isempty (varargin{1}), 0, columns (varargin{1}));
  for i = 1:nplots

    if (isempty (fmt.color))
      lc = __next_line_color__ ();
    else
      lc = fmt.color ();
    endif
    if (isempty (fmt.marker) && isempty (fmt.linestyle))
      [ls, mk] = __next_line_style__ ();
    else
      ls = fmt.linestyle;
      mk = fmt.marker;
    endif

    ## Must occur after __next_line_color__ in order to work correctly.
    hg = hggroup ("parent", hax);
    h = [h; hg];
    args = __add_datasource__ ("__errplot__", hg,
                               {"x", "y", "l", "u", "xl", "xu"});

    hl = [(__line__ (hg, "color", lc, "linestyle", "-", "marker", "none")),
          (__line__ (hg, "color", lc, "linestyle", ls, "marker", mk))];

    switch (numel (varargin))
      case 2
        ydata = varargin{1}(:,i);
        xdata = 1:numel (ydata);
        if (strcmp (ifmt, "yerr") || strcmp (ifmt, "boxy"))
          ldata  = varargin{2}(:,i);
          udata  = ldata;
          xldata = [];
          xudata = [];
        elseif (strcmp (ifmt, "xerr") || strcmp (ifmt, "box"))
          xldata = varargin{2}(:,i);
          xudata = ldata;
          ldata  = [];
          udata  = [];
        else
          error ("errorbar: 2 column errorplot is only valid for xerr or yerr");
        endif
      case 3
        if (strcmp (ifmt, "yerr") || strcmp (ifmt, "boxy"))
          xdata  = varargin{1}(:,i);
          ydata  = varargin{2}(:,i);
          ldata  = varargin{3}(:,i);
          udata  = ldata;
          xldata = [];
          xudata = [];
        elseif (strcmp (ifmt, "xyerr") || strcmp (ifmt, "boxxy"))
          ydata  = varargin{1}(:,i);
          xdata  = 1:numel (ydata);
          xldata = varargin{2}(:,i);
          xudata = xldata;
          ldata  = varargin{3}(:,i);
          udata  = ldata;
        else  # xerr or box
          xdata  = varargin{1}(:,i);
          ydata  = varargin{2}(:,i);
          xldata = varargin{3}(:,i);
          xudata = xldata;
          ldata  = [];
          udata  = [];
        endif
      case 4
        if (strcmp (ifmt, "yerr") || strcmp (ifmt, "boxy"))
          xdata  = varargin{1}(:,i);
          ydata  = varargin{2}(:,i);
          ldata  = varargin{3}(:,i);
          udata  = varargin{4}(:,i);
          xldata = [];
          xudata = [];
        elseif (strcmp (ifmt, "xyerr") || strcmp (ifmt, "boxxy"))
          xdata  = varargin{1}(:,i);
          ydata  = varargin{2}(:,i);
          xldata = varargin{3}(:,i);
          xudata = xldata;
          ldata  = varargin{4}(:,i);
          udata  = ldata;
        else  # xerr or box
          xdata  = varargin{1}(:,i);
          ydata  = varargin{2}(:,i);
          xldata = varargin{3}(:,i);
          xudata = varargin{4}(:,i);
          ldata  = [];
          udata  = [];
        endif
      case 6  # xyerr, boxxy
        if (strcmp (ifmt, "xyerr") || strcmp (ifmt, "boxxy"))
          xdata  = varargin{1}(:,i);
          ydata  = varargin{2}(:,i);
          xldata = varargin{3}(:,i);
          xudata = varargin{4}(:,i);
          ldata  = varargin{5}(:,i);
          udata  = varargin{6}(:,i);
        else
          error ("errorbar: error plot with 6 columns only valid for xyerr and boxxy");
        endif
      otherwise
        error ("errorbar: error plot requires 2, 3, 4, or 6 arguments");
    endswitch

    addproperty ("xdata", hg, "data", xdata(:));
    addproperty ("ydata", hg, "data", ydata(:));
    addproperty ("ldata", hg, "data", ldata(:));
    addproperty ("udata", hg, "data", udata(:));
    addproperty ("xldata", hg, "data", xldata(:));
    addproperty ("xudata", hg, "data", xudata(:));
    addproperty ("format", hg, "string", ifmt);

    addproperty ("color", hg, "linecolor", get (hl(2), "color"));
    addproperty ("linestyle", hg, "linelinestyle", get (hl(2), "linestyle"));
    addproperty ("linewidth", hg, "linelinewidth", get (hl(2), "linewidth"));
    addproperty ("marker", hg, "linemarker", get (hl(2), "marker"));
    addproperty ("markeredgecolor", hg, "linemarkerfacecolor",
                 get (hl(2), "markeredgecolor"));
    addproperty ("markerfacecolor", hg, "linemarkerfacecolor",
                 get (hl(2), "markerfacecolor"));
    addproperty ("markersize", hg, "linemarkersize",
                 get (hl(2), "markersize"));

    ## Matlab property, although Octave does not implement it.
    addproperty ("hittestarea", hg, "radio", "on|{off}", "off");

    fcn = {@update_props, hl};
    addlistener (hg, "color", fcn);
    addlistener (hg, "linestyle", fcn);
    addlistener (hg, "linewidth", fcn);
    addlistener (hg, "marker", fcn);
    addlistener (hg, "markeredgecolor", fcn);
    addlistener (hg, "markerfacecolor", fcn);
    addlistener (hg, "markersize", fcn);

    fcn = {@update_data, hl};
    addlistener (hg, "xdata", fcn);
    addlistener (hg, "ydata", fcn);
    addlistener (hg, "ldata", fcn);
    addlistener (hg, "udata", fcn);
    addlistener (hg, "xldata", fcn);
    addlistener (hg, "xudata", fcn);
    addlistener (hg, "format", fcn);

    hax = ancestor (hg, "axes");
    addlistener (hax, "xscale", fcn);
    addlistener (hax, "yscale", fcn);

    update_data (hg, [], hl);

  endfor

  ## Process legend key
  if (! isempty (fmt.key) && nplots > 0)
    hlegend = [];
    fkids = get (gcf (), "children");
    for i = 1 : numel (fkids)
      if (   strcmp (get (fkids(i), "type"), "axes")
          && strcmp (get (fkids(i), "tag"), "legend"))
        udata = get (fkids(i), "userdata");
        if (! isempty (intersect (udata.handle, gca ())))
          hlegend = fkids(i);
          break;
        endif
      endif
    endfor

    if (isempty (hlegend))
      hlgnd = [];
      tlgnd = {};
    else
      [hlgnd, tlgnd] = __getlegenddata__ (hlegend);
    endif

    hlgnd(end+1) = hg;
    tlgnd(end+1) = fmt.key;

    legend (gca (), hlgnd, tlgnd);
  endif

endfunction

function [xdata, ydata] = errorbar_data (xdata, ydata, ldata, udata,
                                         xldata, xudata, ifmt,
                                         xscale, yscale)
  if (strcmp (xscale, "linear"))
    dx = 0.01 * (max (xdata(:)) - min (xdata(:)));
    xlo = xdata - dx;
    xhi = xdata + dx;
  else
    n = xdata > 0;
    if (! any (n))
      n = xdata < 0;
    endif
    logdata = log (abs (xdata(n)));
    rx = exp (0.01 * (max (logdata) - min (logdata)));
    xlo = xdata/rx;
    xhi = xdata*rx;
  endif
  if (strcmp (yscale, "linear"))
    dy = 0.01 * (max (ydata(:)) - min (ydata(:)));
    ylo = ydata - dy;
    yhi = ydata + dy;
  else
    n = ydata > 0;
    if (! any (n))
      n = ydata < 0;
    endif
    logdata = log (abs (ydata(n)));
    ry = exp (0.01 * (max (logdata) - min (logdata)));
    ylo = ydata/ry;
    yhi = ydata*ry;
  endif
  nans = NaN + xdata(:);  # fast way to do NaN (size (xdata(:)))
  if (strcmp (ifmt, "yerr"))
    xdata = [xdata, xdata, nans, ...
             xlo, xhi, nans, ...
             xlo, xhi, nans];
    ydata = [ydata-ldata, ydata+udata, nans, ...
             ydata+udata, ydata+udata, nans, ...
             ydata-ldata, ydata-ldata, nans];
  elseif (strcmp (ifmt, "xerr"))
    xdata = [xdata-xldata, xdata+xudata, nans, ...
             xdata+xudata, xdata+xudata, nans, ...
             xdata-xldata, xdata-xldata, nans];
    ydata = [ydata, ydata, nans, ...
             ylo, yhi, nans, ...
             ylo, yhi, nans];
  elseif (strcmp (ifmt, "boxy"))
    dx = 0.01 * (max (xdata(:)) - min (xdata(:)));
    xdata = [xlo, xhi, xhi, xlo, xlo, nans];
    ydata = [ydata-ldata, ydata-ldata, ydata+udata, ydata+udata, ...
             ydata-ldata, nans];
  elseif (strcmp (ifmt, "box"))
    dy = 0.01 * (max (ydata(:)) - min (ydata(:)));
    xdata = [xdata-xldata, xdata+xudata, xdata+xudata, xdata-xldata, ...
             xdata-xldata, nans];
    ydata = [ylo, ylo, yhi, yhi, ylo, nans];
  elseif (strcmp (ifmt, "boxxy"))
    xdata = [xdata-xldata, xdata+xudata, xdata+xudata, xdata-xldata, ...
             xdata-xldata, nans];
    ydata = [ydata-ldata, ydata-ldata, ydata+udata, ydata+udata, ...
             ydata-ldata, nans];
  elseif (strcmp (ifmt, "xyerr"))
    [x1, y1] = errorbar_data (xdata, ydata, ldata, udata,
                              xldata, xudata, "xerr", xscale, yscale);
    [x2, y2] = errorbar_data (xdata, ydata, ldata, udata,
                              xldata, xudata, "yerr", xscale, yscale);
    xdata = [x1; x2];
    ydata = [y1; y2];
    return;
  else
    error ("errorbar: valid error bar types are xerr, yerr, xyerr, box, boxy, boxxy");
  endif

  xdata = xdata.'(:);
  ydata = ydata.'(:);

endfunction

function update_props (hg, ~, hl)
  set (hl, "color", get (hg, "color"),
           "linewidth", get (hg, "linewidth"));
  set (hl(2), "linestyle", get (hg, "linestyle"),
              "marker", get (hg, "marker"),
              "markeredgecolor", get (hg, "markeredgecolor"),
              "markerfacecolor", get (hg, "markerfacecolor"),
              "markersize", get (hg, "markersize"));
endfunction

function update_data (hg, ~, hl)

  if (strcmp (get (hg, "type"), "axes"))
    hax = hg;
    hg = ancestor (hl(2), "hggroup");
  else
    hax = ancestor (hg, "axes");
  endif
  xscale = get (hax, "xscale");
  yscale = get (hax, "yscale");

  xdata  = get (hg, "xdata");
  ydata  = get (hg, "ydata");
  ldata  = get (hg, "ldata");
  udata  = get (hg, "udata");
  xldata = get (hg, "xldata");
  xudata = get (hg, "xudata");
  ifmt = get (hg, "format");

  set (hl(2), "xdata", xdata);
  set (hl(2), "ydata", ydata);

  [errorbar_xdata, errorbar_ydata] = ...
          errorbar_data (xdata, ydata, ldata, udata, xldata, xudata, ...
                         ifmt, xscale, yscale);

  set (hl(1), "xdata", errorbar_xdata);
  set (hl(1), "ydata", errorbar_ydata);

endfunction

