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
## @deftypefn  {Function File} {} polar (@var{theta}, @var{rho})
## @deftypefnx {Function File} {} polar (@var{theta}, @var{rho}, @var{fmt})
## @deftypefnx {Function File} {} polar (@var{cplx})
## @deftypefnx {Function File} {} polar (@var{cplx}, @var{fmt})
## @deftypefnx {Function File} {} polar (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} polar (@dots{})
## Create a 2-D plot from polar coordinates @var{theta} and @var{rho}.
##
## If a single complex input @var{cplx} is given then the real part is used
## for @var{theta} and the imaginary part is used for @var{rho}.
##
## The optional argument @var{fmt} specifies the line format in the same way
## as @code{plot}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created plot.
##
## Implementation Note: The polar axis is drawn using line and text objects
## encapsulated in an hggroup.  The hggroup properties are linked to the
## original axes object such that altering an appearance property, for example
## @code{fontname}, will update the polar axis.  Two new properties are
## added to the original axes--@code{rtick}, @code{ttick}--which replace
## @code{xtick}, @code{ytick}.  The first is a list of tick locations in the
## radial (rho) direction; The second is a list of tick locations in the
## angular (theta) direction specified in degrees, i.e., in the range 0--359.
## @seealso{rose, compass, plot}
## @end deftypefn

## Author: jwe

function h = polar (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("polar", varargin{:});

  if (nargs < 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    if (nargs == 3)
      if (! ischar (varargin{3}))
        error ("polar: FMT argument must be a string");
      endif
      htmp = __plr2__ (hax, varargin{:});
      maxr = max (varargin{2}(:));
    elseif (nargs == 2)
      if (ischar (varargin{2}))
        htmp = __plr1__ (hax, varargin{:});
        if (iscomplex (varargin{1}))
          maxr = max (imag (varargin{1})(:));
        else
          maxr = max (varargin{1}(:));
        endif
      else
        fmt = "";
        htmp = __plr2__ (hax, varargin{:}, fmt);
        maxr = max (varargin{2}(:));
      endif
    elseif (nargs == 1)
      fmt = "";
      htmp = __plr1__ (hax, varargin{:}, fmt);
      if (iscomplex (varargin{1}))
        maxr = max (imag (varargin{1})(:));
      else
        maxr = max (varargin{1}(:));
      endif
    else
      print_usage ();
    endif

    if (! ishold (hax))
      hg = hggroup (hax, "tag", "polar_grid", "handlevisibility", "off");

      set (hax, "visible", "off", "plotboxaspectratio", [1, 1, 1],
                "zlim", [-1 1]);

      if (! isprop (hax, "rtick"))
        addproperty ("rtick", hax, "data");
      endif

      set (hax, "rtick", __calc_rtick__ (hax, maxr));

      ## add t(heta)tick
      if (! isprop (hax, "ttick"))
        addproperty ("ttick", hax, "data");
      endif

      ## theta(angular) ticks in degrees
      set (hax, "ttick", 0:30:330);

      __update_polar_grid__ (hax, [], hg);

      set (hg, "deletefcn", {@resetaxis, hax});

      addlistener (hax, "rtick", {@__update_polar_grid__, hg});
      addlistener (hax, "ttick", {@__update_polar_grid__, hg});
      addlistener (hax, "color", {@__update_patch__, hg});
      addlistener (hax, "fontangle", {@__update_text__, hg, "fontangle"});
      addlistener (hax, "fontname", {@__update_text__, hg, "fontname"});
      addlistener (hax, "fontsize", {@__update_text__, hg, "fontsize"});
      addlistener (hax, "fontunits", {@__update_text__, hg, "fontunits"});
      addlistener (hax, "fontweight", {@__update_text__, hg, "fontweight"});
      addlistener (hax, "interpreter", {@__update_text__, hg, "interpreter"});
      addlistener (hax, "layer", {@__update_layer__, hg});
      addlistener (hax, "gridlinestyle",{@__update_lines__,hg,"gridlinestyle"});
      addlistener (hax, "linewidth", {@__update_lines__, hg, "linewidth"});
    else
      hg = findall (hax, "tag", "polar_grid");
      if (! isempty (hg))
        oldrtick = max (get (hax, "rtick"));
        if (maxr > oldrtick)
          set (hax, "rtick", __calc_rtick__ (hax, maxr));
        endif
      endif
    endif

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction

function rtick = __calc_rtick__ (hax, maxr)
  ## FIXME: workaround: calculate r(ho)tick from xtick
  savexlim = get (hax, "xlim");
  saveylim = get (hax, "ylim");
  set (hax, "xlim", [-maxr maxr], "ylim", [-maxr maxr]);
  xtick = get (hax, "xtick");
  rtick = xtick(find (xtick > 0, 1):find (xtick >= maxr, 1));
  if (isempty (rtick))
    rtick = [0.5 1];
  endif
  set (hax, "xlim", savexlim, "ylim", saveylim);
endfunction

function retval = __plr1__ (h, theta, fmt)

  theta = theta(:);
  if (iscomplex (theta))
    rho = imag (theta);
    theta = real (theta);
  else
    rho = theta;
    theta = (1:rows (rho))';
  endif

  retval = __plr2__ (h, theta, rho, fmt);

endfunction

function retval = __plr2__ (h, theta, rho, fmt)

  if (ndims (theta) > 2 || ndims (rho) > 2)
    error ("polar: THETA and RHO must be 2-D objects");
  endif
  theta = real (theta);
  rho = real (rho);

  if (isscalar (theta))
    if (isscalar (rho))
      x = rho * cos (theta);
      y = rho * sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    else
      error ("polar: Can't plot constant THETA with varying RHO");
    endif
  elseif (isvector (theta))
    if (isvector (rho))
      if (length (theta) != length (rho))
        error ("polar: THETA and RHO vector lengths must match");
      endif
      rho = rho(:);
      theta = theta(:);
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    elseif (ismatrix (rho))
      theta = theta(:);
      t_nr = rows (theta);
      [r_nr, r_nc] = size (rho);
      if (t_nr != r_nr)
        rho = rho';
        r_nr = r_nc;
      endif
      if (t_nr != r_nr)
        error ("polar: THETA vector and RHO matrix sizes must match");
      endif
      x = diag (cos (theta)) * rho;
      y = diag (sin (theta)) * rho;
      retval = __plt__ ("polar", h, x, y, fmt);
    else
      error ("polar: invalid data for plotting");
    endif
  elseif (ismatrix (theta))
    if (isvector (rho))
      rho = rho(:);
      r_nr = rows (rho);
      [t_nr, t_nc] = size (theta);
      if (r_nr != t_nr)
        theta = theta';
        t_nr = t_nc;
      endif
      if (r_nr != t_nr)
        error ("polar: THETA matrix and RHO vector sizes must match");
      endif
      diag_r = diag (rho);
      x = diag_r * cos (theta);
      y = diag_r * sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    elseif (ismatrix (rho))
      if (! size_equal (rho, theta))
        error ("polar: THETA and RHO matrix dimensions must match");
      endif
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    else
      error ("polar: invalid data for plotting");
    endif
  else
    error ("polar: invalid data for plotting");
  endif

endfunction

## Callback functions for listeners

function __update_text__ (hax, ~, hg, prop)

  kids = get (hg, "children");
  idx = strcmp (get (kids, "type"), "text");
  set (kids(idx).', prop, get (hax, prop));

endfunction

function __update_lines__ (hax, ~, hg, prop)

  kids = get (hg, "children");
  idx = strcmp (get (kids, "type"), "line");
  lprop = prop;
  if (strcmp (prop, "gridlinestyle"))
    lprop = "linestyle";
  endif
  set (kids(idx).', lprop, get (hax, prop));

endfunction

function __update_patch__ (hax, ~, hg)

  kids = get (hg, "children");
  idx = strcmp (get (kids, "type"), "patch");
  set (kids(idx).', "facecolor", get (hax, "color"));

endfunction

function __update_layer__ (hax, ~, hg)

  ## FIXME: This re-implements allchild() because setting the "children"
  ##        property needs to preserve all children (titles, xlabels, etc.).
  shh = get (0, "showhiddenhandles");
  unwind_protect
    set (0, "showhiddenhandles", "on");
    kids = get (hax, "children");
    if (strcmp (get (hax, "layer"), "bottom"))
      set (hax, "children", [kids(kids != hg); hg]);
    else
      set (hax, "children", [hg; kids(kids != hg)]);
    endif
  unwind_protect_cleanup
    set (0, "showhiddenhandles", shh);
  end_unwind_protect
endfunction

function __update_polar_grid__ (hax, ~, hg)

  ## Delete existing polar grid
  delete (get (hg, "children"));

  rtick = unique (get (hax, "rtick")(:)');
  rtick = rtick(rtick > 0);
  if (isempty (rtick))
    rtick = [0.5 1];
  endif

  ttick = unique (get (hax, "ttick")(:)');
  ttick = ttick(ttick >= 0);
  if (isempty (ttick))
    ttick = 0:30:330;
  endif

  lprops = {"linestyle", get(hax, "gridlinestyle"), ...
            "linewidth", get(hax, "linewidth")};
  ## "fontunits" should be first because it affects "fontsize" property.
  tprops(1:2:12) = {"fontunits", "fontangle", "fontname", "fontsize", ...
                    "fontweight", "interpreter"};
  tprops(2:2:12) = get (hax, tprops(1:2:12));

  ## The number of points used for a circle
  circle_points = 50;
  t = linspace (0, 2*pi, circle_points)';
  x = kron (cos (t), rtick);
  y = kron (sin (t), rtick);

  ## Draw colored disk under axes at Z-depth = -1
  patch (x(:,end), y(:,end), -ones (circle_points, 1),
         get (hax, "color"), "parent", hg);

  ## Plot dotted circles
  line (x(:,1:end-1), y(:,1:end-1), lprops{:}, "parent", hg);

  ## Outer circle is drawn solid
  line (x(:,end), y(:,end), lprops{:}, "linestyle", "-", "parent", hg);

  ## Add radial labels
  [x, y] = pol2cart (0.42 * pi, rtick);
  text (x, y, num2cell (rtick), "verticalalignment", "bottom", tprops{:},
        "parent", hg);

  ## add radial lines
  s = rtick(end) * sin (ttick * pi / 180);
  c = rtick(end) * cos (ttick * pi / 180);
  x = [zeros(1, numel (ttick)); c];
  y = [zeros(1, numel (ttick)); s];
  line (x, y, "linestyle", ":", lprops{:}, "parent", hg);

  ## add angular labels
  tticklabel = num2cell (ttick);
  ## FIXME: This tm factor does not work as fontsize increases
  tm = 1.08;
  text (tm * c, tm * s, tticklabel, "horizontalalignment", "center",
        tprops{:}, "parent", hg);

  lim = 1.1 * rtick(end);
  set (hax, "xlim", [-lim, lim], "ylim", [-lim, lim]);

  ## Put polar grid behind or ahead of plot
  __update_layer__ (hax, [], hg);

endfunction

function resetaxis (~, ~, hax)
  if (isaxes (hax))
    dellistener (hax, "rtick");
    dellistener (hax, "ttick");
    dellistener (hax, "color");
    dellistener (hax, "fontangle");
    dellistener (hax, "fontname");
    dellistener (hax, "fontsize");
    dellistener (hax, "fontunits");
    dellistener (hax, "fontweight");
    dellistener (hax, "interpreter");
    dellistener (hax, "layer");
    dellistener (hax, "gridlinestyle");
    dellistener (hax, "linewidth");
  endif
endfunction


%!demo
%! clf;
%! theta = linspace (0,2*pi,1000);
%! rho = sin (7*theta);
%! polar (theta, rho);
%! title ('polar() plot');

%!demo
%! clf;
%! theta = linspace (0,2*pi,1000);
%! cplx = theta + i*sin (7*theta);
%! polar (cplx, 'g');
%! title ('polar() plot of complex data');

%!demo
%! clf;
%! theta = linspace (0,2*pi,1000);
%! rho = sin (2*theta).*cos (2*theta);
%! polar (theta, rho, '--r');
%! set (gca, 'rtick', 0.1:0.1:0.6, 'ttick', 0:20:340);
%! title ('polar() plot with finer grid');

%!demo
%! clf;
%! theta = linspace (0,2*pi,1000);
%! rho = sin (2*theta).*cos (2*theta);
%! polar (theta, rho, '--b');
%! set (gca, 'fontsize', 12, 'linewidth', 2, 'color', [0.8 0.8 0.8]);
%! title ('polar() plot with modified axis appearance');

%!demo
%! clf;
%! theta = linspace (0,8*pi,1000);
%! rho = sin (5/4*theta);
%! polar (theta, rho);
%! set (gca, 'rtick', 0.2:0.2:1);
%! title ('polar() plot');

