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
## @deftypefn  {Command} {} colorbar
## @deftypefnx {Function File} {} colorbar (@var{loc})
## @deftypefnx {Function File} {} colorbar (@var{delete_option})
## @deftypefnx {Function File} {} colorbar (@var{hcb}, @dots{})
## @deftypefnx {Function File} {} colorbar (@var{hax}, @dots{})
## @deftypefnx {Function File} {} colorbar (@dots{}, "peer", @var{hax}, @dots{})
## @deftypefnx {Function File} {} colorbar (@dots{}, "location", @var{loc}, @dots{})
## @deftypefnx {Function File} {} colorbar (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {@var{h} =} colorbar (@dots{})
## Add a colorbar to the current axes.
##
## A colorbar displays the current colormap along with numerical rulings
## so that the color scale can be interpreted.
##
## The optional input @var{loc} determines the location of the colorbar.
## Valid values for @var{loc} are
##
## @table @asis
## @item @qcode{"EastOutside"}
## Place the colorbar outside the plot to the right.  This is the default.
##
## @item @qcode{"East"}
## Place the colorbar inside the plot to the right.
##
## @item @qcode{"WestOutside"}
## Place the colorbar outside the plot to the left.
##
## @item @qcode{"West"}
## Place the colorbar inside the plot to the left.
##
## @item @qcode{"NorthOutside"}
## Place the colorbar above the plot.
##
## @item @qcode{"North"}
## Place the colorbar at the top of the plot.
##
## @item @qcode{"SouthOutside"}
## Place the colorbar under the plot.
##
## @item @qcode{"South"}
## Place the colorbar at the bottom of the plot.
## @end table
##
## To remove a colorbar from a plot use any one of the following keywords for
## the @var{delete_option}: @qcode{"delete"}, @qcode{"hide"}, @qcode{"off"}.
##
## If the argument @qcode{"peer"} is given, then the following argument is
## treated as the axes handle in which to add the colorbar.  Alternatively,
## If the first argument @var{hax} is an axes handle, then the colorbar is
## added to this axis, rather than the current axes returned by @code{gca}.
##
## If the first argument @var{hcb} is a handle to a colorbar object, then
## operate on this colorbar directly.
##
## Additional property/value pairs are passed directly to the underlying axes
## object.
##
## The optional return value @var{h} is a graphics handle to the created
## colorbar object.
##
## Implementation Note: A colorbar is created as an additional axes to the
## current figure with the @qcode{"tag"} property set to @qcode{"colorbar"}.
## The created axes object has the extra property @qcode{"location"} which
## controls the positioning of the colorbar.
## @seealso{colormap}
## @end deftypefn

function h = colorbar (varargin)

  [hcb, varargin, nargin] = __plt_get_axis_arg__ ("colorbar", varargin{:});

  if (hcb && ! strcmp (get (hcb, "tag"), "colorbar"))
    ax = hcb;
    hcb = [];
  else
    ax = [];
  endif
  loc = "";
  args = {};
  deleting = false;

  i = 1;
  while (i <= nargin)
    arg = varargin{i++};
    if (ischar (arg))
      switch (tolower (arg))
        case "peer"
          if (i > nargin)
            error ('colorbar: missing axes handle after "peer"');
          else
            ax = varargin{i++};
            if (! isscalar (ax) && ! isaxes (ax))
              error ('colorbar: expecting an axes handle following "peer"');
            endif
          endif
        case {"north", "south", "east", "west",
              "northoutside", "southoutside", "eastoutside", "westoutside"}
          loc = tolower (arg);
        case "location"
          if (i > nargin)
            error ('colorbar: missing value after "location"');
          else
            loc = tolower (varargin{i++});
          endif
        case {"delete", "hide", "off", "none"}
          deleting = true;
        otherwise
          args{end+1} = arg;
      endswitch
    else
      args{end+1} = arg;
    endif
  endwhile

  ## Handle changes to existing colorbar
  if (! isempty (hcb))
    if (deleting)
      delete (hcb);
      if (nargout > 0)
        h = hcb;
      endif
      return;
    else
      ## FIXME: No listener on location property so have to re-create
      ##        colorbar whenever an option changes.
      ##        re-instate this code if listener is developed.
      ## if (! isempty (loc))
      ##   set (hcb, "location", loc);
      ## endif
      ## if (! isempty (args))
      ##   set (hcb, args{:});
      ## endif
      ax = get (ancestor (hcb, "figure"), "currrentaxes");
    endif
  endif

  if (isempty (loc))
    loc = "eastoutside";
  endif
  if (isempty (ax))
    ax = gca ();
  endif

  showhiddenhandles = get (0, "showhiddenhandles");
  unwind_protect
    set (0, "showhiddenhandles", "on");
    cax = findobj (ancestor (ax, "figure"),
                   "tag", "colorbar", "type", "axes", "axes", ax);
    if (! isempty (cax))
      delete (cax);
    endif
  unwind_protect_cleanup
    set (0, "showhiddenhandles", showhiddenhandles);
  end_unwind_protect

  if (! deleting)
    ## FIXME: Matlab does not require the "position" property to be active.
    ##        Is there a way to determine the plotbox position for the
    ##        gnuplot graphics toolkit with the outerposition is active?
    set (ax, "activepositionproperty", "position");
    obj = get (ax);
    obj.__cbar_hax__ = ax;
    position = obj.position;

    hpar = ancestor (ax, "figure");
    clen = rows (get (hpar, "colormap"));
    cext = get (ax, "clim");
    cdiff = (cext(2) - cext(1)) / clen / 2;
    cmin = cext(1) + cdiff;
    cmax = cext(2) - cdiff;

    [pos, cpos, vertical, mirror] = ...
       __position_colorbox__ (loc, obj, ancestor (ax, "figure"));
    set (ax, "position", pos);

    cax = __go_axes__ (hpar, "tag", "colorbar",
                             "handlevisibility", "on",
                             "activepositionproperty", "position",
                             "position", cpos);
    addproperty ("location", cax, "radio",
                 "eastoutside|east|westoutside|west|northoutside|north|southoutside|south",
                 loc);
    addproperty ("axes", cax, "handle", ax);

    if (vertical)
      ## Use low-level form to avoid calling newplot which changes axes
      hi = image (cax, "xdata", [0,1], "ydata", [cmin, cmax],
                       "cdata", [1 : clen]');
      if (mirror)
        set (cax, "xtick", [], "xdir", "normal", "ydir", "normal",
                  "ylim", cext, "ylimmode", "manual",
                  "yaxislocation", "right", "layer", "top", args{:});
      else
        set (cax, "xtick", [], "xdir", "normal", "ydir", "normal",
                  "ylim", cext, "ylimmode", "manual",
                  "yaxislocation", "left", "layer", "top", args{:});
      endif
    else
      hi = image (cax, "xdata", [cmin, cmax], "ydata", [0,1],
                       "cdata", [1 : clen]);
      if (mirror)
        set (cax, "ytick", [], "xdir", "normal", "ydir", "normal",
                  "xlim", cext, "xlimmode", "manual",
                  "xaxislocation", "top", "layer", "top", args{:});
      else
        set (cax, "ytick", [], "xdir", "normal", "ydir", "normal",
                  "xlim", cext, "xlimmode", "manual",
                  "xaxislocation", "bottom", "layer", "top", args{:});
      endif
    endif

    ## Dummy object placed in axis to delete colorbar when axis is deleted.
    ctext = text (0, 0, "", "tag", "colorbar",
                  "visible", "off", "handlevisibility", "off",
                  "xliminclude", "off", "yliminclude", "off",
                  "zliminclude", "off",
                  "deletefcn", {@deletecolorbar, cax, obj});

    set (cax, "deletefcn", {@resetaxis, ax, obj});

    addlistener (hpar, "colormap", {@update_colorbar_cmap, hi, vertical, clen});
    addlistener (ax, "clim", {@update_colorbar_clim, hi, vertical});
    addlistener (ax, "dataaspectratio", {@update_colorbar_axis, cax, obj});
    addlistener (ax, "dataaspectratiomode", {@update_colorbar_axis, cax, obj});
    addlistener (ax, "plotboxaspectratio", {@update_colorbar_axis, cax, obj});
    addlistener (ax, "plotboxaspectratiomode", {@update_colorbar_axis, cax, obj});
    addlistener (ax, "position", {@update_colorbar_axis, cax, obj});

  endif

  if (nargout > 0)
    h = cax;
  endif

endfunction

function deletecolorbar (h, d, hc, orig_props)
  ## Don't delete the colorbar and reset the axis size if the
  ## parent figure is being deleted.
  if (isaxes (hc)
      && (isempty (gcbf ()) || strcmp (get (gcbf (), "beingdeleted"), "off")))
    if (strcmp (get (hc, "beingdeleted"), "off"))
      delete (hc);
    endif
    if (! isempty (ancestor (h, "axes"))
        && strcmp (get (ancestor (h, "axes"), "beingdeleted"), "off"))
      ax = ancestor (h, "axes");
      units = get (ax, "units");
      set (ax, "units", orig_props.units);
      set (ancestor (h, "axes"), "position", orig_props.position,
                            "outerposition", orig_props.outerposition,
                   "activepositionproperty", orig_props.activepositionproperty);
      set (ax, "units", units);
    endif
  endif
endfunction

function resetaxis (cax, d, ax, orig_props)
  if (isaxes (ax))
    ## FIXME: Probably don't want to delete everyone's listeners on colormap.
    dellistener (ancestor (ax, "figure"), "colormap");
    dellistener (ax, "clim");
    dellistener (ax, "dataaspectratio");
    dellistener (ax, "dataaspectratiomode");
    dellistener (ax, "plotboxaspectratio");
    dellistener (ax, "plotboxaspectratiomode");
    dellistener (ax, "position");

    units = get (ax, "units");
    set (ax, "units", orig_props.units);
    set (ax, "position", orig_props.position,
             "outerposition", orig_props.outerposition,
             "activepositionproperty", orig_props.activepositionproperty);
    set (ax, "units", units);
  endif
endfunction

function update_colorbar_clim (hax, d, hi, vert)
  if (isaxes (hax)
      && (isempty (gcbf ()) || strcmp (get (gcbf (), "beingdeleted"), "off")))
    clen = rows (get (ancestor (hax, "figure"), "colormap"));
    cext = get (hax, "clim");
    cdiff = (cext(2) - cext(1)) / clen / 2;
    cmin = cext(1) + cdiff;
    cmax = cext(2) - cdiff;

    hiax = get (hi, "parent");
    if (vert)
      set (hi, "ydata", [cmin, cmax]);
      set (hiax, "ylim", cext);
    else
      set (hi, "xdata", [cmin, cmax]);
      set (hiax, "xlim", cext);
    endif
  endif
endfunction

function update_colorbar_cmap (hf, d, hi, vert, init_sz)
  persistent sz = init_sz;

  if (ishandle (hf) && strcmp (get (hf, "type"), "figure")
      && (isempty (gcbf ()) || strcmp (get (gcbf (), "beingdeleted"), "off")))
    clen = rows (get (hf, "colormap"));
    if (clen != sz)
      if (vert)
        set (hi, "cdata", [1:clen]');
      else
        set (hi, "cdata", [1:clen]);
      endif
      sz = clen;
      ## Also update limits on axis or there will be white gaps
      update_colorbar_clim (get (hi, "parent"), d, hi, vert);
    endif
  endif
endfunction

function update_colorbar_axis (h, d, cax, orig_props)
  if (isaxes (cax)
      && (isempty (gcbf ()) || strcmp (get (gcbf (), "beingdeleted"),"off")))
    loc = get (cax, "location");
    obj = get (h);
    obj.__cbar_hax__ = h;
    obj.position = orig_props.position;
    obj.outerposition = orig_props.outerposition;
    [pos, cpos, vertical, mirror] = ...
       __position_colorbox__ (loc, obj, ancestor (h, "figure"));

    if (vertical)
      if (mirror)
        set (cax, "xtick", [], "xdir", "normal", "ydir", "normal",
                  "yaxislocation", "right", "position", cpos);
      else
        set (cax, "xtick", [], "xdir", "normal", "ydir", "normal",
                  "yaxislocation", "left", "position", cpos);
      endif
    else
      if (mirror)
        set (cax, "ytick", [], "xdir", "normal", "ydir", "normal",
                  "xaxislocation", "top", "position", cpos);
      else
        set (cax, "ytick", [], "xdir", "normal", "ydir", "normal",
                  "xaxislocation", "bottom", "position", cpos);
      endif
    endif

  endif
endfunction

function [pos, cpos, vertical, mirr] = __position_colorbox__ (cbox, obj, cf)

  ## This will always represent the position prior to adding the colorbar.
  pos = obj.position;
  sz = pos(3:4);

  if (strcmp (obj.plotboxaspectratiomode, "manual")
      || strcmp (obj.dataaspectratiomode, "manual"))
    if (isempty (strfind (cbox, "outside")))
      scale = 1.0;
    else
      scale = 0.8;
    endif
    if (isempty (strfind (cbox, "east")) && isempty (strfind (cbox, "west")))
      scale = [1, scale];
    else
      scale = [scale, 1];
    endif
    if (strcmp (get (cf, "__graphics_toolkit__"), "gnuplot")
        && strcmp (obj.activepositionproperty, "outerposition"))
      obj.outerposition = obj.outerposition .* [1, 1, scale];
      off = 0.5 * (obj.outerposition (3:4) - __actual_axis_position__ (obj)(3:4));
    else
      obj.position = obj.position .* [1, 1, scale];
      off = 0.5 * (obj.position (3:4) - __actual_axis_position__ (obj)(3:4));
    endif
  else
    off = 0.0;
  endif

  switch (cbox)
    case "northoutside"
      origin = pos(1:2) + [0., 0.9] .* sz + [1, -1] .* off;
      sz = sz .* [1.0, 0.06];
      pos(4) = 0.8 * pos(4);
      mirr = true;
      vertical = false;
    case "north"
      origin = pos(1:2) + [0.05, 0.9] .* sz + [1, -1] .* off;
      sz = sz .* [1.0, 0.06] * 0.9;
      mirr = false;
      vertical = false;
    case "southoutside"
      origin = pos(1:2) + off;
      sz = sz .* [1.0, 0.06];
      pos(2) = pos(2) + pos(4) * 0.2;
      pos(4) = 0.8 * pos(4);
      mirr = false;
      vertical = false;
    case "south"
      origin = pos(1:2) + [0.05, 0.05] .* sz + off;
      sz = sz .* [1.0, 0.06] * 0.9;
      mirr = true;
      vertical = false;
    case "eastoutside"
      origin = pos(1:2) + [0.9, 0] .* sz + [-1, 1] .* off;
      sz = sz .* [0.06, 1.0];
      pos(3) = 0.8 * pos(3);
      mirr = true;
      vertical = true;
    case "east"
      origin = pos(1:2) + [0.9, 0.05] .* sz + [-1, 1] .* off;
      sz = sz .* [0.06, 1.0] * 0.9;
      mirr = false;
      vertical = true;
    case "westoutside"
      origin = pos(1:2) + off;
      sz = sz .* [0.06, 1.0];
      pos(1) = pos(1) + pos(3) * 0.2;
      pos(3) = 0.8 * pos(3);
      mirr = false;
      vertical = true;
    case "west"
      origin = pos(1:2) + [0.05, 0.05] .* sz + off;
      sz = sz .* [0.06, 1.0] .* 0.9;
      mirr = true;
      vertical = true;
  endswitch

  cpos = [origin, sz];

  if (strcmp (obj.plotboxaspectratiomode, "manual")
      || strcmp (obj.dataaspectratiomode, "manual"))
    obj.position = pos;
    actual_pos = __actual_axis_position__ (obj);
    if (strfind (cbox, "outside"))
      scale = 1.0;
    else
      scale = 0.9;
    endif
    if (sz(1) > sz(2))
      ## Ensure north or south colorbars are the proper length
      dx = (1-scale)*actual_pos(3);
      cpos(1) = actual_pos(1) + dx/2;
      cpos(3) = actual_pos(3) - dx;
    else
      ## Ensure east or west colorbars are the proper height
      dy = (1-scale)*actual_pos(4);
      cpos(2) = actual_pos(2) + dy/2;
      cpos(4) = actual_pos(4) - dy;
    endif
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! imagesc (x);
%! colorbar ();
%! title ('colorbar() example');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! imagesc (x);
%! colorbar ('westoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! imagesc (x);
%! colorbar ('peer', gca, 'northoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! imagesc (x);
%! colorbar ('southoutside');

%!demo
%! clf;
%! colormap ('default');
%! contour (peaks ());
%! colorbar ('west');

%!demo
%! clf;
%! colormap ('default');
%! subplot (2,2,1);
%!  contour (peaks ());
%!  colorbar ('east');
%! subplot (2,2,2);
%!  contour (peaks ());
%!  colorbar ('west');
%! subplot (2,2,3);
%!  contour (peaks ());
%!  colorbar ('north');
%! subplot (2,2,4);
%!  contour (peaks ());
%!  colorbar ('south');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,2,1);
%!  imagesc (x);
%!  colorbar ();
%! subplot (2,2,2);
%!  imagesc (x);
%!  colorbar ('westoutside');
%! subplot (2,2,3);
%!  imagesc (x);
%!  colorbar ('northoutside');
%! subplot (2,2,4);
%!  imagesc (x);
%!  colorbar ('southoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  imagesc (x);
%!  axis square;
%!  colorbar ();
%! subplot (1,2,2);
%!  imagesc (x);
%!  axis square;
%!  colorbar ('westoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  imagesc (x);
%!  axis square;
%!  colorbar ('northoutside');
%! subplot (1,2,2);
%!  imagesc (x);
%!  axis square;
%!  colorbar ('southoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,1,1);
%!  imagesc (x);
%!  axis square;
%!  colorbar ();
%! subplot (2,1,2);
%!  imagesc (x);
%!  axis square;
%!  colorbar ('westoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,1,1);
%!  imagesc (x);
%!  axis square;
%!  colorbar ('northoutside');
%! subplot (2,1,2);
%!  imagesc (x);
%!  axis square;
%!  colorbar ('southoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  imagesc (x);
%!  colorbar ();
%! subplot (1,2,2);
%!  imagesc (x);
%!  colorbar ('westoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  imagesc (x);
%!  colorbar ('northoutside');
%! subplot (1,2,2);
%!  imagesc (x);
%!  colorbar ('southoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,1,1);
%!  imagesc (x);
%!  colorbar ();
%! subplot (2,1,2);
%!  imagesc (x);
%!  colorbar ('westoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (2,1,1);
%!  imagesc (x);
%!  colorbar ('northoutside');
%! subplot (2,1,2);
%!  imagesc (x);
%!  colorbar ('southoutside');

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! subplot (1,2,1);
%!  contour (x);
%!  axis square;
%!  colorbar ('east');
%!  xlim ([1, 64]);
%!  ylim ([1, 64]);
%! subplot (1,2,2);
%!  contour (x);
%!  colorbar ('west');
%!  xlim ([1, 64]);
%!  ylim ([1, 64]);

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! contour (x);
%! xlim ([1, 64]);
%! ylim ([1, 64]);
%! colorbar ();
%! colorbar off;

%!demo
%! clf;
%! colormap ('default');
%! n = 64; x = kron (1:n, ones (n,1)); x = abs (x - x.');
%! contour (x);
%! xlim ([1, 64]);
%! ylim ([1, 64]);
%! colorbar ();

%!demo
%! clf;
%! colormap ('default');
%! imagesc (1 ./ hilb (99));
%! h = colorbar ();
%! set (h, 'yscale', 'log');

%!demo
%! clf;
%! colormap ('default');
%! imagesc (log10 (1 ./ hilb (99)));
%! h = colorbar ();
%! ytick = get (h, 'ytick');
%! set (h, 'yticklabel', sprintf ('10^{%g}|', ytick));

%!demo
%! clf;
%! colormap ('default');
%! n = 5; x = linspace (0,5,n); y = linspace (0,1,n);
%! imagesc (1 ./ hilb (n));
%! axis equal;
%! colorbar ();

%!demo
%! clf;
%! colormap ('default');
%! n = 5; x = linspace (0,5,n); y = linspace (0,1,n);
%! imagesc (x, y, 1 ./ hilb (n));
%! axis equal;
%! colorbar ();

%!demo
%! clf;
%! colormap ('default');
%! n = 5; x = linspace (0,5,n); y = linspace (0,1,n);
%! imagesc (y, x, 1 ./ hilb (n));
%! axis equal;
%! colorbar ();

## This requires that the axes position be properly determined for 'axis equal'
%!demo
%! clf;
%! colormap ('default');
%! axes;
%! colorbar ();
%! hold on;
%! contour (peaks ());
%! hold off;

%!demo
%! clf;
%! colormap ('default');
%! plot ([0, 2]);
%! colorbar ('east');
%! axis square;

%!demo
%! clf;
%! colormap ('default');
%! plot ([0, 2]);
%! colorbar ('eastoutside');
%! axis square;

%!demo
%! clf;
%! colormap ('default');
%! pcolor (peaks (20));
%! shading interp;
%! axis ('tight', 'square');
%! colorbar ();
#%! axes ('color','none','box','on','activepositionproperty','position');

%!demo
%! clf;
%! colormap ('default');
%! plot ([0, 2]);
%! colorbar ('east');
%! axis equal;

%!demo
%! clf;
%! colormap ('default');
%! plot ([0, 2]);
%! colorbar ('eastoutside');
%! axis equal;

