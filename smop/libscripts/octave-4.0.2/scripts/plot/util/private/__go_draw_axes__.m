## Copyright (C) 2005-2015 John W. Eaton
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
## @deftypefn {Function File} {} __go_draw_axes__ (@var{h}, @var{plot_stream}, @var{enhanced}, @var{mono})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function __go_draw_axes__ (h, plot_stream, enhanced, mono,
                           bg_is_set, fg_is_set, hlgnd)

  showhiddenhandles = get (0, "showhiddenhandles");
  unwind_protect
    set (0, "showhiddenhandles", "on");
    axis_obj = __get__ (h);
  unwind_protect_cleanup
    set (0, "showhiddenhandles", showhiddenhandles);
  end_unwind_protect

  parent_figure_obj = get (axis_obj.parent);
  gnuplot_term = __gnuplot_get_var__ (axis_obj.parent, "GPVAL_TERM");

  ## Set to false for plotyy axes.
  ymirror = true;
  if (isfield (axis_obj, "__plotyy_axes__"))
    if (all (ishandle (axis_obj.__plotyy_axes__)))
      ymirror = false;
    else
      h = axis_obj.__plotyy_axes__;
      h = h(ishandle (h));
      h = h(isprop (h, "__plotyy_axes__"));
      rmappdata (h, "__plotyy_axes__");
    endif
  endif

  nd = __calc_dimensions__ (h);

  if (strcmp (axis_obj.dataaspectratiomode, "manual")
      && strcmp (axis_obj.xlimmode, "manual")
      && strcmp (axis_obj.ylimmode, "manual"))
    ## All can't be "manual"
    axis_obj.plotboxaspectratiomode = "auto";
  endif

  if (strcmp (axis_obj.dataaspectratiomode, "manual")
      && strcmp (axis_obj.xlimmode, "manual")
      && strcmp (axis_obj.ylimmode, "manual")
      && (nd == 2 || all (mod (axis_obj.view, 90) == 0)))
    ## FIXME: adjust plotboxaspectratio to respect other
    fpos = get (axis_obj.parent, "position");
    apos = axis_obj.position;
  endif

  pos = __actual_axis_position__ (h);

  if (strcmpi (axis_obj.dataaspectratiomode, "manual"))
    dr = axis_obj.dataaspectratio;
    if (nd == 2 || all (mod (axis_obj.view, 90) == 0))
      dr = dr(1) / dr(2);
    else
      ## FIXME: need to properly implement 3D
      dr = mean (dr(1:2)) / dr(3);
    endif
  else
    dr = 1;
  endif

  if (strcmp (axis_obj.activepositionproperty, "position"))
    if (__gnuplot_has_feature__ ("screen_coordinates_for_{lrtb}margin"))
      if (nd == 2 || all (mod (axis_obj.view, 90) == 0))
        x = [1, 1];
      else
        ## 3D plots need to be sized down to fit in the window.
        x = 1.0 ./ sqrt ([2, 2.5]);
      endif
      fprintf (plot_stream, "set tmargin screen %.15g;\n",
               pos(2)+pos(4)/2+x(2)*pos(4)/2);
      fprintf (plot_stream, "set bmargin screen %.15g;\n",
               pos(2)+pos(4)/2-x(2)*pos(4)/2);
      fprintf (plot_stream, "set lmargin screen %.15g;\n",
               pos(1)+pos(3)/2-x(1)*pos(3)/2);
      fprintf (plot_stream, "set rmargin screen %.15g;\n",
               pos(1)+pos(3)/2+x(1)*pos(3)/2);
      sz_str = "";
    else
      fprintf (plot_stream, "set tmargin 0;\n");
      fprintf (plot_stream, "set bmargin 0;\n");
      fprintf (plot_stream, "set lmargin 0;\n");
      fprintf (plot_stream, "set rmargin 0;\n");

      if (nd == 3 && all (axis_obj.view == [0, 90]))
        ## FIXME: Kludge to allow colorbar to be added to a pcolor() plot
        pos(3:4) = pos(3:4) * 1.4;
        pos(1:2) = pos(1:2) - pos(3:4) * 0.125;
      endif

      fprintf (plot_stream, "set origin %.15g, %.15g;\n", pos(1), pos(2));

      if (strcmpi (axis_obj.dataaspectratiomode, "manual"))
        sz_str = sprintf ("set size ratio %.15g", -dr);
      else
        sz_str = "set size noratio";
      endif
      sz_str = sprintf ("%s %.15g, %.15g;\n", sz_str, pos(3), pos(4));
    endif
  else ## activepositionproperty == outerposition
    fprintf (plot_stream, "unset tmargin;\n");
    fprintf (plot_stream, "unset bmargin;\n");
    fprintf (plot_stream, "unset lmargin;\n");
    fprintf (plot_stream, "unset rmargin;\n");
    fprintf (plot_stream, "set origin %g, %g;\n", pos(1:2));
    sz_str = "";
    if (strcmpi (axis_obj.dataaspectratiomode, "manual"))
      sz_str = sprintf ("ratio %g", -dr);
    else
      sz_str = "noratio";
    endif
    sz_str = sprintf ("set size %s %g, %g;\n", sz_str, pos(3:4));
  endif
  if (! isempty (sz_str))
    fputs (plot_stream, sz_str);
  endif

  ## Reset all labels, axis-labels, tick-labels, and title
  ## FIXME: We should have an function to initialize the axis.
  ##        Presently, this is dispersed in this function.
  fputs (plot_stream, "unset label;\n");
  fputs (plot_stream, "unset xtics;\n");
  fputs (plot_stream, "unset ytics;\n");
  fputs (plot_stream, "unset ztics;\n");
  fputs (plot_stream, "unset x2tics;\n");
  fputs (plot_stream, "unset y2tics;\n");

  if (! isempty (axis_obj.title))
    t = get (axis_obj.title);
    if (isempty (t.string))
      fputs (plot_stream, "unset title;\n");
    else
      colorspec = get_text_colorspec (t.color, mono);
      [tt, f, s] = __maybe_munge_text__ (enhanced, t, "string");
      fontspec = create_fontspec (f, s, gnuplot_term);
      fprintf (plot_stream, "set title \"%s\" %s %s %s;\n",
               undo_string_escapes (tt), fontspec, colorspec,
               __do_enhanced_option__ (enhanced, t));
    endif
  endif

  if (! isempty (axis_obj.xlabel))
    t = get (axis_obj.xlabel);
    angle = t.rotation;
    colorspec = get_text_colorspec (t.color, mono);
    if (isempty (t.string))
      fprintf (plot_stream, "unset xlabel;\n");
      fprintf (plot_stream, "unset x2label;\n");
    else
      [tt, f, s] = __maybe_munge_text__ (enhanced, t, "string");
      fontspec = create_fontspec (f, s, gnuplot_term);
      if (strcmpi (axis_obj.xaxislocation, "top"))
        fprintf (plot_stream, "set x2label \"%s\" %s %s %s",
                 undo_string_escapes (tt), colorspec, fontspec,
                 __do_enhanced_option__ (enhanced, t));
      else
        fprintf (plot_stream, "set xlabel \"%s\" %s %s %s",
                 undo_string_escapes (tt), colorspec, fontspec,
                 __do_enhanced_option__ (enhanced, t));
      endif
      fprintf (plot_stream, " rotate by %f;\n", angle);
      if (strcmpi (axis_obj.xaxislocation, "top"))
        fprintf (plot_stream, "unset xlabel;\n");
      else
        fprintf (plot_stream, "unset x2label;\n");
      endif
    endif
  endif

  if (! isempty (axis_obj.ylabel))
    t = get (axis_obj.ylabel);
    angle = t.rotation;
    colorspec = get_text_colorspec (t.color, mono);
    if (isempty (t.string))
      fprintf (plot_stream, "unset ylabel;\n");
      fprintf (plot_stream, "unset y2label;\n");
    else
      [tt, f, s] = __maybe_munge_text__ (enhanced, t, "string");
      fontspec = create_fontspec (f, s, gnuplot_term);
      if (strcmpi (axis_obj.yaxislocation, "right"))
        fprintf (plot_stream, "set y2label \"%s\" %s %s %s",
                 undo_string_escapes (tt), colorspec, fontspec,
                 __do_enhanced_option__ (enhanced, t));
      else
        fprintf (plot_stream, "set ylabel \"%s\" %s %s %s",
                 undo_string_escapes (tt), colorspec, fontspec,
                 __do_enhanced_option__ (enhanced, t));
      endif
      fprintf (plot_stream, " rotate by %f;\n", angle);
      if (strcmpi (axis_obj.yaxislocation, "right"))
        fprintf (plot_stream, "unset ylabel;\n");
      else
        fprintf (plot_stream, "unset y2label;\n");
      endif
    endif
  endif

  if (! isempty (axis_obj.zlabel))
    t = get (axis_obj.zlabel);
    angle = t.rotation;
    colorspec = get_text_colorspec (t.color, mono);
    if (isempty (t.string))
      fputs (plot_stream, "unset zlabel;\n");
    else
      [tt, f, s] = __maybe_munge_text__ (enhanced, t, "string");
      fontspec = create_fontspec (f, s, gnuplot_term);
      fprintf (plot_stream, "set zlabel \"%s\" %s %s %s",
               undo_string_escapes (tt), colorspec, fontspec,
               __do_enhanced_option__ (enhanced, t));
      fprintf (plot_stream, " rotate by %f;\n", angle);
    endif
  endif

  if (strcmpi (axis_obj.xaxislocation, "top"))
    xaxisloc = "x2";
    xaxisloc_using = "x2";
  else
    xaxisloc = "x";
    xaxisloc_using = "x1";
    if (strcmpi (axis_obj.xaxislocation, "zero"))
      fputs (plot_stream, "set xzeroaxis;\n");
    endif
  endif
  if (strcmpi (axis_obj.yaxislocation, "right"))
    yaxisloc = "y2";
    yaxisloc_using = "y2";
  else
    yaxisloc = "y";
    yaxisloc_using = "y1";
    if (strcmpi (axis_obj.yaxislocation, "zero"))
      fputs (plot_stream, "set yzeroaxis;\n");
    endif
  endif

  have_grid = false;

  if (strcmpi (axis_obj.xgrid, "on"))
    have_grid = true;
    fprintf (plot_stream, "set grid %stics;\n", xaxisloc);
  else
    fprintf (plot_stream, "set grid no%stics;\n", xaxisloc);
  endif

  if (strcmpi (axis_obj.ygrid, "on"))
    have_grid = true;
    fprintf (plot_stream, "set grid %stics;\n", yaxisloc);
  else
    fprintf (plot_stream, "set grid no%stics;\n", yaxisloc);
  endif

  if (strcmpi (axis_obj.zgrid, "on"))
    have_grid = true;
    fputs (plot_stream, "set grid ztics;\n");
  else
    fputs (plot_stream, "set grid noztics;\n");
  endif

  if (strcmpi (axis_obj.xminorgrid, "on"))
    have_grid = true;
    if (strcmp (axis_obj.xscale, "log"))
      m = 10;
    else
      m = 5;
    endif
    fprintf (plot_stream, "set m%stics %d;\n", xaxisloc, m);
    fprintf (plot_stream, "set grid m%stics;\n", xaxisloc);
  else
    fprintf (plot_stream, "set grid nom%stics;\n", xaxisloc);
  endif

  if (strcmpi (axis_obj.yminorgrid, "on"))
    have_grid = true;
    if (strcmp (axis_obj.yscale, "log"))
      m = 10;
    else
      m = 5;
    endif
    fprintf (plot_stream, "set m%stics %d;\n", yaxisloc, m);
    fprintf (plot_stream, "set grid m%stics;\n", yaxisloc);
  else
    fprintf (plot_stream, "set grid nom%stics;\n", yaxisloc);
  endif

  if (strcmpi (axis_obj.zminorgrid, "on"))
    have_grid = true;
    if (strcmp (axis_obj.zscale, "log"))
      m = 10;
    else
      m = 5;
    endif
    fprintf (plot_stream, "set mztics %d;\n", m);
    fputs (plot_stream, "set grid mztics;\n");
  else
    fputs (plot_stream, "set grid nomztics;\n");
  endif

  ## The grid front/back/layerdefault option also controls the
  ## appearance of tics, so it is used even if the grid is absent.
  if (strcmpi (axis_obj.layer, "top"))
    fputs (plot_stream, "set grid front;\n");
    fputs (plot_stream, "set border front;\n");
  else
    fputs (plot_stream, "set grid layerdefault;\n");
    ## FIXME: The gnuplot help says that "layerdefault" should work
    ##        for set border too, but it fails for me with gnuplot 4.2.5.
    ##        So, use "back" instead.
    fputs (plot_stream, "set border back;\n");
  endif

  fprintf (plot_stream, "set grid linewidth %f, linewidth %f;\n",
           axis_obj.linewidth, axis_obj.linewidth);

  if (! have_grid)
    fputs (plot_stream, "unset grid;\n");
  endif

  xlogscale = strcmpi (axis_obj.xscale, "log");
  ylogscale = strcmpi (axis_obj.yscale, "log");
  zlogscale = strcmpi (axis_obj.zscale, "log");

  ## Detect logscale and negative lims
  if (xlogscale && all (axis_obj.xlim < 0))
    axis_obj.xsgn = -1;
    if (strcmp (axis_obj.xdir, "reverse"))
      axis_obj.xdir = "normal";
    elseif (strcmp (axis_obj.xdir, "normal"))
      axis_obj.xdir = "reverse";
    endif
    axis_obj.xtick = -flip (axis_obj.xtick);
    axis_obj.xticklabel = flip (axis_obj.xticklabel);
    axis_obj.xlim = -flip (axis_obj.xlim);
  else
    axis_obj.xsgn = 1;
  endif
  if (ylogscale && all (axis_obj.ylim < 0))
    axis_obj.ysgn = -1;
    if (strcmp (axis_obj.ydir, "reverse"))
      axis_obj.ydir = "normal";
    elseif (strcmp (axis_obj.ydir, "normal"))
      axis_obj.ydir = "reverse";
    endif
    axis_obj.ytick = -flip (axis_obj.ytick);
    axis_obj.yticklabel = flip (axis_obj.yticklabel);
    axis_obj.ylim = -flip (axis_obj.ylim);
  else
    axis_obj.ysgn = 1;
  endif
  if (zlogscale && all (axis_obj.zlim < 0))
    axis_obj.zsgn = -1;
    if (strcmp (axis_obj.zdir, "reverse"))
      axis_obj.zdir = "normal";
    elseif (strcmp (axis_obj.zdir, "normal"))
      axis_obj.zdir = "reverse";
    endif
    axis_obj.ztick = -flip (axis_obj.ztick);
    axis_obj.zticklabel = flip (axis_obj.zticklabel);
    axis_obj.zlim = -flip (axis_obj.zlim);
  else
    axis_obj.zsgn = 1;
  endif

  xlim = axis_obj.xlim;
  ylim = axis_obj.ylim;
  zlim = axis_obj.zlim;
  clim = axis_obj.clim;

  do_tics (axis_obj, plot_stream, ymirror, mono, gnuplot_term);

  fputs (plot_stream, "unset logscale;\n");
  if (xlogscale)
    fprintf (plot_stream, "set logscale %s;\n", xaxisloc);
  endif
  if (ylogscale)
    fprintf (plot_stream, "set logscale %s;\n", yaxisloc);
  endif
  if (zlogscale)
    fputs (plot_stream, "set logscale z;\n");
  endif

  xautoscale = strcmpi (axis_obj.xlimmode, "auto");
  yautoscale = strcmpi (axis_obj.ylimmode, "auto");
  zautoscale = strcmpi (axis_obj.zlimmode, "auto");
  cautoscale = strcmpi (axis_obj.climmode, "auto");
  cdatadirect = false;
  truecolor = false;

  fputs (plot_stream, "set clip two;\n");

  kids = axis_obj.children;
  ## Remove the axis labels and title from the children, and
  ## preserved the original order.
  [jnk, k] = setdiff (kids, [axis_obj.xlabel; axis_obj.ylabel; ...
                             axis_obj.zlabel; axis_obj.title]);
  kids = kids (sort (k));

  if (nd == 3)
    fputs (plot_stream, "set parametric;\n");
    fputs (plot_stream, "set style data lines;\n");
    fputs (plot_stream, "set surface;\n");
    fputs (plot_stream, "unset contour;\n");
  endif

  data_idx = 0;
  data = cell ();
  is_image_data = [];
  hidden_removal = NaN;
  view_map = false;

  if (! cautoscale && clim(1) == clim(2))
    clim(2)++;
  endif
  addedcmap = [];

  ximg_data = {};
  ximg_data_idx = 0;

  while (! isempty (kids))

    obj = get (kids(end));

    if (isfield (obj, "xdata"))
      obj.xdata = double (obj.xdata);
    endif
    if (isfield (obj, "ydata"))
      obj.ydata = double (obj.ydata);
    endif
    if (isfield (obj, "zdata"))
      obj.zdata = double (obj.zdata);
    endif

    if (isfield (obj, "units"))
      units = obj.units;
      unwind_protect
        set (kids(end), "units", "data");
        obj = get (kids(end));
      unwind_protect_cleanup
        set (kids(end), "units", units);
      end_unwind_protect
    endif
    kids = kids(1:(end-1));

    if (strcmp (obj.visible, "off"))
      continue;
    endif

    if (xlogscale && isfield (obj, "xdata"))
      obj.xdata = axis_obj.xsgn * obj.xdata;
      obj.xdata(obj.xdata<=0) = NaN;
    endif
    if (ylogscale && isfield (obj, "ydata"))
      obj.ydata = axis_obj.ysgn * obj.ydata;
      obj.ydata(obj.ydata<=0) = NaN;
    endif
    if (zlogscale && isfield (obj, "zdata"))
      obj.zdata = axis_obj.zsgn * obj.zdata;
      obj.zdata(obj.zdata<=0) = NaN;
    endif

    ## Check for facecolor interpolation for surfaces.
    doing_interp_color = ...
       isfield (obj, "facecolor") && strcmp (obj.facecolor, "interp");

    switch (obj.type)
      case "image"
        img_data = obj.cdata;
        img_xdata = obj.xdata;
        img_ydata = obj.ydata;

        if (ndims (img_data) == 3)
          truecolor = true;
        elseif (strcmpi (obj.cdatamapping, "direct"))
          cdatadirect = true;
        endif
        data_idx++;
        is_image_data(data_idx) = true;
        parametric(data_idx) = false;
        have_cdata(data_idx) = false;
        have_3d_patch(data_idx) = false;

        if (img_xdata(2) < img_xdata(1))
          img_xdata = img_xdata(2:-1:1);
          img_data = img_data(:,end:-1:1,:);
        elseif (img_xdata(1) == img_xdata(2))
          img_xdata = img_xdata(1) + [0, columns(img_data)-1];
        endif
        if (img_ydata(2) < img_ydata(1))
          img_ydata = img_ydata(2:-1:1);
          img_data = img_data(end:-1:1,:,:);
        elseif (img_ydata(1) == img_ydata(2))
          img_ydata = img_ydata(1) + [0, rows(img_data)-1];
        endif

        x_origin = min (img_xdata);
        y_origin = min (img_ydata);

        [y_dim, x_dim] = size (img_data(:,:,1));
        if (x_dim > 1)
          dx = abs (img_xdata(2)-img_xdata(1))/(x_dim-1);
        else
          x_dim = 2;
          img_data = [img_data, img_data];
          dx = abs (img_xdata(2)-img_xdata(1));
          if (dx < 1)
            ## Correct gnuplot string for 1-D images
            dx       = 0.5;
            x_origin = 0.75;
          endif
        endif
        if (y_dim > 1)
          dy = abs (img_ydata(2)-img_ydata(1))/(y_dim-1);
        else
          y_dim = 2;
          img_data = [img_data; img_data];
          dy = abs (img_ydata(2)-img_ydata(1));
          if (dy < 1)
            ## Correct gnuplot string for 1-D images
            dy       = 0.5;
            y_origin = 0.75;
          endif
        endif

        if (ndims (img_data) == 3)
          data{data_idx} = permute (img_data, [3, 1, 2])(:);
          format = "1:2:3";
          imagetype = "rgbimage";
        else
          data{data_idx} = img_data(:);
          format = "1";
          imagetype = "image";
        endif

        titlespec{data_idx} = "title \"\"";
        usingclause{data_idx} = sprintf ("binary array=%dx%d scan=yx origin=(%.15g,%.15g) dx=%.15g dy=%.15g using %s",
            x_dim, y_dim, x_origin, y_origin, dx, dy, format);
        withclause{data_idx} = sprintf ("with %s;", imagetype);

      case "line"
        if (strcmp (obj.linestyle, "none")
            && (! isfield (obj, "marker")
                || (isfield (obj, "marker")
                    && strcmp (obj.marker, "none"))))
          continue;
        endif
        data_idx++;
        is_image_data(data_idx) = false;
        parametric(data_idx) = true;
        have_cdata(data_idx) = false;
        have_3d_patch(data_idx) = false;
        if (isempty (obj.displayname))
          titlespec{data_idx} = "title \"\"";
        else
          tmp = undo_string_escapes (
                  __maybe_munge_text__ (enhanced, obj, "displayname")
                );
          titlespec{data_idx} = ['title "' tmp '"'];
        endif
        usingclause{data_idx} = sprintf ("record=%d", numel (obj.xdata));
        errbars = "";
        if (nd == 3)
          xdat = obj.xdata(:);
          ydat = obj.ydata(:);
          if (! isempty (obj.zdata))
            zdat = obj.zdata(:);
          else
            zdat = zeros (size (xdat));
          endif
          data{data_idx} = [xdat, ydat, zdat]';
          usingclause{data_idx} = sprintf ("record=%d using ($1):($2):($3)",
                                           numel (xdat));
          hidden_removal = false;
          ## fputs (plot_stream, "set parametric;\n");
        else
          xdat = obj.xdata(:);
          ydat = obj.ydata(:);
          data{data_idx} = [xdat, ydat]';
          usingclause{data_idx} = ...
            sprintf ("record=%d using ($1):($2) axes %s%s",
                     rows (xdat), xaxisloc_using, yaxisloc_using);
        endif

        style = do_linestyle_command (obj, obj.color, data_idx, mono,
                                      plot_stream, errbars);

        withclause{data_idx} = sprintf ("with %s linestyle %d",
                                        style{1}, data_idx);

        if (length (style) > 1)
          data_idx++;
          is_image_data(data_idx) = is_image_data(data_idx - 1);
          parametric(data_idx) = parametric(data_idx - 1);
          have_cdata(data_idx) = have_cdata(data_idx - 1);
          have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
          titlespec{data_idx} = "title \"\"";
          usingclause{data_idx} = usingclause{data_idx - 1};
          data{data_idx} = data{data_idx - 1};
          withclause{data_idx} = sprintf ("with %s linestyle %d",
                                          style{2}, data_idx);
        endif
        if (length (style) > 2)
          data_idx++;
          is_image_data(data_idx) = is_image_data(data_idx - 1);
          parametric(data_idx) = parametric(data_idx - 1);
          have_cdata(data_idx) = have_cdata(data_idx - 1);
          have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
          titlespec{data_idx} = "title \"\"";
          usingclause{data_idx} = usingclause{data_idx - 1};
          data{data_idx} = data{data_idx - 1};
          withclause{data_idx} = sprintf ("with %s linestyle %d",
                                          style{3}, data_idx);
        endif

     case "patch"
       cmap = parent_figure_obj.colormap;
       [nr, nc] = size (obj.xdata);

       if (! isempty (obj.cdata))
         cdat = obj.cdata;
         if (strcmpi (obj.cdatamapping, "direct"))
           cdatadirect = true;
         endif
       else
         cdat = [];
       endif

       data_3d_idx = NaN;
       for i = 1:nc
         xcol = obj.xdata(:,i);
         ycol = obj.ydata(:,i);
         if (nd == 3)
           if (! isempty (obj.zdata))
             zcol = obj.zdata(:,i);
           else
             zcol = zeros (size (xcol));
           endif
         endif

         if (! isnan (xcol) && ! isnan (ycol))
           ## Is the patch closed or not
           if (strcmp (obj.facecolor, "none"))
             hidden_removal = false;
           else

             if (isnan (hidden_removal))
               hidden_removal = true;
             endif
             if (nd == 3)
               if (numel (xcol) > 3)
                 error ("__go_draw_axes__: gnuplot (as of v4.2) only supports 3-D filled triangular patches");
               else
                 if (isnan (data_3d_idx))
                   data_idx++;
                   data_3d_idx = data_idx;
                   is_image_data(data_idx) = false;
                   parametric(data_idx) = false;
                   have_cdata(data_idx) = true;
                   have_3d_patch(data_idx) = true;
                   withclause{data_3d_idx} = sprintf ("with pm3d");
                   usingclause{data_3d_idx} =  "using 1:2:3:4";
                   data{data_3d_idx} = [];
                 endif
                 local_idx = data_3d_idx;
                 ccdat = NaN;
               endif
             else
               data_idx++;
               local_idx = data_idx;
               is_image_data(data_idx) = false;
               parametric(data_idx) = false;
               have_cdata(data_idx) = false;
               have_3d_patch(data_idx) = false;
             endif

             if (i > 1 || isempty (obj.displayname))
               titlespec{local_idx} = "title \"\"";
             else
               tmp = undo_string_escapes (
                       __maybe_munge_text__ (enhanced, obj, "displayname")
                     );
               titlespec{local_idx} = ['title "' tmp '"'];
             endif
             if (isfield (obj, "facecolor"))
               if ((strcmp (obj.facecolor, "flat")
                   || strcmp (obj.facecolor, "interp"))
                   && isfield (obj, "cdata"))
                 if (ndims (obj.cdata) == 2
                     && (columns (obj.cdata) == nc
                         && (rows (obj.cdata) == 1
                             || rows (obj.cdata) == 3)))
                   ccol = cdat(:, i);
                 elseif (ndims (obj.cdata) == 2
                     && (rows (obj.cdata) == nc
                         && (columns (obj.cdata) == 1
                             || columns (obj.cdata) == 3)))
                   ccol = cdat(i, :);
                 elseif (ndims (obj.cdata) == 3)
                   ccol = permute (cdat (:, i, :), [1, 3, 2]);
                 else
                   ccol = cdat;
                 endif
                 if (strcmp (obj.facecolor, "flat"))
                   if (isequal (size (ccol), [1, 3]))
                     ## RGB Triplet
                     color = ccol;
                   elseif (nd == 3 && numel (xcol) == 3)
                     ccdat = ccol;
                   else
                     if (cdatadirect)
                       r = round (ccol);
                     else
                       r = 1 + round ((rows (cmap) - 1)
                                      * (ccol - clim(1))/(clim(2) - clim(1)));
                     endif
                     r = max (1, min (r, rows (cmap)));
                     color = cmap(r, :);
                   endif
                 elseif (strcmp (obj.facecolor, "interp"))
                   if (nd == 3 && numel (xcol) == 3)
                     ccdat = ccol;
                     if (! isvector (ccdat))
                       tmp = rows (cmap) + rows (addedcmap) + ...
                            [1 : rows(ccdat)];
                       addedcmap = [addedcmap; ccdat];
                       ccdat = tmp(:);
                     else
                       ccdat = ccdat(:);
                     endif
                   else
                     if (sum (diff (ccol)))
                       warning ("\"interp\" not supported, using 1st entry of cdata");
                     endif
                     if (cdatadirect)
                       r = round (ccol);
                     else
                       r = 1 + round ((rows (cmap) - 1)
                                      * (ccol - clim(1))/(clim(2) - clim(1)));
                     endif
                     r = max (1, min (r, rows (cmap)));
                     color = cmap(r(1),:);
                   endif
                 endif
               elseif (isnumeric (obj.facecolor))
                 color = obj.facecolor;
               else
                 color = [0, 1, 0];
               endif
             else
               color = [0, 1, 0];
             endif

             if (nd == 3 && numel (xcol) == 3)
               if (isnan (ccdat))
                 ccdat = (rows (cmap) + rows (addedcmap) + 1) * ones(3, 1);
                 addedcmap = [addedcmap; reshape(color, 1, 3)];
               elseif (numel (ccdat) <= 1)
                 ccdat = zcol;
               endif
               data{data_3d_idx} = [data{data_3d_idx}, ...
                                    [[xcol; xcol(end)], [ycol; ycol(end)], ...
                                    [zcol; zcol(end)], [ccdat; ccdat(end)]]'];
             else
               if (mono)
                 colorspec = "";
               elseif (__gnuplot_has_feature__ ("transparent_patches")
                       && isscalar (obj.facealpha))
                 colorspec = sprintf ("lc rgb \"#%02x%02x%02x\" fillstyle transparent solid %f",
                                      round (255*color), obj.facealpha);
               else
                 colorspec = sprintf ("lc rgb \"#%02x%02x%02x\"",
                                      round (255*color));
               endif

               withclause{data_idx} = sprintf ("with filledcurve %s",
                                             colorspec);
               data{data_idx} = [xcol, ycol]';
               usingclause{data_idx} = sprintf ("record=%d using ($1):($2)",
                                                numel (xcol));
             endif
           endif
         endif

         ## patch outline
         if (!(strcmp (obj.edgecolor, "none")
                && (strcmp (obj.marker, "none")
                    || (strcmp (obj.markeredgecolor, "none")
                        && strcmp (obj.markerfacecolor, "none")))))

           data_idx++;
           is_image_data(data_idx) = false;
           parametric(data_idx) = false;
           have_cdata(data_idx) = false;
           have_3d_patch(data_idx) = false;
           titlespec{data_idx} = "title \"\"";
           usingclause{data_idx} = sprintf ("record=%d", numel (obj.xdata));

           if (isfield (obj, "markersize"))
             mdat = obj.markersize / 3;
           endif

           if (isfield (obj, "edgecolor"))
             ## FIXME: This is the wrong thing to do as edgecolor,
             ## markeredgecolor and markerfacecolor can have different values
             ## and we should treat them seperately. However, the code below
             ## allows the scatter functions to work as expected, where only
             ## one of these values is set.
             if (strcmp (obj.edgecolor, "none"))
               if (strcmp (obj.markeredgecolor, "none"))
                 ec = obj.markerfacecolor;
               else
                 ec = obj.markeredgecolor;
               endif
             else
               ec = obj.edgecolor;
             endif

             if ((strcmp (ec, "flat")
                  || strcmp (ec, "interp"))
                 && isfield (obj, "cdata"))
               if (ndims (obj.cdata) == 2
                   && (columns (obj.cdata) == nc
                       && (rows (obj.cdata) == 1
                           || rows (obj.cdata) == 3)))
                 ccol = cdat(:, i);
               elseif (ndims (obj.cdata) == 2
                       && (rows (obj.cdata) == nc
                           && (columns (obj.cdata) == 1
                               || columns (obj.cdata) == 3)))
                 ccol = cdat(i, :);
               elseif (ndims (obj.cdata) == 3)
                 ccol = permute (cdat (:, i, :), [1, 3, 2]);
               else
                 ccol = cdat;
               endif
               if (strcmp (ec, "flat"))
                 if (numel (ccol) == 3)
                   color = ccol;
                 else
                   if (isscalar (ccol))
                     ccol = repmat (ccol, numel (xcol), 1);
                   endif
                   color = "flat";
                   have_cdata(data_idx) = true;
                 endif
               elseif (strcmp (ec, "interp"))
                 if (numel (ccol) == 3)
                   warning ("\"interp\" not supported, using 1st entry of cdata");
                   color = ccol(1,:);
                 else
                   if (isscalar (ccol))
                     ccol = repmat (ccol, numel (xcol), 1);
                   endif
                   color = "interp";
                   have_cdata(data_idx) = true;
                 endif
               endif
             elseif (isnumeric (ec))
               color = ec;
             else
               color = [0, 0, 0];
             endif
           else
             color = [0, 0, 0];
           endif

           if (isfield (obj, "linestyle"))
             switch (obj.linestyle)
               case "-"
                 lt = "lt 1";
               case "--"
                 lt = "lt 2";
               case ":"
                 lt = "lt 3";
               case "-."
                 lt = "lt 6";
               case "none"
                 lt = "";
               otherwise
                 lt = "";
             endswitch
           else
             lt = "";
           endif

           if (isfield (obj, "linewidth"))
             lw = sprintf ("linewidth %f", obj.linewidth);
           else
             lw = "";
           endif

           [pt, pt2, obj] = gnuplot_pointtype (obj);
           if (! isempty (pt))
             pt = sprintf ("pointtype %s", pt);
           endif
           if (! isempty (pt2))
             pt2 = sprintf ("pointtype %s", pt2);
           endif

           if (mono)
             colorspec = "";
           else
             if (ischar (color))
               colorspec = "palette";
             else
               colorspec = sprintf ("lc rgb \"#%02x%02x%02x\"",
                                    round (255*color));
             endif
           endif

           sidx = 1;
           if (isempty (lt))
             style = "";
           else
             style = "lines";
           endif
           tmpwith = {};

           facesame = true;
           if (! isequal (pt, pt2) && isfield (obj, "markerfacecolor")
               && ! strcmp (obj.markerfacecolor, "none"))
             if (strcmp (obj.markerfacecolor, "auto")
                 || ! isnumeric (obj.markerfacecolor)
                 || (isnumeric (obj.markerfacecolor)
                     && isequal (color, obj.markerfacecolor)))
               style = strcat (style, "points");
               if (isfield (obj, "markersize"))
                 if (length (mdat) == nc)
                   m = mdat(i);
                 else
                   m = mdat;
                 endif
                 ps = sprintf ("pointsize %f", m / 3);
               else
                 ps = "";
               endif

               tmpwith{sidx} = sprintf ("with %s %s %s %s %s %s",
                                        style, lw, pt2, lt, ps,
                                        colorspec);
             else
               facesame = false;
               if (! isempty (style))
                 tmpwith{sidx} = sprintf ("with %s %s %s %s",
                                          style, lw, lt,
                                          colorspec);
                 sidx ++;
               endif
               if (isnumeric (obj.markerfacecolor) && ! mono)
                 colorspec = sprintf ("lc rgb \"#%02x%02x%02x\"",
                                      round (255*obj.markerfacecolor));
               endif
               style = "points";
               if (isfield (obj, "markersize"))
                 if (length (mdat) == nc)
                   m = mdat(i);
                 else
                   m = mdat;
                 endif
                 ps = sprintf ("pointsize %f", m / 3);
               else
                 ps = "";
               endif
               tmpwith{sidx} = sprintf ("with %s %s %s %s %s %s",
                                        style, lw, pt2, lt, ps,
                                        colorspec);
             endif
           endif

           if (isfield (obj, "markeredgecolor")
               && ! strcmp (obj.markeredgecolor, "none"))
             if (facesame && ! isempty (pt)
                 && (strcmp (obj.markeredgecolor, "auto")
                     || ! isnumeric (obj.markeredgecolor)
                     || (isnumeric (obj.markeredgecolor)
                         && isequal (color, obj.markeredgecolor))))
               if (sidx == 1 && ((length (style) == 5
                        && strncmp (style, "lines", 5))
                       || isempty (style)))
                 style = strcat (style, "points");
                 if (isfield (obj, "markersize"))
                   if (length (mdat) == nc)
                     m = mdat(i);
                   else
                     m = mdat;
                   endif
                   ps = sprintf ("pointsize %f", m / 3);
                 else
                   ps = "";
                 endif
                 tmpwith{sidx} = sprintf ("with %s %s %s %s %s %s",
                                          style, lw, pt, lt, ps,
                                          colorspec);
               endif
             else
               if (! isempty (style))
                 if (length (tmpwith) < sidx || isempty (tmpwith{sidx}))
                   tmpwith{sidx} = sprintf ("with %s %s %s %s",
                                            style, lw, lt,
                                            colorspec);
                 endif
                 sidx ++;
               endif

               if (! isempty (pt))
                 if (! mono)
                   if (strcmp (obj.markeredgecolor, "auto"))
                     colorspec = sprintf ("lc rgb \"#%02x%02x%02x\"",
                                          round (255*color));
                   elseif (isnumeric (obj.markeredgecolor) && ! mono)
                     colorspec = sprintf ("lc rgb \"#%02x%02x%02x\"",
                                          round (255*obj.markeredgecolor));
                   endif
                 endif
                 style = "points";
                 if (isfield (obj, "markersize"))
                   if (length (mdat) == nc)
                     m = mdat(i);
                   else
                     m = mdat;
                   endif
                   ps = sprintf ("pointsize %f", m / 3);
                 else
                   ps = "";
                 endif
                 tmpwith{sidx} = sprintf ("with %s %s %s %s %s %s",
                                          style, lw, pt, lt, ps,
                                          colorspec);
               endif
             endif
           endif

           if (isempty (tmpwith))
             withclause{data_idx} = sprintf ("with %s %s %s %s %s",
                                             style, lw, pt, lt,
                                             colorspec);
           else
             withclause{data_idx} = tmpwith{1};
           endif
           if (nd == 3)
             if (ischar (color))
               if (! isnan (xcol) && ! isnan (ycol) && ! isnan (zcol))
                 data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)], ...
                                   [zcol; zcol(1)], [ccol; ccol(1)]]';
               else
                 data{data_idx} = [xcol, ycol, zcol, ccol]';
               endif
               usingclause{data_idx} = sprintf ("record=%d using ($1):($2):($3):($4)", columns (data{data_idx}));
             else
               if (! isnan (xcol) && ! isnan (ycol) && ! isnan (zcol))
                 data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)], ...
                                   [zcol; zcol(1)]]';
               else
                 data{data_idx} = [xcol, ycol, zcol]';
               endif
               usingclause{data_idx} = sprintf ("record=%d using ($1):($2):($3)", columns (data{data_idx}));
             endif
           else
             if (ischar (color))
               if (! isnan (xcol) && ! isnan (ycol))
                 data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)], ...
                                   [ccol; ccol(1)]]';
               else
                 data{data_idx} = [xcol, ycol, ccol]';
               endif
               usingclause{data_idx} = sprintf ("record=%d using ($1):($2):($3)", columns (data{data_idx}));
             else
               if (! isnan (xcol) && ! isnan (ycol))
                 data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)]]';
               else
                 data{data_idx} = [xcol, ycol]';
               endif
               usingclause{data_idx} = sprintf ("record=%d using ($1):($2)", columns (data{data_idx}));
             endif
           endif

           if (length (tmpwith) > 1)
             data_idx++;
             is_image_data(data_idx) = is_image_data(data_idx - 1);
             parametric(data_idx) = parametric(data_idx - 1);
             have_cdata(data_idx) = have_cdata(data_idx - 1);
             have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
             titlespec{data_idx} = "title \"\"";
             usingclause{data_idx} = usingclause{data_idx - 1};
             data{data_idx} = data{data_idx - 1};
             withclause{data_idx} = tmpwith{2};
           endif
           if (length (tmpwith) > 2)
             data_idx++;
             is_image_data(data_idx) = is_image_data(data_idx - 1);
             parametric(data_idx) = parametric(data_idx - 1);
             have_cdata(data_idx) = have_cdata(data_idx - 1);
             have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
             titlespec{data_idx} = "title \"\"";
             usingclause{data_idx} = usingclause{data_idx - 1};
             data{data_idx} = data{data_idx - 1};
             withclause{data_idx} = tmpwith{3};
           endif
         endif
       endfor

      case "surface"
        view_map = true;
        if (! (strcmp (obj.edgecolor, "none")
               && strcmp (obj.facecolor, "none")))
          data_idx++;
          is_image_data(data_idx) = false;
          parametric(data_idx) = false;
          have_cdata(data_idx) = true;
          have_3d_patch(data_idx) = false;
          style = do_linestyle_command (obj, obj.edgecolor,
                                        data_idx, mono,
                                        plot_stream);

          if (isempty (obj.displayname))
            titlespec{data_idx} = "title \"\"";
          else
            tmp = undo_string_escapes (
                    __maybe_munge_text__ (enhanced, obj, "displayname")
                  );
            titlespec{data_idx} = ['title "' tmp '"'];
          endif
          withclause{data_idx} = sprintf ("with pm3d linestyle %d",
                                          data_idx);
          withpm3d = true;
          pm3didx = data_idx;

          xdat = obj.xdata;
          ydat = obj.ydata;
          zdat = obj.zdata;
          cdat = obj.cdata;

          err = false;
          if (! size_equal (zdat, cdat))
            err = true;
          endif
          if (isvector (xdat) && isvector (ydat) && ismatrix (zdat))
            if (rows (zdat) == length (ydat)
                && columns (zdat) == length (xdat))
              [xdat, ydat] = meshgrid (xdat, ydat);
            else
              err = true;
            endif
          elseif (ismatrix (xdat) && ismatrix (ydat) && ismatrix (zdat))
            if (! size_equal (xdat, ydat, zdat))
              err = true;
            endif
          else
            err = true;
          endif
          if (err)
            error ("__go_draw_axes__: invalid grid data");
          endif
          xlen = columns (zdat);
          ylen = rows (zdat);
          if (xlen == columns (xdat) && xlen == columns (ydat)
              && ylen == rows (xdat) && ylen == rows (ydat))
            len = 4 * xlen;
            zz = zeros (ylen, len);
            k = 1;
            for kk = 1:4:len
              zz(:,kk)   = xdat(:,k);
              zz(:,kk+1) = ydat(:,k);
              zz(:,kk+2) = zdat(:,k);
              zz(:,kk+3) = cdat(:,k);
              k++;
            endfor
            data{data_idx} = zz.';
          endif

          if (doing_interp_color)
            interp_str = "interpolate 0, 0";
          else
            ## No interpolation of facecolors.
            interp_str = "";
          endif
          usingclause{data_idx} = sprintf ("record=%dx%d using ($1):($2):($3):($4)", ylen, xlen);

          flat_interp_face = (strcmp (obj.facecolor, "flat")
                              || strcmp (obj.facecolor, "interp"));
          flat_interp_edge = (strcmp (obj.edgecolor, "flat")
                              || strcmp (obj.edgecolor, "interp"));

          facecolor_none_or_white = (strcmp (obj.facecolor, "none")
                                     || (isnumeric (obj.facecolor)
                                         && all (obj.facecolor == 1)));
          hidden_removal = false;
          fputs (plot_stream, "set style increment default;\n");
          if (flat_interp_edge && facecolor_none_or_white)
            withpm3d = false;
            withclause{data_idx} = sprintf ("with %s palette", style{1});
            fputs (plot_stream, "unset pm3d\n");
            if (all (obj.facecolor == 1))
              hidden_removal = true;
            endif
          elseif (facecolor_none_or_white)
            if (all (obj.facecolor == 1))
              hidden_removal = true;
            endif
            fputs (plot_stream,"unset pm3d;\n");
            fputs (plot_stream,"set style increment user;\n");
            withpm3d = false;
            withclause{data_idx} = sprintf ("with %s linestyle %d",
                                            style{1}, data_idx);
            fputs (plot_stream, "unset pm3d\n");
          endif

          if (doing_interp_color)
            ## "depthorder" interferes with interpolation of colors.
            dord = "scansautomatic";
          else
            dord = "depthorder";
          endif

          if (flat_interp_face && strcmp (obj.edgecolor, "flat"))
            fprintf (plot_stream,
                     "set pm3d explicit at s %s %s corners2color c3;\n",
                     interp_str, dord);
          elseif (! facecolor_none_or_white)
            if (strcmp (obj.edgecolor, "none"))
              if (__gnuplot_has_feature__ ("transparent_surface")
                  && isscalar (obj.facealpha))
                fprintf (plot_stream,
                         "set style fill transparent solid %f;\n",
                         obj.facealpha);
              endif
              fprintf (plot_stream,
                       "set pm3d explicit at s %s corners2color c3;\n",
                       interp_str, dord);
            else
              fprintf (plot_stream,
                       "set pm3d explicit at s hidden3d %d %s %s corners2color c3;\n",
                       data_idx, interp_str, dord);

              if (__gnuplot_has_feature__ ("transparent_surface")
                  && isscalar (obj.facealpha))
                fprintf (plot_stream,
                         "set style fill transparent solid %f;\n",
                         obj.facealpha);
              endif
            endif
          endif

          zz = [];
          if (length (style) > 1)
            len = 3 * xlen;
            zz = zeros (ylen, len);
            k = 1;
            for kk = 1:3:len
              zz(:,kk)   = xdat(:,k);
              zz(:,kk+1) = ydat(:,k);
              zz(:,kk+2) = zdat(:,k);
              k++;
            endfor
            zz = zz.';

            data_idx++;
            is_image_data(data_idx) = is_image_data(data_idx - 1);
            parametric(data_idx) = parametric(data_idx - 1);
            have_cdata(data_idx) = false;
            have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
            titlespec{data_idx} = "title \"\"";
            usingclause{data_idx} = sprintf ("record=%dx%d using ($1):($2):($3)", ylen, xlen);
            data{data_idx} = zz;
            withclause{data_idx} = sprintf ("with %s linestyle %d",
                                            style{2}, data_idx);

          endif
          if (length (style) > 2)
            data_idx++;
            is_image_data(data_idx) = is_image_data(data_idx - 1);
            parametric(data_idx) = parametric(data_idx - 1);
            have_cdata(data_idx) = false;
            have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
            titlespec{data_idx} = "title \"\"";
            usingclause{data_idx} = sprintf ("record=%dx%d using ($1):($2):($3)", ylen, xlen);
            data{data_idx} = zz;
            withclause{data_idx} = sprintf ("with %s linestyle %d",
                                            style{3}, data_idx);
          endif
          if (withpm3d && strcmp (style{1}, "linespoints"))
            if (isempty (zz))
              len = 3 * xlen;
              zz = zeros (ylen, len);
              k = 1;
              for kk = 1:3:len
                zz(:,kk)   = xdat(:,k);
                zz(:,kk+1) = ydat(:,k);
                zz(:,kk+2) = zdat(:,k);
                k++;
              endfor
              zz = zz.';
            endif
            data_idx++;
            is_image_data(data_idx) = is_image_data(data_idx - 1);
            parametric(data_idx) = parametric(data_idx - 1);
            have_cdata(data_idx) = false;
            have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
            titlespec{data_idx} = "title \"\"";
            usingclause{data_idx} = sprintf ("record=%dx%d using ($1):($2):($3)", ylen, xlen);
            data{data_idx} = zz;
            withclause{data_idx} = sprintf ("with points linestyle %d",
                                            pm3didx);
          endif
        endif

      case "text"
        [label, f, s] = __maybe_munge_text__ (enhanced, obj, "string");
        fontspec = create_fontspec (f, s, gnuplot_term);
        lpos = obj.position;
        halign = obj.horizontalalignment;
        valign = obj.verticalalignment;
        angle = obj.rotation;
        units = obj.units;
        color = obj.color;
        if (strcmpi (units, "normalized"))
          units = "graph";
        elseif (strcmp (axis_obj.yaxislocation, "right")
                && strcmp (units, "data"))
          units = "second";
        else
          units = "";
        endif

        if (isnumeric (color))
          colorspec = get_text_colorspec (color, mono);
        endif

        if (ischar (obj.string))
          num_lines = rows (obj.string);
          num_lines += numel (strfind (obj.string, "\n"));
        else
          num_lines = numel (obj.string);
        endif
        switch (valign)
          ## Text offset in characters. Relies on gnuplot for font metrics.
          case "top"
            dy = -0.5;
          case "cap"
            dy = -0.5;
          case "middle"
            dy = 0.5 * (num_lines - 1);
          case "baseline"
            dy = 0.5 + (num_lines - 1);
          case "bottom"
            dy = 0.5 + (num_lines - 1);
        endswitch
        ## Gnuplot's Character units are different for x/y and vary with
        ## fontsize. The aspect ratio of 1:1.7 was determined by experiment
        ## to work for eps/ps/etc. For the MacOS aqua terminal a value of 2.5
        ## is needed. However, the difference is barely noticable.
        dx_and_dy = [(-dy * sind (angle)), (dy * cosd (angle))] .* [1.7 1];

        ## FIXME: Multiline text produced the gnuplot
        ##        "warning: ft_render: skipping glyph"
        if (nd == 3)
          ## This produces the desired vertical alignment in 3D.
          fprintf (plot_stream,
                   "set label \"%s\" at %s %.15e,%.15e,%.15e %s rotate by %f offset character %f,%f %s %s front %s;\n",
                   undo_string_escapes (label), units, lpos(1),
                   lpos(2), lpos(3), halign, angle, dx_and_dy, fontspec,
                   __do_enhanced_option__ (enhanced, obj), colorspec);
        else
          fprintf (plot_stream,
                   "set label \"%s\" at %s %.15e,%.15e %s rotate by %f offset character %f,%f %s %s front %s;\n",
                   undo_string_escapes (label), units,
                   lpos(1), lpos(2), halign, angle, dx_and_dy, fontspec,
                   __do_enhanced_option__ (enhanced, obj), colorspec);
        endif

      case "hggroup"
        ## Push group children into the kid list.
        if (isempty (kids))
          kids = obj.children;
        elseif (! isempty (obj.children))
          kids = [kids; obj.children];
        endif

      otherwise
        error ("__go_draw_axes__: unknown object class, %s",
               obj.type);
    endswitch

  endwhile

  ## This is need to prevent warnings for rotations in 3D plots, while
  ## allowing colorbars with contours.
  if (nd == 2 || (data_idx > 1 && ! view_map))
    fputs (plot_stream, "set pm3d implicit;\n");
  else
    fputs (plot_stream, "set pm3d explicit;\n");
  endif

  if (isnan (hidden_removal) || hidden_removal)
    fputs (plot_stream, "set hidden3d;\n");
  else
    fputs (plot_stream, "unset hidden3d;\n");
  endif

  have_data = (! (isempty (data) || all (cellfun ("isempty", data))));

  ## Note we don't use the [xy]2range of gnuplot as we don't use the
  ## dual axis plotting features of gnuplot.
  if (isempty (xlim))
    return;
  endif
  if (strcmpi (axis_obj.xdir, "reverse"))
    xdir = "reverse";
  else
    xdir = "noreverse";
  endif
  fprintf (plot_stream, "set xrange [%.15e:%.15e] %s;\n", xlim, xdir);
  if (strcmpi (axis_obj.xaxislocation, "top"))
    fprintf (plot_stream, "set x2range [%.15e:%.15e] %s;\n", xlim, xdir);
  endif

  if (isempty (ylim))
    return;
  endif
  if (strcmpi (axis_obj.ydir, "reverse"))
    ydir = "reverse";
  else
    ydir = "noreverse";
  endif
  fprintf (plot_stream, "set yrange [%.15e:%.15e] %s;\n", ylim, ydir);
  if (strcmpi (axis_obj.yaxislocation, "right"))
    fprintf (plot_stream, "set y2range [%.15e:%.15e] %s;\n", ylim, ydir);
  endif

  if (nd == 3)
    if (isempty (zlim))
      return;
    endif
    if (strcmpi (axis_obj.zdir, "reverse"))
      zdir = "reverse";
    else
      zdir = "noreverse";
    endif
    fprintf (plot_stream, "set zrange [%.15e:%.15e] %s;\n", zlim, zdir);
  endif

  cmap = parent_figure_obj.colormap;
  cmap_sz = rows (cmap);
  if (! any (isinf (clim)))
    if (truecolor || ! cdatadirect)
      if (rows (addedcmap) > 0)
        for i = 1:data_idx
          if (have_3d_patch(i))
            data{i}(end,:) = clim(2) * (data{i}(end, :) - 0.5) / cmap_sz;
           endif
        endfor
        fprintf (plot_stream, "set cbrange [%.15e:%.15e];\n",
                 clim(1), clim(2) * (cmap_sz + rows (addedcmap)) / cmap_sz);
      else
        fprintf (plot_stream, "set cbrange [%.15e:%.15e];\n", clim);
      endif
    else
      fprintf (plot_stream, "set cbrange [1:%d];\n", cmap_sz +
               rows (addedcmap));
    endif
  endif

  if (strcmpi (axis_obj.box, "on"))
    if (nd == 3)
      fputs (plot_stream, "set border 4095;\n");
    else
      fputs (plot_stream, "set border 431;\n");
    endif
  else
    if (nd == 3)
      fputs (plot_stream, "set border 895;\n");
    elseif (! isempty (axis_obj.ytick))
      if (strcmpi (axis_obj.yaxislocation, "right"))
        fprintf (plot_stream, "unset ytics; set y2tics %s nomirror\n",
                 axis_obj.tickdir);
        if (strcmpi (axis_obj.xaxislocation, "top"))
          maybe_do_x2tick_mirror (plot_stream, axis_obj)
          fputs (plot_stream, "set border 12;\n");
        elseif (strcmpi (axis_obj.xaxislocation, "bottom"))
          maybe_do_xtick_mirror (plot_stream, axis_obj)
          fputs (plot_stream, "set border 9;\n");
        else # xaxislocation == zero
          fprintf (plot_stream, "unset x2tics; set xtics %s nomirror\n",
                   axis_obj.tickdir);
          fputs (plot_stream, "set border 8;\n");
          fprintf (plot_stream, "set xzeroaxis lt -1 lw %f;\n",
                   axis_obj.linewidth);
        endif
      elseif (strcmpi (axis_obj.yaxislocation, "left"))
        fprintf (plot_stream, "unset y2tics; set ytics %s nomirror\n",
                 axis_obj.tickdir);
        if (strcmpi (axis_obj.xaxislocation, "top"))
          maybe_do_x2tick_mirror (plot_stream, axis_obj)
          fputs (plot_stream, "set border 6;\n");
        elseif (strcmpi (axis_obj.xaxislocation, "bottom"))
          maybe_do_xtick_mirror (plot_stream, axis_obj)
          fputs (plot_stream, "set border 3;\n");
        else # xaxislocation == zero
          maybe_do_xtick_mirror (plot_stream, axis_obj)
          fputs (plot_stream, "set border 2;\n");
          fprintf (plot_stream, "set xzeroaxis lt -1 lw %f;\n",
                   axis_obj.linewidth);
        endif
      else # yaxislocation == zero
        fprintf (plot_stream, "unset y2tics; set ytics %s nomirror\n",
                 axis_obj.tickdir);
        if (strcmpi (axis_obj.xaxislocation, "top"))
          maybe_do_x2tick_mirror (plot_stream, axis_obj)
          fputs (plot_stream, "set border 4;\n");
        elseif (strcmpi (axis_obj.xaxislocation, "bottom"))
          maybe_do_xtick_mirror (plot_stream, axis_obj)
          fputs (plot_stream, "set border 1;\n");
        else # xaxislocation == zero
          maybe_do_xtick_mirror (plot_stream, axis_obj)
          fprintf (plot_stream, "unset y2tics; set ytics %s nomirror\n",
                   axis_obj.tickdir);
          fputs (plot_stream, "unset border;\n");
          fprintf (plot_stream, "set xzeroaxis lt -1 lw %f;\n",
                   axis_obj.linewidth);
        endif
        fprintf (plot_stream, "set yzeroaxis lt -1 lw %f;\n",
                 axis_obj.linewidth);
      endif
    endif
  endif

  if (strcmpi (axis_obj.visible, "off"))
    fputs (plot_stream, "unset border; unset tics\n");
  else
    fprintf (plot_stream, "set border lw %f;\n", axis_obj.linewidth);
  endif

  if (! isempty (hlgnd) && ! isempty (hlgnd.children)
      && any (strcmpi (get (hlgnd.children, "visible"), "on")))
    if (strcmpi (hlgnd.box, "on"))
      box = "box";
    else
      box = "nobox";
    endif
    if (strcmpi (hlgnd.orientation, "vertical"))
      horzvert = "vertical";
    else
      horzvert = "horizontal";
    endif
    if (strcmpi (hlgnd.textposition, "right"))
      reverse = "reverse";
    else
      reverse = "noreverse";
    endif
    inout = "inside";
    keypos = hlgnd.location;
    if (ischar (keypos))
      keypos = lower (keypos);
      keyout = strfind (keypos, "outside");
      if (! isempty (keyout))
        inout = "outside";
        keypos = keypos(1:keyout-1);
      endif
    endif
    switch (keypos)
      case "north"
        pos = "center top";
      case "south"
        pos = "center bottom";
      case "east"
        pos = "right center";
      case "west"
        pos = "left center";
      case "northeast"
        pos = "right top";
      case "northwest"
        pos = "left top";
      case "southeast"
        pos = "right bottom";
      case "southwest"
        pos = "left bottom";
      case "best"
        pos = "";
        warning ("legend: 'Best' not yet implemented for location specifier.\n");
        ## Least conflict with data in plot.
        ## Least unused space outside plot.
      otherwise
        pos = "";
    endswitch
    if (__gnuplot_has_feature__ ("key_has_font_properties"))
      [fontname, fontsize] = get_fontname_and_size (hlgnd);
      fontspacespec = [ create_spacingspec(fontname, fontsize, gnuplot_term),...
                        create_fontspec(fontname, fontsize, gnuplot_term) ];
    else
      fontspacespec = "";
    endif
    textcolors = get (findobj (hlgnd.children, "type", "text"), "color");
    if (iscell (textcolors))
      textcolors = cell2mat (textcolors);
      textcolors = unique (textcolors, "rows");
    endif
    if (rows (textcolors) > 1)
      ## Gnuplot is unable to assign arbitrary colors to each text entry
      ## for the key/legend.  But, the text color can be set to match the
      ## color of the plot object.
      colorspec = "textcolor variable";
    else
      colorspec = get_text_colorspec (textcolors, mono);
    endif
    fprintf (plot_stream, "set key %s %s;\nset key %s %s %s %s %s %s;\n",
             inout, pos, box, reverse, horzvert, fontspacespec, colorspec,
             __do_enhanced_option__ (enhanced, hlgnd));
  else
    fputs (plot_stream, "unset key;\n");
  endif
  fputs (plot_stream, "set style data lines;\n");

  cmap = [cmap; addedcmap];
  cmap_sz = cmap_sz + rows (addedcmap);
  if (mono == false && length (cmap) > 0)
    fprintf (plot_stream,
             "set palette positive color model RGB maxcolors %i;\n",
             cmap_sz);
    fprintf (plot_stream,
             "set palette file \"-\" binary record=%d using 1:2:3:4;\n",
             cmap_sz);
    fwrite (plot_stream, [1:cmap_sz; cmap.'], "float32");
    fwrite (plot_stream, "\n");
  endif

  fputs (plot_stream, "unset colorbox;\n");

  if (have_data)
    if (nd == 2)
      plot_cmd = "plot";
    else
      plot_cmd = "splot";
      rot_x = 90 - axis_obj.view(2);
      rot_z = axis_obj.view(1);
      while (rot_z < 0)
        rot_z += 360;
      endwhile
      fputs (plot_stream, "set ticslevel 0;\n");
      if (view_map && rot_x == 0 && rot_z == 0)
        fputs (plot_stream, "set view map;\n");
      else
        fprintf (plot_stream, "set view %.15g, %.15g;\n", rot_x, rot_z);
      endif
    endif
    if (have_3d_patch (1))
      fputs (plot_stream, "set pm3d depthorder\n");
      fprintf (plot_stream, "%s \"-\" %s %s %s \\\n", plot_cmd,
               usingclause{1}, titlespec{1}, withclause{1});
    elseif (is_image_data (1))
      if (numel (is_image_data) > 1 && is_image_data(2))
        ## Remove terminating semicolon
        n = max (strfind (withclause{1}, ";"));
        if (! isempty (n))
          withclause{1} = withclause{1}(1:n-1);
        endif
      endif
      fprintf (plot_stream, "%s \"-\" %s %s %s \\\n", plot_cmd,
               usingclause{1}, titlespec{1}, withclause{1});
    else
      fprintf (plot_stream, "%s \"-\" binary format='%%float64' %s %s %s \\\n",
               plot_cmd, usingclause{1}, titlespec{1}, withclause{1});
    endif
    for i = 2:data_idx
      if (have_3d_patch (i))
        fprintf (plot_stream, ", \"-\" %s %s %s \\\n",
                 usingclause{i}, titlespec{i}, withclause{i});
      elseif (is_image_data (i))
        if (! is_image_data (i-1))
          fputs (plot_stream, "; ");
          if (bg_is_set)
            fputs (plot_stream, "unset obj 1; \\\n");
            bg_is_set = false;
          endif
          if (fg_is_set)
            fputs (plot_stream, "unset obj 2; \\\n");
            fg_is_set = false;
          endif
          if (numel (is_image_data) > i && is_image_data(i+1))
            ## Remove terminating semicolon
            n = max (strfind (withclause{i}, ";"));
            if (! isempty (n))
              withclause{i} = withclause{i}(1:n-1);
            endif
          endif
          fprintf (plot_stream, "%s \"-\" %s %s %s \\\n", plot_cmd,
                   usingclause{i}, titlespec{i}, withclause{i});
        else
          ## For consecutive images continue with the same plot command
          fprintf (plot_stream, "%s \"-\" %s %s %s \\\n", ",",
                   usingclause{i}, titlespec{i}, withclause{i});
        endif
      elseif (is_image_data (i-1))
        if (bg_is_set)
          fputs (plot_stream, "unset obj 1; \\\n");
          bg_is_set = false;
        endif
        if (fg_is_set)
          fputs (plot_stream, "unset obj 2; \\\n");
          fg_is_set = false;
        endif
        fprintf (plot_stream,"%s \"-\" binary format='%%float64' %s %s %s \\\n",
                 plot_cmd, usingclause{i}, titlespec{i}, withclause{i});
      else
        fprintf (plot_stream, ", \"-\" binary format='%%float64' %s %s %s \\\n",
                 usingclause{i}, titlespec{i}, withclause{i});
      endif
    endfor
    fputs (plot_stream, ";\n");
    for i = 1:data_idx
      if (have_3d_patch (i))
        ## Can't write 3d patch data as binary as can't plot more than
        ## a single patch at a time and have to plot all patches together
        ## so that the gnuplot depth ordering is done correctly
        for j = 1 : 4 : columns (data{i})
          if (j != 1)
            fputs (plot_stream, "\n\n");
          endif
          fprintf (plot_stream, "%.15g %.15g %.15g %.15g\n", data{i}(:,j).');
          fprintf (plot_stream, "%.15g %.15g %.15g %.15g\n\n",data{i}(:,j+1).');
          fprintf (plot_stream, "%.15g %.15g %.15g %.15g\n", data{i}(:,j+2).');
          fprintf (plot_stream, "%.15g %.15g %.15g %.15g\n", data{i}(:,j+3).');
        endfor
        fputs (plot_stream, "e\n");
      elseif (is_image_data(i))
        fwrite (plot_stream, data{i}, "float32");
      else
        __gnuplot_write_data__ (plot_stream, data{i}, nd, parametric(i),
                                have_cdata(i));
      endif
    endfor
  else
    fputs (plot_stream, "plot \"-\";\nInf Inf\ne\n");
  endif

  ## Needed to allow mouse rotation with pcolor.
  if (view_map)
    fputs (plot_stream, "unset view;\n");
  endif

  if (bg_is_set)
    fputs (plot_stream, "unset obj 1;\n");
    bg_is_set = false;
  endif

  fflush (plot_stream);

endfunction

function x = flip (x)
  if (rows (x) == 1)
    x = fliplr (x);
  elseif (columns (x) == 1 || ischar (x))
    x = flipud (x);
  else
    x = flipud (fliplr (x));
  endif
endfunction

function spacing_spec = create_spacingspec (f, s, gp_term)
  ## The gnuplot default font size is 10, and default spacing is 1.25.
  ## gnuplot has a concept of a figure global font, and sizes everything
  ## appropriate to that, including the legend spacing.
  ##
  ## This means that if an alternative size is used, gnuplot will use an
  ## inappropriate spacing in the legend by default.
  ##
  ## FIXME: Are fractional spacing specifications allowed?  Or should this
  ##        number be rounded?
  spc = s / 10 * 1.25;
  spacing_spec = sprintf ("spacing %d", spc);

endfunction

function fontspec = create_fontspec (f, s, gp_term)
  if (strcmp (f, "*") || strcmp (gp_term, "tikz"))
    fontspec = sprintf ("font \",%d\"", s);
  else
    fontspec = sprintf ("font \"%s,%d\"", f, s);
  endif
endfunction

function style = do_linestyle_command (obj, linecolor, idx, mono,
                                       plot_stream, errbars = "")
  style = {};

  fprintf (plot_stream, "set style line %d default;\n", idx);
  fprintf (plot_stream, "set style line %d", idx);

  found_style = false;
  if (isnumeric (linecolor))
    color = linecolor;
    if (! mono)
      fprintf (plot_stream, " linecolor rgb \"#%02x%02x%02x\"",
               round (255*color));
    endif
  else
    color = [0, 0, 0];
  endif

  if (isfield (obj, "linestyle"))
    switch (obj.linestyle)
      case "-"
        lt = "1";
      case "--"
        lt = "2";
      case ":"
        lt = "3";
      case "-."
        lt = "6";
      case "none"
        lt = "";
      otherwise
        lt = "";
    endswitch

    if (! isempty (lt))
      fprintf (plot_stream, " linetype %s", lt);
    endif

  else
    lt = "";
  endif
  if (! isempty (errbars))
    found_style = true;
  endif

  if (isfield (obj, "linewidth"))
    fprintf (plot_stream, " linewidth %f", obj.linewidth);
    found_style = true;
  endif

  [pt, pt2, obj] = gnuplot_pointtype (obj);

  if (! isempty (pt))
    found_style = true;
  endif

  sidx = 1;
  if (isempty (errbars))
    if (isempty (lt))
      style{sidx} = "";
    else
      style{sidx} = "lines";
    endif

    facesame = true;
    if (! isequal (pt, pt2) && isfield (obj, "markerfacecolor")
        && ! strcmp (obj.markerfacecolor, "none"))
      if (strcmp (obj.markerfacecolor, "auto")
          || ! isnumeric (obj.markerfacecolor)
          || (isnumeric (obj.markerfacecolor)
              && isequal (color, obj.markerfacecolor)))
        if (! isempty (pt2))
          fprintf (plot_stream, " pointtype %s", pt2);
          style{sidx} = strcat (style{sidx}, "points");
        endif
        if (isfield (obj, "markersize"))
          fprintf (plot_stream, " pointsize %f", obj.markersize / 3);
        endif
      else
        facesame = false;
        if (! found_style)
          fputs (plot_stream, " default");
        endif
        fputs (plot_stream, ";\n");
        if (! isempty (style{sidx}))
          sidx ++;
          idx ++;
        else
          fputs (plot_stream, ";\n");
        endif
        fprintf (plot_stream, "set style line %d default;\n", idx);
        fprintf (plot_stream, "set style line %d", idx);
        if (isnumeric (obj.markerfacecolor) && ! mono)
          fprintf (plot_stream, " linecolor rgb \"#%02x%02x%02x\"",
                   round (255*obj.markerfacecolor));
        endif
        if (! isempty (pt2))
          style{sidx} = "points";
          fprintf (plot_stream, " pointtype %s", pt2);
        endif
        if (isfield (obj, "markersize"))
          fprintf (plot_stream, " pointsize %f", obj.markersize / 3);
        endif
      endif
    endif
    if (isfield (obj, "markeredgecolor")
        && ! strcmp (obj.markeredgecolor, "none"))
      if (facesame && ! isempty (pt)
          && (strcmp (obj.markeredgecolor, "auto")
              || ! isnumeric (obj.markeredgecolor)
              || (isnumeric (obj.markeredgecolor)
                  && isequal (color, obj.markeredgecolor))))
        if (sidx == 1 && ((length (style{sidx}) == 5
            && strncmp (style{sidx}, "lines", 5)) || isempty (style{sidx})))
          if (! isempty (pt))
            style{sidx} = strcat (style{sidx}, "points");
            fprintf (plot_stream, " pointtype %s", pt);
          endif
          if (isfield (obj, "markersize"))
            fprintf (plot_stream, " pointsize %f", obj.markersize / 3);
          endif
        endif
      else
        if (! found_style)
          fputs (plot_stream, " default");
        endif
        fputs (plot_stream, ";\n");
        if (! isempty (style{sidx}))
          sidx ++;
          idx ++;
        else
          fputs (plot_stream, ";\n");
        endif
        fprintf (plot_stream, "set style line %d default;\n", idx);
        fprintf (plot_stream, "set style line %d", idx);
        if (! mono)
          if (strcmp (obj.markeredgecolor, "auto"))
            fprintf (plot_stream, " linecolor rgb \"#%02x%02x%02x\"",
                     round (255*color));
          elseif (isnumeric (obj.markeredgecolor) && ! mono)
            fprintf (plot_stream, " linecolor rgb \"#%02x%02x%02x\"",
                     round (255*obj.markeredgecolor));
          endif
        endif
        if (! isempty (pt))
          style{sidx} = "points";
          fprintf (plot_stream, " pointtype %s", pt);
        endif
        if (isfield (obj, "markersize"))
          fprintf (plot_stream, " pointsize %f", obj.markersize / 3);
        endif
      endif
    endif
  else
    style{1} = errbars;
    fputs (plot_stream, " pointtype 0");
  endif

  if (! found_style && isempty (style{1}))
    fputs (plot_stream, " default");
  endif

  fputs (plot_stream, ";\n");

endfunction

function [pt, pt2, obj] = gnuplot_pointtype (obj)
  if (isfield (obj, "marker"))
    switch (obj.marker)
      case "+"
        pt = pt2 = "1";
      case "o"
        pt = "6";
        pt2 = "7";
      case "*"
        pt = pt2 = "3";
      case "."
        pt = "6";
        pt2 = "7";
        if (isfield (obj, "markerfacecolor")
            || strcmp (obj.markerfacecolor, "none"))
          obj.markerfacecolor = "auto";
        endif
        if (isfield (obj, "markersize"))
          obj.markersize /= 3;
        else
          obj.markersize = 5;
        endif
      case "x"
        pt = pt2 = "2";
      case {"square", "s"}
        pt = "4";
        pt2 = "5";
      case {"diamond", "d"}
        pt = "12";
        pt2 = "13";
      case "^"
        pt = "8";
        pt2 = "9";
      case "v"
        pt = "10";
        pt2 = "11";
      case ">"
        ## FIXME: Should be triangle pointing right, use triangle pointing up
        pt = "8";
        pt2 = "9";
      case "<"
        ## FIXME: Should be triangle pointing left, use triangle pointing down
        pt = "10";
        pt2 = "11";
      case {"pentagram", "p"}
        ## FIXME: Should be pentagram, using pentagon
        pt = "14";
        pt2 = "15";
      case {"hexagram", "h"}
        ## FIXME: Should be 6 pt start, using "*" instead
        pt = pt2 = "3";
      case "none"
        pt = pt2 = "";
      otherwise
        pt = pt2 = "";
    endswitch
  else
    pt = pt2 = "";
  endif
endfunction

function __gnuplot_write_data__ (plot_stream, data, nd, parametric, cdata)

  ## DATA is already transposed.

  ## FIXME: this may need to be converted to C++ for speed.

  ## Convert NA elements to normal NaN values because fprintf writes
  ## "NA" and that confuses gnuplot.
  idx = find (isna (data));
  if (any (idx))
    data(idx) = NaN;
  endif

  if (nd == 2)
    fwrite (plot_stream, data, "float64");
  elseif (nd == 3)
    if (parametric)
      fwrite (plot_stream, data, "float64");
    else
      nr = rows (data);
      if (cdata)
        for j = 1:4:nr
          fwrite (plot_stream, data(j:j+3,:), "float64");
        endfor
      else
        for j = 1:3:nr
          fwrite (plot_stream, data(j:j+2,:), "float64");
        endfor
      endif
    endif
  endif

endfunction

function do_tics (obj, plot_stream, ymirror, mono, gnuplot_term)

  obj.xticklabel = ticklabel_to_cell (obj.xticklabel);
  obj.yticklabel = ticklabel_to_cell (obj.yticklabel);
  obj.zticklabel = ticklabel_to_cell (obj.zticklabel);

  if (strcmp (obj.xminorgrid, "on"))
    obj.xminortick = "on";
  endif
  if (strcmp (obj.yminorgrid, "on"))
    obj.yminortick = "on";
  endif
  if (strcmp (obj.zminorgrid, "on"))
    obj.zminortick = "on";
  endif

  [fontname, fontsize] = get_fontname_and_size (obj);
  fontspec = create_fontspec (fontname, fontsize, gnuplot_term);

  ## A Gnuplot tic scale of 69 is equivalent to Octave's 0.5.
  ticklength = sprintf ("scale %4.1f", (69/0.5)*obj.ticklength(1));

  if (strcmpi (obj.xaxislocation, "top"))
    do_tics_1 (obj.xtickmode, obj.xtick, obj.xminortick, obj.xticklabelmode,
               obj.xticklabel, obj.xcolor, "x2", plot_stream, true, mono,
               "border", obj.tickdir, ticklength, fontname, fontspec,
               obj.interpreter, obj.xscale, obj.xsgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.xticklabelmode, obj.xticklabel,
               obj.xcolor, "x", plot_stream, true, mono, "border",
               "", "", fontname, fontspec, obj.interpreter, obj.xscale,
               obj.xsgn, gnuplot_term);
  elseif (strcmpi (obj.xaxislocation, "zero"))
    do_tics_1 (obj.xtickmode, obj.xtick, obj.xminortick, obj.xticklabelmode,
               obj.xticklabel, obj.xcolor, "x", plot_stream, true, mono,
               "axis", obj.tickdir, ticklength, fontname, fontspec,
               obj.interpreter, obj.xscale, obj.xsgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.xticklabelmode, obj.xticklabel,
               obj.xcolor, "x2", plot_stream, true, mono, "axis",
               "", "", fontname, fontspec, obj.interpreter, obj.xscale,
               obj.xsgn, gnuplot_term);
  else
    do_tics_1 (obj.xtickmode, obj.xtick, obj.xminortick, obj.xticklabelmode,
               obj.xticklabel, obj.xcolor, "x", plot_stream, true, mono,
               "border", obj.tickdir, ticklength, fontname, fontspec,
               obj.interpreter, obj.xscale, obj.xsgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.xticklabelmode, obj.xticklabel,
               obj.xcolor, "x2", plot_stream, true, mono, "border",
               "", "", fontname, fontspec, obj.interpreter, obj.xscale,
               obj.xsgn, gnuplot_term);
  endif
  if (strcmpi (obj.yaxislocation, "right"))
    do_tics_1 (obj.ytickmode, obj.ytick, obj.yminortick, obj.yticklabelmode,
               obj.yticklabel, obj.ycolor, "y2", plot_stream, ymirror, mono,
               "border", obj.tickdir, ticklength, fontname, fontspec,
               obj.interpreter, obj.yscale, obj.ysgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.yticklabelmode, obj.yticklabel,
               obj.ycolor, "y", plot_stream, ymirror, mono, "border",
               "", "", fontname, fontspec, obj.interpreter, obj.yscale,
               obj.ysgn, gnuplot_term);
  elseif (strcmpi (obj.yaxislocation, "zero"))
    do_tics_1 (obj.ytickmode, obj.ytick, obj.yminortick, obj.yticklabelmode,
               obj.yticklabel, obj.ycolor, "y", plot_stream, ymirror, mono,
               "axis", obj.tickdir, ticklength, fontname, fontspec,
               obj.interpreter, obj.yscale, obj.ysgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.yticklabelmode, obj.yticklabel,
               obj.ycolor, "y2", plot_stream, ymirror, mono, "axis",
               "", "", fontname, fontspec, obj.interpreter, obj.yscale,
               obj.ysgn, gnuplot_term);
  else
    do_tics_1 (obj.ytickmode, obj.ytick, obj.yminortick, obj.yticklabelmode,
               obj.yticklabel, obj.ycolor, "y", plot_stream, ymirror, mono,
               "border", obj.tickdir, ticklength, fontname, fontspec,
               obj.interpreter, obj.yscale, obj.ysgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.yticklabelmode, obj.yticklabel,
               obj.ycolor, "y2", plot_stream, ymirror, mono, "border",
               "", "", fontname, fontspec, obj.interpreter, obj.yscale,
               obj.ysgn, gnuplot_term);
  endif
  do_tics_1 (obj.ztickmode, obj.ztick, obj.zminortick, obj.zticklabelmode,
             obj.zticklabel, obj.zcolor, "z", plot_stream, true, mono,
             "border", obj.tickdir, ticklength, fontname, fontspec,
             obj.interpreter, obj.zscale, obj.zsgn, gnuplot_term);
endfunction

function do_tics_1 (ticmode, tics, mtics, labelmode, labels, color, ax,
                    plot_stream, mirror, mono, axispos, tickdir, ticklength,
                    fontname, fontspec, interpreter, scale, sgn, gnuplot_term)
  persistent warned_latex = false;

  ## Avoid emitting anything if the tics are empty, because this undoes the
  ## effect of the previous unset xtics and thereby adds back in the tics.
  if (isempty (tics))
    return;
  endif

  if (mirror)
    mirror = "mirror";
  else
    mirror = "nomirror";
  endif
  if (strcmpi (interpreter, "tex"))
    for n = 1 : numel (labels)
      labels{n} = __tex2enhanced__ (labels{n}, fontname, false, false);
    endfor
  elseif (strcmpi (interpreter, "latex"))
    if (! warned_latex)
      warning ("latex markup not supported for tick marks");
      warned_latex = true;
    endif
  endif
  if (strcmp (scale, "log"))
    num_mtics = 10;
    if (any (strcmp (gnuplot_term, {"tikz", "pstex", "pslatex", "epslatex"})))
      fmt = "$10^{%T}$";
    else
      fmt = "10^{%T}";
    endif
    if (sgn < 0)
      fmt = strcat ("-", fmt);
    endif
  else
    fmt = "%g";
    num_mtics = 5;
  endif
  colorspec = get_text_colorspec (color, mono);
  fprintf (plot_stream, "set format %s \"%s\";\n", ax, fmt);
  if (strcmpi (ticmode, "manual"))
    if (isempty (tics))
      fprintf (plot_stream, "unset %stics;\nunset m%stics;\n", ax, ax);
      return
    endif
    fprintf (plot_stream, "set %stics %s %s %s %s (", ax, tickdir,
             ticklength, axispos, mirror);
    fprintf (plot_stream, " %.15g,", tics(1:end-1));
    fprintf (plot_stream, " %.15g) %s;\n", tics(end), fontspec);
  else
    fprintf (plot_stream, "set %stics %s %s %s %s %s %s;\n", ax,
             tickdir, ticklength, axispos, mirror, colorspec, fontspec);
  endif
  if (strcmpi (labelmode, "manual"))
    k = 1;
    ntics = numel (tics);
    nlabels = numel (labels);
    fprintf (plot_stream, "set %stics add %s %s %s %s (", ax,
             tickdir, ticklength, axispos, mirror);
    labels = strrep (labels, "%", "%%");
    for i = 1:ntics
      fprintf (plot_stream, " \"%s\" %.15g", labels{k++}, tics(i));
      if (i < ntics)
        fputs (plot_stream, ", ");
      endif
      if (k > nlabels)
        k = 1;
      endif
    endfor
    fprintf (plot_stream, ") %s %s;\n", colorspec, fontspec);
  endif
  if (strcmp (mtics, "on"))
    fprintf (plot_stream, "set m%stics %d;\n", ax, num_mtics);
  else
    fprintf (plot_stream, "unset m%stics;\n", ax);
  endif
endfunction

function ticklabel = ticklabel_to_cell (ticklabel)
  if (ischar (ticklabel))
    ticklabel = cellstr (ticklabel);
  elseif (iscellstr (ticklabel))
    ticklabel = ticklabel;
  else
    error ("__go_draw_axes__: unsupported type of ticklabel");
  endif
endfunction

function colorspec = get_text_colorspec (color, mono)
  if (mono)
    colorspec = "";
  else
    colorspec = sprintf ("textcolor rgb \"#%02x%02x%02x\"",
                         round (255*color));
  endif
endfunction

function [f, s, fnt, it, bld] = get_fontname_and_size (t)
  if (isempty (t.fontname) || strcmp (t.fontname, "*"))
    fnt = "{}";
  else
    fnt = t.fontname;
  endif
  f = fnt;
  it = false;
  bld = false;
  if (! isempty (t.fontweight) && strcmpi (t.fontweight, "bold"))
    if (! isempty (t.fontangle)
        && (strcmpi (t.fontangle, "italic")
            || strcmpi (t.fontangle, "oblique")))
      f = [f "-bolditalic"];
      it = true;
      bld = true;
    else
      f = [f "-bold"];
      bld = true;
    endif
  elseif (! isempty (t.fontangle)
          && (strcmpi (t.fontangle, "italic")
              || strcmpi (t.fontangle, "oblique")))
    f = [f "-italic"];
    it = true;
  endif
  if (isempty (t.fontsize))
    s = 10;
  else
    s = t.fontsize;
  endif
endfunction

function [str, f, s] = __maybe_munge_text__ (enhanced, obj, fld)

  persistent warned_latex = false;

  if (strcmp (fld, "string"))
    [f, s, fnt, it, bld] = get_fontname_and_size (obj);
  else
    f = "Helvetica";
    s = 10;
    fnt = f;
    it = false;
    bld = false;
  endif

  ## The text object may be multiline, and may be of any class
  str = getfield (obj, fld);
  if (ischar (str) && rows (str) > 1)
    str = cellstr (str);
  elseif (isnumeric (str))
    str = cellstr (num2str (str(:)));
  endif
  if (iscellstr (str))
    for n = 1:numel (str)
      if (isnumeric (str{n}))
        str{n} = num2str (str{n});
      endif
    endfor
    str = sprintf ("%s\n", str{:})(1:end-1);
  endif

  if (enhanced)
    str = regexprep (str, '(?<!\\)@', '\@');
  endif

  if (enhanced)
    if (strcmpi (obj.interpreter, "tex"))
      if (iscellstr (str))
        for n = 1:numel (str)
          str{n} = __tex2enhanced__ (str{n}, fnt, it, bld);
        endfor
      else
        str = __tex2enhanced__ (str, fnt, it, bld);
      endif
    elseif (strcmpi (obj.interpreter, "latex"))
      if (! warned_latex)
        warning ("latex markup not supported for text objects");
        warned_latex = true;
      endif
    endif
  endif
endfunction

function str = __tex2enhanced__ (str, fnt, it, bld)
  persistent sym = __setup_sym_table__ ();
  persistent flds = fieldnames (sym);

  [s, e, m] = regexp (str, "\\\\([a-zA-Z]+|0)", "start", "end", "matches");

  for i = length (s) : -1 : 1
    ## special case for "\0"  and replace with empty set "{/Symbol \306}'
    if (strncmp (m{i}, '\0', 2))
      str = [str(1:s(i) - 1) '{/Symbol \306}' str(s(i) + 2:end)];
    else
      f = m{i}(2:end);
      if (isfield (sym, f))
        g = getfield (sym, f);
        ## FIXME: The symbol font doesn't seem to support bold or italic
        ##if (bld)
        ##  if (it)
        ##    g = regexprep (g, '/Symbol', '/Symbol-bolditalic');
        ##  else
        ##    g = regexprep (g, '/Symbol', '/Symbol-bold');
        ##  endif
        ##elseif (it)
        ##  g = regexprep (g, '/Symbol', '/Symbol-italic');
        ##endif
        str = [str(1:s(i) - 1) g str(e(i) + 1:end)];
      elseif (strncmp (f, "rm", 2))
        bld = false;
        it = false;
        str = [str(1:s(i) - 1) '/' fnt ' ' str(s(i) + 3:end)];
      elseif (strncmp (f, "it", 2) || strncmp (f, "sl", 2))
        it = true;
        if (bld)
          str = [str(1:s(i) - 1) '/' fnt '-bolditalic ' str(s(i) + 3:end)];
        else
          str = [str(1:s(i) - 1) '/' fnt '-italic ' str(s(i) + 3:end)];
        endif
      elseif (strncmp (f, "bf", 2))
        bld = true;
        if (it)
          str = [str(1:s(i) - 1) '/' fnt '-bolditalic ' str(s(i) + 3:end)];
        else
          str = [str(1:s(i) - 1) '/' fnt '-bold ' str(s(i) + 3:end)];
        endif
      elseif (strcmpi (f, "color"))
        ## FIXME: Ignore \color but remove trailing {} block as well
        d = strfind (str(e(i) + 1:end),'}');
        if (isempty (d))
          warning ('syntax error in \color argument');
        else
          str = [str(1:s(i) - 1) str(e(i) + d + 1:end)];
        endif
      elseif (strcmpi (f, "fontname"))
        b1 = strfind (str(e(i) + 1:end),'{');
        b2 = strfind (str(e(i) + 1:end),'}');
        if (isempty (b1) || isempty (b2))
          warning ('syntax error in \fontname argument');
        else
          str = [str(1:s(i) - 1), '/', str(e(i)+b1(1) + 1:e(i)+b2(1)-1), ...
                 '{}', str(e(i) + b2(1) + 1:end)];
        endif
      elseif (strcmpi (f, "fontsize"))
        b1 = strfind (str(e(i) + 1:end),'{');
        b2 = strfind (str(e(i) + 1:end),'}');
        if (isempty (b1) || isempty (b2))
          warning ('syntax error in \fontname argument');
        else
          str = [str(1:s(i) - 1), '/=', str(e(i)+b1(1) + 1:e(i)+b2(1)-1), ...
                 '{}', str(e(i) + b2(1) + 1:end)];
        endif
      else
        ## Last desperate attempt to treat the symbol. Look for things
        ## like \pix, that should be translated to the symbol Pi and x
        for j = 1 : length (flds)
          if (strncmp (flds{j}, f, length (flds{j})))
            g = getfield (sym, flds{j});
            ## FIXME: The symbol font doesn't seem to support bold or italic
            ##if (bld)
            ##  if (it)
            ##    g = regexprep (g, '/Symbol', '/Symbol-bolditalic');
            ##  else
            ##    g = regexprep (g, '/Symbol', '/Symbol-bold');
            ##  endif
            ##elseif (it)
            ##  g = regexprep (g, '/Symbol', '/Symbol-italic');
            ##endif
            str = [str(1:s(i) - 1) g str(s(i) + length (flds{j}) + 1:end)];
            break;
          endif
        endfor
      endif
    endif
  endfor

  ## Prepend @ to things like _0^x or _{-100}^{100} for alignment.
  ## But need to put the shorter of the two arguments first.
  ## Careful of nested {} and unprinted characters when defining
  ## shortest..  Don't have to worry about things like ^\theta as they
  ## are already converted to ^{/Symbol q}.

  ## FIXME: This is a mess... Is it worth it just for a "@" character?

  [s, m] = regexp (str,'[_\^]','start','matches');
  i = 1;
  p = 0;
  while (i < length (s))
    if (i < length (s))
      if (str(s(i) + p + 1) == "{")
        s1 = strfind (str(s(i) + p + 2:end),'{');
        si = 1;
        l1 = strfind (str(s(i) + p + 1:end),'}');
        li = 1;
        while (li <= length (l1) && si <= length (s1))
          if (l1(li) < s1(si))
            if (li == si)
              break;
            endif
            li++;
          else
            si++;
          endif
        endwhile
        l1 = l1 (min (length (l1), si));
        if (s(i) + l1 + 1 == s(i+1))
          if (str(s(i + 1) + p + 1) == "{")
            s2 = strfind (str(s(i + 1) + p + 2:end),'{');
            si = 1;
            l2 = strfind (str(s(i + 1) + p + 1:end),'}');
            li = 1;
            while (li <= length (l2) && si <= length (s2))
              if (l2(li) < s2(si))
                if (li == si)
                  break;
                endif
                li++;
              else
                si++;
              endif
            endwhile
            l2 = l2 (min (length (l2), si));
            if (length_string (str(s(i)+p+2:s(i)+p+l1-1)) <=
                length_string (str(s(i+1)+p+2:s(i+1)+p+l2-1)))
              ## Shortest already first!
              str = [str(1:s(i)+p-1) "@" str(s(i)+p:end)];
            else
              ## Have to swap sub/super-script to get shortest first.
              str = [str(1:s(i)+p-1), "@", str(s(i+1)+p:s(i+1)+p+l2), ...
                     str(s(i)+p:s(i)+p+l1), str(s(i+1)+p+l2+1:end)];
            endif
          else
            ## Have to swap sub/super-script to get shortest first.
            str = [str(1:s(i)+p-1), "@", str(s(i+1)+p:s(i+1)+p+1), ...
                   str(s(i)+p:s(i)+p+l1), str(s(i+1)+p+2:end)];
          endif
          i += 2;
          p ++;
        else
          i++;
        endif
      else
        if (s(i+1) == s(i) + 2)
          ## Shortest already first!
          str = [str(1:s(i)+p-1) "@" str(s(i)+p:end)];
          p ++;
          i += 2;
        else
          i ++;
        endif
      endif
    else
      i ++;
    endif
  endwhile

endfunction

function l = length_string (s)
  l = length (s) - length (strfind (s,'{')) - length (strfind (s,'}'));
  m = regexp (s, '/([\w-]+|[\w-]+=\d+)', 'matches');
  if (! isempty (m))
    l = l - sum (cellfun ("length", m));
  endif
endfunction

function sym = __setup_sym_table__ ()
  ## Setup the translation table for TeX to gnuplot enhanced mode.
  sym.forall = '{/Symbol \042}';
  sym.exists = '{/Symbol \044}';
  sym.ni = '{/Symbol \047}';
  sym.cong = '{/Symbol \100}';
  sym.Delta = '{/Symbol D}';
  sym.Phi = '{/Symbol F}';
  sym.Gamma = '{/Symbol G}';
  sym.vartheta = '{/Symbol J}';
  sym.Lambda = '{/Symbol L}';
  sym.Pi = '{/Symbol P}';
  sym.Theta = '{/Symbol Q}';
  sym.Sigma = '{/Symbol S}';
  sym.varsigma = '{/Symbol V}';
  sym.Omega = '{/Symbol W}';
  sym.Xi = '{/Symbol X}';
  sym.Psi = '{/Symbol Y}';
  sym.perp = '{/Symbol \136}';
  sym.alpha = '{/Symbol a}';
  sym.beta = '{/Symbol b}';
  sym.chi = '{/Symbol c}';
  sym.delta = '{/Symbol d}';
  sym.epsilon = '{/Symbol e}';
  sym.phi = '{/Symbol f}';
  sym.gamma = '{/Symbol g}';
  sym.eta = '{/Symbol h}';
  sym.iota = '{/Symbol i}';
  sym.varphi = '{/Symbol j}';              # Not in OpenGL
  sym.kappa = '{/Symbol k}';
  sym.lambda = '{/Symbol l}';
  sym.mu = '{/Symbol m}';
  sym.nu = '{/Symbol n}';
  sym.o = '{/Symbol o}';
  sym.pi = '{/Symbol p}';
  sym.theta = '{/Symbol q}';
  sym.rho = '{/Symbol r}';
  sym.sigma = '{/Symbol s}';
  sym.tau = '{/Symbol t}';
  sym.upsilon = '{/Symbol u}';
  sym.varpi = '{/Symbol v}';
  sym.omega = '{/Symbol w}';
  sym.xi = '{/Symbol x}';
  sym.psi = '{/Symbol y}';
  sym.zeta = '{/Symbol z}';
  sym.sim = '{/Symbol \176}';
  sym.Upsilon = '{/Symbol \241}';
  sym.prime = '{/Symbol \242}';
  sym.leq = '{/Symbol \243}';
  sym.infty = '{/Symbol \245}';
  sym.clubsuit = '{/Symbol \247}';
  sym.diamondsuit = '{/Symbol \250}';
  sym.heartsuit = '{/Symbol \251}';
  sym.spadesuit = '{/Symbol \252}';
  sym.leftrightarrow = '{/Symbol \253}';
  sym.leftarrow = '{/Symbol \254}';
  sym.uparrow = '{/Symbol \255}';
  sym.rightarrow = '{/Symbol \256}';
  sym.downarrow = '{/Symbol \257}';
  sym.circ = '{/Symbol \260}';         # degree symbol, not circ as in FLTK
  sym.deg = '{/Symbol \260}';
  sym.ast = '{/Symbol *}';
  sym.pm = '{/Symbol \261}';
  sym.geq = '{/Symbol \263}';
  sym.times = '{/Symbol \264}';
  sym.propto = '{/Symbol \265}';
  sym.partial = '{/Symbol \266}';
  sym.bullet = '{/Symbol \267}';
  sym.div = '{/Symbol \270}';
  sym.neq = '{/Symbol \271}';
  sym.equiv = '{/Symbol \272}';
  sym.approx = '{/Symbol \273}';
  sym.ldots = '{/Symbol \274}';
  sym.mid = '{/Symbol \275}';
  sym.aleph = '{/Symbol \300}';
  sym.Im = '{/Symbol \301}';
  sym.Re = '{/Symbol \302}';
  sym.wp = '{/Symbol \303}';
  sym.otimes = '{/Symbol \304}';
  sym.oplus = '{/Symbol \305}';
  ## empty set, not circled slash division operator as in FLTK.
  sym.oslash = '{/Symbol \306}';
  sym.cap = '{/Symbol \307}';
  sym.cup = '{/Symbol \310}';
  sym.supset = '{/Symbol \311}';
  sym.supseteq = '{/Symbol \312}';
  sym.subset = '{/Symbol \314}';
  sym.subseteq = '{/Symbol \315}';
  sym.in = '{/Symbol \316}';
  sym.notin = '{/Symbol \317}';            # Not in OpenGL
  sym.angle = '{/Symbol \320}';
  sym.bigtriangledown = '{/Symbol \321}';  # Not in OpenGL
  sym.langle = '{/Symbol \341}';
  sym.rangle = '{/Symbol \361}';
  sym.nabla = '{/Symbol \321}';
  sym.prod = '{/Symbol \325}';             # Not in OpenGL
  sym.surd = '{/Symbol \326}';
  sym.cdot = '{/Symbol \327}';
  sym.neg = '{/Symbol \330}';
  sym.wedge = '{/Symbol \331}';
  sym.vee = '{/Symbol \332}';
  sym.Leftrightarrow = '{/Symbol \333}';   # Not in OpenGL
  sym.Leftarrow = '{/Symbol \334}';
  sym.Uparrow = '{/Symbol \335}';          # Not in OpenGL
  sym.Rightarrow = '{/Symbol \336}';
  sym.Downarrow = '{/Symbol \337}';        # Not in OpenGL
  sym.diamond = '{/Symbol \340}';          # Not in OpenGL
  sym.copyright = '{/Symbol \343}';
  sym.lfloor = '{/Symbol \353}';
  sym.lceil = '{/Symbol \351}';
  sym.rfloor = '{/Symbol \373}';
  sym.rceil = '{/Symbol \371}';
  sym.int = '{/Symbol \362}';
endfunction

function retval = __do_enhanced_option__ (enhanced, obj)
  retval = "";
  if (enhanced)
    if (strcmpi (obj.interpreter, "none"))
      retval = "noenhanced";
    else
      retval = "enhanced";
    endif
  endif
endfunction

function maybe_do_xtick_mirror (plot_stream, axis_obj)
  if (! isempty(axis_obj.xtick))
    fprintf (plot_stream, "unset x2tics; set xtics %s nomirror\n",
                          axis_obj.tickdir);
  endif
endfunction

function maybe_do_x2tick_mirror (plot_stream, axis_obj)
  if (! isempty(axis_obj.xtick))
    fprintf (plot_stream, "unset xtics; set x2tics %s nomirror\n",
                          axis_obj.tickdir);
  endif
endfunction
