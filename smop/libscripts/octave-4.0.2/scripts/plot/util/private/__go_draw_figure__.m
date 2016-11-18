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
## @deftypefn {Function File} {} __go_draw_figure__ (@var{h}, @var{plot_stream}, @var{enhanced}, @var{mono})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function __go_draw_figure__ (h, plot_stream, enhanced, mono)

  htype = get (h, "type");
  if (strcmp (htype, "figure"))
    ## Get complete list of children.
    kids = allchild (h);
    nkids = length (kids);

    if (nkids > 0)
      fputs (plot_stream, "\nreset;\n");
      fputs (plot_stream, "set autoscale keepfix;\n");
      fputs (plot_stream, "set origin 0, 0\n");
      fputs (plot_stream, "set size 1, 1\n");
      bg = get (h, "color");
      if (isnumeric (bg))
        fprintf (plot_stream, "set obj 1 rectangle from screen 0,0 to screen 1,1 behind fc rgb \"#%02x%02x%02x\" fs solid noborder\n", round (255 * bg));
        bg_is_set = true;
      else
        bg_is_set = false;
      endif
      fg_was_set = false;

      for i = nkids:-1:1
        type = get (kids(i), "type");
        switch (type)
          case "axes"
            if (strcmpi (get (kids (i), "tag"), "legend"))
              ## This is so ugly. If there was a way of getting
              ## gnuplot to give us the text extents of strings
              ## then we could get rid of this mess.
              lh = getfield (get (kids(i), "userdata"), "handle");
              if (isscalar (lh))
                ## We have a legend with a single parent. It'll be handled
                ## below as a gnuplot key to the axis it corresponds to
                continue;
              else
                ca = lh(1);
                ## Rely upon listener to convert axes position
                ## to "normalized" units.
                legend_axes_units = get (kids(i), "units");
                legend_axes_position = get (kids(i), "position");
                legend_axes_outerposition = get (kids(i), "outerposition");
                legend_axes_box = get (kids(i), "box");
                legend_axes_ylim = get (kids(i), "ylim");
                orig_axes_units = get (ca, "units");
                hlgnd = get (kids(i));

                unwind_protect
                  set (ca, "units", "normalized");
                  set (kids(i), "units", "normalized", "box", "off",
                       "ylim", [-2, -1], "position", get (ca(1), "position"),
                       "outerposition", get (ca(1), "outerposition"));

                  ## Create a new set of lines with the appropriate
                  ## displaynames, etc
                  toberm = [];
                  hobj = get (kids(i), "children");
                  for j = numel (hobj) : -1 : 1
                    if (! strcmp (get (hobj(j), "type"), "text"))
                      continue;
                    endif
                    displayname = get (hobj(j), "string");
                    ll = [];
                    lm = [];
                    for k = numel (hobj) : -1 : 1
                      if (! strcmp (get (hobj(k), "type"), "line"))
                        continue;
                      endif
                      if (get (hobj(j), "userdata")
                          != get (hobj(k), "userdata"))
                        continue;
                      endif
                      if (! strcmp (get (hobj(k), "linestyle"), "none"))
                        ll = hobj(k);
                      endif
                      if (! strcmp (get (hobj(k), "marker"), "none"))
                        lm = hobj(k);
                      endif
                    endfor

                    if (! isempty (ll))
                      if (! isempty (lm))
                        toberm = [toberm, line("xdata",[0,0],"ydata",[0,0], "color", get(lm,"color"), "linestyle", get(ll,"linestyle"), "marker", get(lm,"marker"), "markeredgecolor", get(lm,"markeredgecolor"), "markerfacecolor", get(lm,"markerfacecolor"), "markersize", get (lm, "markersize"), "displayname", displayname, "parent", kids(i))];
                      else
                        toberm = [toberm, line("xdata",[0,0],"ydata",[0,0], "color", get(ll,"color"), "linestyle", get(ll,"linestyle"), "marker", "none", "displayname", displayname, "parent", kids(i))];
                      endif
                    elseif (! isempty (lm))
                      toberm = [toberm, line("xdata",[0,0],"ydata",[0,0], "color", get(lm,"color"), "linestyle", "none", "marker", get(lm,"marker"), "markeredgecolor", get(lm,"markeredgecolor"), "markerfacecolor", get(lm,"markerfacecolor"), "markersize", get (lm, "markersize"), "displayname", displayname, "parent", kids(i))];
                    endif
                  endfor
                  if (bg_is_set)
                    fprintf (plot_stream, "set border linecolor rgb \"#%02x%02x%02x\"\n", round (255 * (1 - bg)));
                  endif
                  __go_draw_axes__ (kids(i), plot_stream, enhanced, mono,
                                    bg_is_set, false, hlgnd);
                unwind_protect_cleanup
                  ## Return axes "units" and "position" back to
                  ## their original values.
                  set (ca, "units", orig_axes_units);
                  set (kids(i), "units", legend_axes_units,
                       "box", legend_axes_box,
                       "ylim", legend_axes_ylim,
                       "position", legend_axes_position,
                       "outerposition", legend_axes_outerposition);
                  delete (toberm);
                  bg_is_set = false;
                end_unwind_protect
              endif
            else
              ## Rely upon listener to convert axes position
              ## to "normalized" units.
              orig_axes_units = get (kids(i), "units");
              orig_axes_position = get (kids(i), "position");
              unwind_protect
                set (kids(i), "units", "normalized");
                fg = get (kids(i), "color");
                if (isnumeric (fg) && strcmp (get (kids(i), "visible"), "on"))
                  fprintf (plot_stream, "set obj 2 rectangle from graph 0,0 to graph 1,1 behind fc rgb \"#%02x%02x%02x\" fs solid noborder\n", round (255 * fg));
                  fg_is_set = true;
                  fg_was_set = true;
                elseif (fg_was_set)
                  fprintf (plot_stream, "unset obj 2\n");
                  fg_is_set = false;
                  fg_was_set = false;
                else
                  fg_is_set = false;
                endif
                if (bg_is_set)
                  fprintf (plot_stream, "set border linecolor rgb \"#%02x%02x%02x\"\n", round (255 * (1 - bg)));
                endif
                ## Find if this axes has an associated legend axes and pass it
                ## to __go_draw_axes__
                hlegend = [];
                fkids = get (h, "children");
                for j = 1 : numel (fkids)
                  if (ishandle (fkids (j))
                      && strcmp (get (fkids (j), "type"), "axes")
                      && (strcmp (get (fkids (j), "tag"), "legend")))
                    udata = get (fkids (j), "userdata");
                    if (isscalar (udata.handle)
                        && ! isempty (intersect (udata.handle, kids (i))))
                      hlegend = get (fkids (j));
                      break;
                    endif
                  endif
                endfor
                __go_draw_axes__ (kids(i), plot_stream, enhanced, mono,
                                  bg_is_set, fg_is_set, hlegend);
              unwind_protect_cleanup
                ## Return axes "units" and "position" back to
                ## their original values.
                set (kids(i), "units", orig_axes_units);
                set (kids(i), "position", orig_axes_position);
                bg_is_set = false;
                fg_is_set = false;
              end_unwind_protect
            endif
          case "uimenu"
            ## ignore uimenu objects
            kids(i) = [];
          otherwise
            error ("__go_draw_figure__: unknown object class, %s", type);
        endswitch
      endfor
      if (isempty (kids))
        fputs (plot_stream, "\nreset; clear;\n");
        fflush (plot_stream);
      else
        fputs (plot_stream, "\nunset multiplot;\n");
      endif
    else
      fputs (plot_stream, "\nreset; clear;\n");
      fflush (plot_stream);
    endif
  else
    error ("__go_draw_figure__: expecting figure object, found '%s'",
           htype);
  endif

endfunction

