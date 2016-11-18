## Copyright (C) 2010-2015 David Bateman
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
## @deftypefn  {Function File} {} legend (@var{str1}, @var{str2}, @dots{})
## @deftypefnx {Function File} {} legend (@var{matstr})
## @deftypefnx {Function File} {} legend (@var{cellstr})
## @deftypefnx {Function File} {} legend (@dots{}, "location", @var{pos})
## @deftypefnx {Function File} {} legend (@dots{}, "orientation", @var{orient})
## @deftypefnx {Function File} {} legend (@var{hax}, @dots{})
## @deftypefnx {Function File} {} legend (@var{hobjs}, @dots{})
## @deftypefnx {Function File} {} legend (@var{hax}, @var{hobjs}, @dots{})
## @deftypefnx {Function File} {} legend ("@var{option}")
## @deftypefnx {Function File} {[@var{hleg}, @var{hleg_obj}, @var{hplot}, @var{labels}] =} legend (@dots{})
##
## Display a legend for the current axes using the specified strings as labels.
##
## Legend entries may be specified as individual character string arguments,
## a character array, or a cell array of character strings.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.  If the handles,
## @var{hobjs}, are not specified then the legend's strings will be associated
## with the axes' descendants.  @code{legend} works on line graphs,
## bar graphs, etc.  A plot must exist before legend is called.
##
## The optional parameter @var{pos} specifies the location of the legend
## as follows:
##
## @multitable @columnfractions 0.06 0.14 0.80
## @headitem @tab pos @tab location of the legend
## @item @tab north @tab center top
## @item @tab south @tab center bottom
## @item @tab east @tab right center
## @item @tab west @tab left center
## @item @tab northeast @tab right top (default)
## @item @tab northwest @tab left top
## @item @tab southeast @tab right bottom
## @item @tab southwest @tab left bottom
## @item
## @item @tab outside @tab can be appended to any location string
## @end multitable
##
## The optional parameter @var{orient} determines if the key elements are
## placed vertically or horizontally.  The allowed values are
## @qcode{"vertical"} (default) or @qcode{"horizontal"}.
##
## The following customizations are available using @var{option}:
##
## @table @asis
## @item @qcode{"show"}
##   Show legend on the plot
##
## @item @qcode{"hide"}
##   Hide legend on the plot
##
## @item @qcode{"toggle"}
##   Toggles between @qcode{"hide"} and @qcode{"show"}
##
## @item @qcode{"boxon"}
##   Show a box around legend (default)
##
## @item @qcode{"boxoff"}
##   Hide the box around legend
##
## @item @qcode{"right"}
##   Place label text to the right of the keys (default)
##
## @item @qcode{"left"}
##   Place label text to the left of the keys
##
## @item @qcode{"off"}
##   Delete the legend object
## @end table
##
## The optional output values are
##
## @table @var
## @item hleg
##   The graphics handle of the legend object.
##
## @item hleg_obj
##   Graphics handles to the text and line objects which make up the legend.
##
## @item hplot
##   Graphics handles to the plot objects which were used in making the legend.
##
## @item labels
##   A cell array of strings of the labels in the legend.
## @end table
##
## The legend label text is either provided in the call to @code{legend} or
## is taken from the DisplayName property of graphics objects.  If no
## labels or DisplayNames are available, then the label text is simply
## @qcode{"data1"}, @qcode{"data2"}, @dots{}, @nospell{@qcode{"dataN"}}.
##
## Implementation Note: A legend is implemented as an additional axes object
## of the current figure with the @qcode{"tag"} set to @qcode{"legend"}.
## Properties of the legend object may be manipulated directly by using
## @code{set}.
## @end deftypefn

function [hleg, hleg_obj, hplot, labels] = legend (varargin)

  if (nargin > 0
      && (! ishandle (varargin{1})
          || (strcmp (get (varargin{1}, "type"), "axes")
              && ! strcmp (get (varargin{1}, "tag"), "legend"))))
    [ca, varargin, nargin] = __plt_get_axis_arg__ ("legend", varargin{:});
    if (isempty (ca))
      ca = gca ();
    endif
    fig = ancestor (ca, "figure");
  else
    fig = get (0, "currentfigure");
    if (isempty (fig))
      fig = gcf ();
    endif
    ca = gca ();
  endif

  ## Special handling for plotyy which has two axes objects
  if (ishandle (ca) && isprop (ca, "__plotyy_axes__"))
    plty = get (ca, "__plotyy_axes__");
    if (isscalar (plty) && ishandle (plty))
      ca = [ca, plty];
    elseif (iscell (plty))
      ca = [ca, plty{:}];
    elseif (all (ishandle (plty)))
      ca = [ca, plty(:).'];
    else
      error ("legend.m: This should not happen. File a bug report.");
    endif
    ## Remove duplicates while preserving order
    [~, n] = unique (ca);
    ca = ca(sort (n));
  endif

  if (nargin > 0 && all (ishandle (varargin{1})))
    kids = flipud (varargin{1}(:));
    varargin(1) = [];
  else
    kids = ca;
    kids(strcmp (get (ca, "tag"), "legend")) = [];
    if (isscalar (kids))
      kids = get (kids, "children")(:);
    else
      kids = flipud (vertcat (get (kids, "children"){:}));
    endif
  endif
  nargs = numel (varargin);
  nkids = numel (kids);

  ## Find any existing legend object on figure
  hlegend = [];
  fkids = get (fig, "children");
  for i = 1 : numel (fkids)
    if (   strcmp (get (fkids(i), "type"), "axes")
        && strcmp (get (fkids(i), "tag"), "legend"))
      udata = get (fkids(i), "userdata");
      if (any (ismember (udata.handle, ca)))
        hlegend = fkids(i);
        break;
      endif
    endif
  endfor

  orientation = "default";
  location = "default";
  show = "create";
  textpos = "default";
  box = "default";
  delete_leg = false;
  find_leg_hdl = (nargs == 0);

  ## Process old way of specifying location with a number rather than a string.
  if (nargs > 0)
    pos = varargin{nargs};
    if (isnumeric (pos) && isscalar (pos) && pos == fix (pos))
      if (pos >= -1 && pos <= 4)
        location = [{"northeastoutside", "best", "northeast",
                     "northwest", "southwest", "southeast"}] {pos + 2};
        nargs--;
      else
        error ("legend: invalid location specified");
      endif
    endif
  endif

  ## Find location and orientation property/value pairs
  while (nargs > 1)
    pos = varargin{nargs-1};
    str = varargin{nargs};
    if (strcmpi (pos, "location") && ischar (str))
      location = lower (str);
      nargs -= 2;
    elseif (strcmpi (pos, "orientation") && ischar (str))
      orientation = lower (str);
      nargs -= 2;
    else
      break;
    endif
  endwhile

  ## Validate the orientation
  switch (orientation)
    case {"vertical", "horizontal", "default"}
      ## These are all accepted orientations.
    otherwise
      error ("legend: unrecognized legend orientation");
  endswitch

  ## Validate the location type
  outside = false;
  inout = strfind (location, "outside");
  if (! isempty (inout))
    outside = true;
    location = location(1:inout-1);
  else
    outside = false;
  endif

  switch (location)
    case {"north", "south", "east", "west", "northeast", "northwest", ...
          "southeast", "southwest", "default"}
    case "best"
      warning ("legend: 'best' not yet implemented for location specifier\n");
      location = "northeast";
    otherwise
      error ("legend: unrecognized legend location");
  endswitch

  if (nargs == 1)
    arg = varargin{1};
    if (ischar (arg))
      if (rows (arg) == 1)
        str = tolower (strtrim (arg));
        switch (str)
          case "off"
            delete_leg = true;
          case "hide"
            show = "off";
            nargs--;
          case "show"
            if (! isempty (hlegend))
              show = "on";
            else
              show = "create";
              textpos = "right";
            endif
            nargs--;
          case "toggle"
            if (isempty (hlegend))
              show = "create";
              textpos = "right";
            elseif (strcmp (get (hlegend, "visible"), "off"))
              show = "on";
            else
              show = "off";
            endif
            nargs--;
          case "boxon"
            box = "on";
            nargs--;
          case "boxoff"
            box = "off";
            nargs--;
          case "left"
            textpos = "left";
            nargs--;
          case "right"
            textpos = "right";
            nargs--;
        endswitch
      else
        ## Character matrix of labels
        varargin = cellstr (arg);
        nargs = numel (varargin);
      endif
    elseif (iscellstr (arg))
      ## Cell array of labels
      varargin = arg;
      nargs = numel (varargin);
    else
      error ("legend: expecting argument to be a character string");
    endif
  elseif (nargs > 1 && iscellstr (varargin{1}))
    ## Cell array of labels followed by property/value pairs
    varargin = {varargin{1}{:}, varargin{2:end}};
    nargs = numel (varargin);
  endif

  have_labels = (nargs > 0);
  hobjects = [];
  hplots = [];
  text_strings = {};

  if (delete_leg)
    delete (hlegend);
    hlegend = [];
  elseif (find_leg_hdl)
    ## Don't change anything about legend.
    ## hleg output will be assigned hlegend value at end of function.
  elseif (strcmp (show, "off"))
    if (! isempty (hlegend))
      set (findobj (hlegend), "visible", "off");
      hlegend = [];
    endif
  elseif (strcmp (show, "on"))
    if (! isempty (hlegend))
      set (findobj (hlegend), "visible", "on");
      ## NOTE: Matlab sets both "visible" and "box" to "on"
      set (hlegend, "visible", get (hlegend, "box"));
    endif
  elseif (strcmp (box, "on"))
    if (! isempty (hlegend))
      set (hlegend, "box", "on", "visible", "on");
    endif
  elseif (strcmp (box, "off"))
    if (! isempty (hlegend))
      set (hlegend, "box", "off", "visible", "off");
    endif
  elseif (! have_labels && ! isempty (hlegend)
          && ! (strcmp (location, "default")
                && strcmp (orientation, "default")))
    ## Changing location or orientation of existing legend
    if (strcmp (location, "default"))
      set (hlegend, "orientation", orientation);
    elseif (strcmp (orientation, "default"))
      if (outside)
        set (hlegend, "location", [location "outside"]);
      else
        set (hlegend, "location", location);
      endif
    else
      if (outside)
        set (hlegend, "location", [location "outside"],
                      "orientation", orientation);
      else
        set (hlegend, "location", location,
                      "orientation", orientation);
      endif
    endif
  else
    ## Create new legend
    hobjects = [];
    hplots = [];
    text_strings = {};

    if (have_labels)
      ## Check for valid data that can be labeled.
      have_data = false;
      have_dname = false;
      for k = 1 : nkids
        typ = get (kids(k), "type");
        if (any (strcmp (typ, {"line", "patch", "surface", "hggroup"})))
          have_data = true;
          break;
        endif
      endfor

      if (! have_data)
        warning ("legend: plot data is empty; setting key labels has no effect");
      endif
    else
      ## No labels.  Search for DisplayName property.
      have_dname = false;
      for k = 1 : nkids
        hkid = kids(k);
        typ = get (hkid, "type");
        if (any (strcmp (typ, {"line", "patch", "surface"})))
          if (! isempty (get (hkid, "displayname")))
            have_dname = true;
            break;
          endif
        elseif (strcmp (typ, "hggroup"))
          hgkids = get (hkid, "children");
          for j = 1 : length (hgkids)
            try
              dname = get (hgkids(j), "DisplayName");
              if (! isempty (dname))
                have_dname = true;
                break;  # break from j-loop over hgkids
              endif
            end_try_catch
          endfor
          if (have_dname)
            break;  # break from k loop over nkids
          endif
        endif  # elseif hggroup
      endfor   # for loop k = 1 : nkids
    endif      # else branch of if (have_labels)

    if (have_labels || ! have_dname)
      k = nkids;
      if (! have_labels)
        varargin = arrayfun (@(x) sprintf ("data%d", x), [1:nkids]',
                             "uniformoutput", false);
        have_labels = true;
        nargs = nkids;
      endif
      for i = 1 : nargs
        arg = varargin{i};
        if (ischar (arg))
          typ = get (kids(k), "type");
          while (k > 0
                 && ! any (strcmp (typ, {"line","patch","surface","hggroup"})))
            typ = get (kids(--k), "type");
          endwhile
          if (k > 0)
            if (strcmp (get (kids(k), "type"), "hggroup"))
              hgkids = get (kids(k), "children");
              for j = 1 : length (hgkids)
                hgobj = get (hgkids(j));
                if (isfield (hgobj, "displayname"))
                  if (have_labels)
                    set (hgkids(j), "displayname", arg);
                  endif
                  hplots(end+1) = hgkids(j);
                  text_strings(end+1) = arg;
                  break;
                endif
              endfor
            else
              if (have_labels)
                set (kids(k), "displayname", arg);
              endif
              hplots(end+1) = kids(k);
              text_strings(end+1) = arg;
            endif

            if (--k == 0)
              break;
            endif
          else
            break;  # k = 0, no further handles to process
          endif
        else
          error ("legend: expecting argument to be a character string");
        endif
      endfor
      if (have_labels && i < nargs)
        warning ("legend: ignoring extra labels");
      endif
    else
      ## No labels specified but objects have DisplayName property set.
      k = nkids;
      while (k > 0)
        typ = get (kids(k), "type");
        while (k > 1
               && ! any (strcmp (typ, {"line","patch","surface","hggroup"})))
          typ = get (kids(--k), "type");
        endwhile
        if (! any (strcmp (typ, {"line","patch","surface","hggroup"})))
          break;
        endif
        if (k > 0)
          if (strcmp (get (kids(k), "type"), "hggroup"))
            hgkids = get (kids(k), "children");
            for j = 1 : length (hgkids)
              hgobj = get (hgkids(j));
              if (isfield (hgobj, "displayname")
                  && ! isempty (hgobj.displayname))
                hplots(end+1) = hgkids(j);
                text_strings(end+1) = hgobj.displayname;
                break;
              endif
            endfor
          else
            if (! isempty (get (kids(k), "displayname")))
              hplots(end+1) = kids(k);
              text_strings(end+1) = get (kids(k), "displayname");
            endif
          endif
          if (--k == 0)
            break;
          endif
        endif
      endwhile
    endif

    if (isempty (hplots))
      if (! isempty (hlegend))
        fkids = get (fig, "children");
        delete (fkids(fkids == hlegend));
        hlegend = [];
        hobjects = [];
        hplots = [];
        text_strings = {};
      endif
    else
      ## Preserve the old legend if it exists
      if (! isempty (hlegend))
        if (strcmp (textpos, "default"))
          textpos = get (hlegend, "textposition");
        endif
        if (strcmp (location, "default"))
          location = get (hlegend, "location");
          inout = strfind (location, "outside");
          if (! isempty (inout))
            outside = true;
            location = location(1:inout-1);
          else
            outside = false;
          endif
        endif
        if (strcmp (orientation, "default"))
          orientation = get (hlegend, "orientation");
        endif
        box = get (hlegend, "box");
      else
        if (strcmp (textpos, "default"))
          textpos = "right";
        endif
        if (strcmp (location, "default"))
          location = "northeast";
        endif
        if (strcmp (orientation, "default"))
          orientation = "vertical";
        endif
        box = "on";
      endif

      ## Get axis size and fontsize in points.
      ## Rely on listener to handle coversion.
      units = get (ca(1), "units");
      unwind_protect
        set (ca(1), "units", "points");
        set (ca(1), "fontunits", "points");
        if (isempty (hlegend) || ! isprop (hlegend, "unmodified_axes_position"))
          unmodified_axes_position = get (ca(1), "position");
          unmodified_axes_outerposition = get (ca(1), "outerposition");
        else
          unmodified_axes_position = get (hlegend, "unmodified_axes_position");
          unmodified_axes_outerposition = get (hlegend, ...
                                               "unmodified_axes_outerposition");
        endif
        ca_pos = unmodified_axes_position;
        ca_outpos = unmodified_axes_outerposition;
        tightinset = get (ca(1), "tightinset");
        for i = 2 : numel (ca)
          tightinset = max (tightinset, get (ca(i), "tightinset"));
        endfor
      unwind_protect_cleanup
        set (ca(1), "units", units);
      end_unwind_protect

      ## Padding between legend entries horizontally and vertically
      xpad = 2;
      ypad = 2;

      linelength = 15;

      ## Create the axis first
      oldfig = get (0, "currentfigure");
      if (oldfig != fig)
        set (0, "currentfigure", fig);
      else
        oldfig = [];
      endif
      curaxes = get (fig, "currentaxes");
      unwind_protect
        ud = ancestor (hplots, "axes");
        if (! isscalar (ud))
          ud = unique ([ud{:}]);
        endif
        if (isempty (hlegend))
          addprops = true;
          hlegend = axes ("tag", "legend", "userdata", struct ("handle", ud),
                          "box", box,
                          "xtick", [], "ytick", [],
                          "xlim", [0, 1], "ylim", [0, 1],
                          "visible", ifelse (strcmp (box, "on"), "on", "off"),
                          "activepositionproperty", "position",
                          "interpreter", "tex");
          ## Inherit properties from current axis
          ## "fontunits" shoud be first because it affects interpretation
          ## of "fontsize" property
          proplist = {"fontunits", "fontangle", "fontname", "fontsize", ...
                      "fontweight"};
          ca_props = get (ca(1), proplist);
          set (hlegend, proplist, ca_props);
        else
          addprops = false;
          axes (hlegend);
          delete (get (hlegend, "children"));
        endif
        if (addprops)
          addproperty ("edgecolor", hlegend, "color", [0, 0, 0]);
          addproperty ("textcolor", hlegend, "color", [0, 0, 0]);
          locations = {"north", "south", "east", "west", ...
                       "{northeast}", "southeast", "northwest", "southwest", ...
                       "northoutside", "southoutside", ...
                       "eastoutside", "westoutside", ...
                       "northeastoutside", "southeastoutside", ...
                       "northwestoutside", "southwestoutside"};
          addproperty ("location", hlegend, "radio", strjoin (locations, "|"));
          addproperty ("orientation", hlegend, "radio",
                       "{vertical}|horizontal");
          addproperty ("string", hlegend, "any", text_strings);
          addproperty ("textposition", hlegend, "radio", "left|{right}");
        endif
        ## Inherit visual properties from legend object
        fontunits = get (hlegend, "fontunits");
        fontangle = get (hlegend, "fontangle");
        fontname = get (hlegend, "fontname");
        fontsize = get (hlegend, "fontsize");
        fontweight = get (hlegend, "fontweight");
        interpreter = get (hlegend, "interpreter");
        textcolor = get (hlegend, "textcolor");
        ## Add text label to the axis first, checking their extents
        nentries = numel (hplots);
        texthandle = [];
        maxwidth = 0;
        maxheight = 0;
        for k = 1 : nentries
          halign = ifelse (strcmp (textpos, "right"), "left", "right");
          texthandle(end+1) = text (0, 0, text_strings{k},
                                    "color", textcolor,
                                    "horizontalalignment", halign,
                                    "interpreter", interpreter,
                                    "fontunits", fontunits,
                                    "fontangle", fontangle,
                                    "fontname", fontname,
                                    "fontsize", fontsize,
                                    "fontweight", fontweight,
                                    "userdata", hplots(k));
          units = get (texthandle(end), "units");
          unwind_protect
            set (texthandle(end), "units", "points");
            extents = get (texthandle(end), "extent");
            maxwidth = max (maxwidth, extents(3));
            maxheight = max (maxheight, extents(4));
          unwind_protect_cleanup
            set (texthandle(end), "units", units);
          end_unwind_protect
        endfor

        num1 = nentries;
        if (strcmp (orientation, "vertical"))
          height = nentries * (ypad + maxheight);
          if (outside)
            if (height > ca_pos(4))
              ## Avoid shrinking the height of the axis to zero if outside
              num1 = ca_pos(4) / (maxheight + ypad) / 2;
            endif
          else
            if (height > 0.9 * ca_pos(4))
              num1 = 0.9 * ca_pos(4) / (maxheight + ypad);
            endif
          endif
        else
          width = nentries * (ypad + maxwidth);
          if (outside)
            if (width > ca_pos(3))
              ## Avoid shrinking the width of the axis to zero if outside
              num1 = ca_pos(3) / (maxwidth + ypad) / 2;
            endif
          else
            if (width > 0.9 * ca_pos(3))
              num1 = 0.9 * ca_pos(3) / (maxwidth + ypad);
            endif
          endif
        endif
        num2 = ceil (nentries / num1);

        xstep = 3 * xpad + (maxwidth + linelength);
        if (strcmp (textpos, "right"))
          xoffset = xpad;
          txoffset = 2 * xpad + linelength;
        else
          xoffset = 2 * xpad + maxwidth;
          txoffset = xpad + maxwidth;
        endif
        ystep = (ypad + maxheight);
        yoffset = ystep / 2;

        ## Place the legend in the desired location
        if (strcmp (orientation, "vertical"))
          lpos = [0, 0, num2 * xstep, num1 * ystep];
        else
          lpos = [0, 0, num1 * xstep, num2 * ystep];
        endif

        gnuplot = strcmp (get (fig, "__graphics_toolkit__"), "gnuplot");
        if (gnuplot)
          ## Gnuplot places the key (legend) at edge of the figure window.
          ## OpenGL places the legend box at edge of the unmodified axes
          ## position.
          if (isempty (strfind (location, "east")))
            gnuplot_offset = unmodified_axes_outerposition(1) ...
                           + unmodified_axes_outerposition(3) ...
                           - unmodified_axes_position(1) ...
                           - unmodified_axes_position(3);
          else
            gnuplot_offset = unmodified_axes_position(1) ...
                           - unmodified_axes_outerposition(1);
          endif
          ## FIXME: The "fontsize" is added to match the behavior of OpenGL.
          ## This implies that a change in fontsize should trigger a listener
          ## to update the legend.  The "2" was determined using a long legend
          ## key in the absence of any subplots.
          gnuplot_offset = gnuplot_offset - 2 * fontsize;
        else
          gnuplot_offset = 0;
        endif

        ## For legend's outside the associated axes postion,
        ## align their edge to the unmodified_axes_outerpostion,
        ## and adjust the axes postion accordingly.
        switch (location)
          case "north"
            if (outside)
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_outpos(2) + ca_outpos(4) - lpos(4) - ypad, lpos(3), ...
                      lpos(4)];

              new_pos = [ca_pos(1), ca_pos(2), ca_pos(3), ca_pos(4) - lpos(4)];
            else
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_pos(2) + ca_pos(4) - lpos(4) - ypad, lpos(3), lpos(4)];
            endif
          case "south"
            if (outside)
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_outpos(2) + ypad, lpos(3), lpos(4)];
              new_pos = [ca_pos(1), lpos(2) + lpos(4) + 2 * ypad ...
                      + tightinset(2), ca_pos(3), ...
                         ca_pos(4) - lpos(4)];
            else
              lpos = [ca_pos(1) + (ca_pos(3) - lpos(3)) / 2, ...
                      ca_pos(2) + ypad, lpos(3), lpos(4)];
            endif
          case "east"
            if (outside)
              lpos = [ca_outpos(1) + ca_outpos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2), ...
                         lpos(1) - 2 * xpad - ca_pos(1) - tightinset(3), ...
                         ca_pos(4)];
              new_pos(3) = new_pos(3) + gnuplot_offset;
            else
              lpos = [ca_pos(1) + ca_pos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, lpos(3), lpos(4)];
            endif
          case "west"
            if (outside)
              lpos = [ca_outpos(1) + ypad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, ...
                      lpos(3), lpos(4)];
              new_pos = [lpos(1) + lpos(3) + 2 * xpad + tightinset(1), ...
                         ca_pos(2), ca_pos(3) - lpos(3) - 2 * xpad, ca_pos(4)];
              new_pos(1) = new_pos(1) - gnuplot_offset;
              new_pos(3) = new_pos(3) + gnuplot_offset;
            else
              lpos = [ca_pos(1) +  ypad, ...
                      ca_pos(2) + (ca_pos(4) - lpos(4)) / 2, lpos(3), lpos(4)];
            endif
          case "northeast"
            if (outside)
              lpos = [ca_outpos(1) + ca_outpos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + ca_pos(4) - lpos(4), lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2), ...
                         lpos(1) - 2 * xpad - tightinset(3) - ca_pos(1), ...
                         ca_pos(4)];
              new_pos(3) = new_pos(3) + gnuplot_offset;
            else
              lpos = [ca_pos(1) + ca_pos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + ca_pos(4) - lpos(4) - ypad, lpos(3), lpos(4)];
            endif
          case "northwest"
            if (outside)
              lpos = [ca_outpos(1) + ypad , ca_pos(2) + ca_pos(4) - lpos(4), ...
                      lpos(3), lpos(4)];
              new_pos = [lpos(1) + lpos(3) + 2 * xpad + tightinset(1), ...
              ca_pos(2), ca_pos(3) - lpos(3) - 2 * xpad, ca_pos(4)];
              new_pos(1) = new_pos(1) - gnuplot_offset;
              new_pos(3) = new_pos(3) + gnuplot_offset;
            else
              lpos = [ca_pos(1) + ypad, ...
                      ca_pos(2) + ca_pos(4) - lpos(4) - ypad, lpos(3), lpos(4)];
            endif
          case "southeast"
            if (outside)
              lpos = [ca_outpos(1) + ca_outpos(3) - lpos(3) - ypad, ...
                      ca_pos(2), lpos(3), lpos(4)];
              new_pos = [ca_pos(1), ca_pos(2), ...
                         lpos(1) - 2 * xpad - ca_pos(1) - tightinset(3), ...
                         ca_pos(4)];
              new_pos(3) = new_pos(3) + gnuplot_offset;
            else
              lpos = [ca_pos(1) + ca_pos(3) - lpos(3) - ypad, ...
                      ca_pos(2) + ypad, lpos(3), lpos(4)];
            endif
          case "southwest"
            if (outside)
              lpos = [ca_outpos(1) + ypad, ca_pos(2), lpos(3), lpos(4)];
              new_pos = [lpos(1) + lpos(3) + 2 * xpad + tightinset(1), ...
              ca_pos(2), ca_pos(3) - lpos(3) - 2 * xpad, ca_pos(4)];
              new_pos(1) = new_pos(1) - gnuplot_offset;
              new_pos(3) = new_pos(3) + gnuplot_offset;
            else
              lpos = [ca_pos(1) + ypad, ca_pos(2) + ypad, lpos(3), lpos(4)];
            endif
        endswitch

        units = get (hlegend, "units");
        unwind_protect
          set (hlegend, "units", "points");
          set (hlegend, "position", lpos);
        unwind_protect_cleanup
          set (hlegend, "units", units);
        end_unwind_protect

        ## Now write the line segments and place the text objects correctly
        xk = 0;
        yk = 0;
        for k = 1 : numel (hplots)
          hobjects(end+1) = texthandle(k);
          switch (get (hplots(k), "type"))

            case "line"
              color = get (hplots(k), "color");
              style = get (hplots(k), "linestyle");
              lwidth = min (get (hplots(k), "linewidth"), 5);
              if (! strcmp (style, "none"))
                l1 = line ("xdata", ([xoffset, xoffset + linelength] + xk * xstep) / lpos(3),
                           "ydata", [1, 1] .* (lpos(4) - yoffset - yk * ystep) / lpos(4),
                           "color", color, "linestyle", style, "linewidth", lwidth,
                           "marker", "none",
                           "userdata", hplots(k));
                hobjects(end+1) = l1;
              endif
              marker = get (hplots(k), "marker");
              if (! strcmp (marker, "none"))
                l1 = line ("xdata", (xoffset + 0.5 * linelength  + xk * xstep) / lpos(3),
                           "ydata", (lpos(4) - yoffset - yk * ystep) / lpos(4),
                           "color", color, "linestyle", "none", "linewidth", lwidth,
                           "marker", marker,
                           "markeredgecolor",get (hplots(k), "markeredgecolor"),
                           "markerfacecolor",get (hplots(k), "markerfacecolor"),
                           "markersize", min (get (hplots(k), "markersize"),10),
                           "userdata", hplots(k));
                hobjects(end+1) = l1;
              endif

              if (addprops)
                addlistener (hplots(k), "color",
                             {@updateline, hlegend, linelength, false});
                addlistener (hplots(k), "linestyle",
                             {@updateline, hlegend, linelength, false});
                addlistener (hplots(k), "linewidth",
                             {@updateline, hlegend, linelength, false});
                addlistener (hplots(k), "marker",
                             {@updateline, hlegend, linelength, false});
                addlistener (hplots(k), "markeredgecolor",
                             {@updateline, hlegend, linelength, false});
                addlistener (hplots(k), "markerfacecolor",
                             {@updateline, hlegend, linelength, false});
                addlistener (hplots(k), "markersize",
                             {@updateline, hlegend, linelength, false});
                addlistener (hplots(k), "displayname",
                             {@updateline, hlegend, linelength, true});
              endif

            case "patch"
              facecolor = get (hplots(k), "facecolor");
              edgecolor = get (hplots(k), "edgecolor");
              cdata = get (hplots(k), "cdata");
              if (! strcmp (facecolor, "none") || ! strcmp (edgecolor, "none"))
                p1 = patch ("xdata", ([0, linelength, linelength, 0] +
                                      xoffset + xk * xstep) / lpos(3),
                            "ydata", (lpos(4) - yoffset -
                                      [yk-0.3, yk-0.3, yk+0.3, yk+0.3] .* ystep) / lpos(4),
                            "facecolor", facecolor, "edgecolor", edgecolor,
                            "cdata", cdata, "userdata", hplots(k));
              else
                ## non-standard patch only making use of marker styles
                ## such as scatter plot.
                p1 = patch ("xdata", (xoffset + 0.5 * linelength  + xk * xstep) / lpos(3),
                            "ydata", (lpos(4) - yoffset - yk * ystep) / lpos(4),
                            "marker", get (hplots(k), "marker"),
                            "markeredgecolor",get (hplots(k),"markeredgecolor"),
                            "markerfacecolor",get (hplots(k),"markerfacecolor"),
                            "markersize", min (get (hplots(k),"markersize"),10),
                            "cdata", cdata, "userdata", hplots(k));
              endif
              hobjects(end+1) = p1;
              ## Copy clim from axes so that colors work out.
              set (hlegend, "clim", get (ca(1), "clim"));

              ## FIXME: Need listeners, as for line objects.
              ##        Changing clim, for example, won't update colors

            case "surface"
              facecolor = get (hplots(k), "facecolor");
              edgecolor = get (hplots(k), "edgecolor");
              cdata = sum (get (ca(1), "clim")) / 2;
              if (! strcmp (facecolor, "none") || ! strcmp (edgecolor, "none"))
                p1 = patch ("xdata", ([0, linelength, linelength, 0] +
                                      xoffset + xk * xstep) / lpos(3),
                            "ydata", (lpos(4) - yoffset -
                                      [yk-0.3, yk-0.3, yk+0.3, yk+0.3] .* ystep) / lpos(4),
                            "facecolor", facecolor, "edgecolor", edgecolor,
                            "cdata", cdata, "userdata", hplots(k));
                hobjects(end+1) = p1;
              endif
              ## FIXME: Need listeners, as for line objects.

          endswitch

          set (texthandle(k), "position",
                              [(txoffset + xk * xstep) / lpos(3), ...
                               (lpos(4) - yoffset - yk * ystep) / lpos(4)]);
          if (strcmp (orientation, "vertical"))
            yk++;
            if (yk > num1)
              yk = 0;
              xk++;
            endif
          else
            xk++;
            if (xk > num1)
              xk = 0;
              yk++;
            endif
          endif
        endfor

        ## Add an invisible text object to original axis
        ## that when it is destroyed will remove the legend
        props = {"parent", ca(1), "tag", "legend", ...
                 "handlevisibility", "off", "visible", "off", ...
                 "xliminclude", "off", "yliminclude", "off"};
        t1 = findall (ca(1), "tag", "legend", "type", "text");
        if (isempty (t1))
          t1 = text (0, 0, "", props{:});
          set (t1, "deletefcn", {@deletelegend1, hlegend});
        endif
        if (isprop (hlegend, "unmodified_axes_position"))
          set (hlegend, "unmodified_axes_position",
                         unmodified_axes_position,
                        "unmodified_axes_outerposition",
                         unmodified_axes_outerposition);
        else
          addproperty ("unmodified_axes_position", hlegend,
                       "data", unmodified_axes_position);
          addproperty ("unmodified_axes_outerposition", hlegend,
                       "data", unmodified_axes_outerposition);
        endif

        ## Resize the axis that the legend is attached to if the legend is
        ## "outside" the plot and create a listener to resize axis to original
        ## size if the legend is deleted, hidden, or shown.
        if (outside)
          for i = 1 : numel (ca)
            units = get (ca(i), "units");
            unwind_protect
              set (ca(i), "units", "points");
              if (gnuplot && numel (ca) == 1)
                ## Let Gnuplot handle the positioning of the keybox.
                ## This violates strict Matlab compatibility, but reliably
                ## renders an esthetic result.
                set (ca(i), "position",  unmodified_axes_position);
                set (ca(i), "activepositionproperty", "outerposition")
              else
                ## numel (ca) > 1 for axes overlays (like plotyy)
                set (ca(i), "position", new_pos);
              endif
            unwind_protect_cleanup
              set (ca(i), "units", units);
            end_unwind_protect
          endfor

          set (hlegend, "deletefcn", {@deletelegend2, ca, ...
                                      unmodified_axes_position, ...
                                      unmodified_axes_outerposition, ...
                                      t1, hplots});
          addlistener (hlegend, "visible", {@hideshowlegend, ca, ...
                                            unmodified_axes_position, ...
                                            new_pos});
        else
          set (hlegend, "deletefcn", {@deletelegend2, ca, [], [], t1, hplots});
        endif

        if (! addprops)
          ## Remove listeners on existing legend temporarily to stop recursion.
          dellistener (hlegend, "location");
          dellistener (hlegend, "orientation");
          dellistener (hlegend, "string");
          dellistener (hlegend, "textposition");
        endif

        if (! addprops)
          set (hlegend, "string", text_strings);
        endif

        if (outside)
          set (hlegend, "location", [location "outside"],
                        "orientation", orientation, "textposition", textpos);
        else
          set (hlegend, "location", location, "orientation", orientation,
                        "textposition", textpos);
        endif

        if (addprops)
          addlistener (hlegend, "edgecolor", @updatelegendtext);
          addlistener (hlegend, "fontangle", @updatelegendtext);
          addlistener (hlegend, "fontname", @updatelegendtext);
          addlistener (hlegend, "fontweight", @updatelegendtext);
          addlistener (hlegend, "textcolor", @updatelegendtext);
          ## Properties which could change size of box, such as fontsize,
          ## require legend to be redrawn.
          ## FIXME: fontsize is changed by print.m function during the
          ##        production of a plot for output.  This screws things up
          ##        because legend tries to return the axes size to what it
          ##        was when the figure was created, versus what it is now
          ##        when the figure is being printed.  Temporary hack is
          ##        good enough for generating the Octave manual which still
          ##        relies on gnuplot for generating images.  See bug #40333.
          if (! gnuplot)
            addlistener (hlegend, "fontsize", @updatelegend);
          endif
          addlistener (hlegend, "fontunits", @updatelegend);
          addlistener (hlegend, "interpreter", @updatelegend);
          addlistener (hlegend, "location", @updatelegend);
          addlistener (hlegend, "orientation", @updatelegend);
          addlistener (hlegend, "string", @updatelegend);
          addlistener (hlegend, "textposition", @updatelegend);
          ## FIXME: need to add listeners for tightinset and position
          ##        addlistener (ca, "tightinset", @update????);
          ##        addlistener (ca, "position", @update????);
        else
          ## Restore certain listeners
          addlistener (hlegend, "location", @updatelegend);
          addlistener (hlegend, "orientation", @updatelegend);
          addlistener (hlegend, "string", @updatelegend);
          addlistener (hlegend, "textposition", @updatelegend);
        endif
      unwind_protect_cleanup
        set (fig, "currentaxes", curaxes);
        if (! isempty (oldfig))
          set (0, "currentfigure", oldfig);
        endif
      end_unwind_protect
    endif
  endif

  if (nargout > 0)
    hleg = hlegend;
    hleg_obj = hobjects;
    hplot = hplots;
    labels = text_strings;
  endif

endfunction

function updatelegend (h, ~)
  persistent recursive = false;

  if (! recursive)
    recursive = true;
    unwind_protect
      hax = getfield (get (h, "userdata"), "handle");
      [hplots, ~] = __getlegenddata__ (h);
      position = get (h, "unmodified_axes_position");
      outerposition = get (h, "unmodified_axes_outerposition");
      units = get (hax, "units");
      set (hax, "units", "points");
      switch (get (hax, "activepositionproperty"))
        case "position"
          set (hax, "outerposition", outerposition);
          set (hax, "position", position);
        case "outerposition"
          set (hax, "position", position);
          set (hax, "outerposition", outerposition);
      endswitch
      set (hax, {"units"}, units);

      h = legend (hax(1), hplots, get (h, "string"));
    unwind_protect_cleanup
      recursive = false;
    end_unwind_protect
  endif

endfunction

function updatelegendtext (h, ~)
  kids = get (h, "children");
  htext = [];
  for i = 1:numel (kids)
    if (strcmp (get (kids(i), "type"), "text"))
      htext(end+1) = kids(i);
    endif
  endfor

  tprops = {"fontangle", "fontname", "fontweight", "color"};
  lprops = {"fontangle", "fontname", "fontweight", "textcolor"};
  set (htext, tprops, get (h, lprops));

  ec = get (h, "edgecolor");
  set (h, "xcolor", ec, "ycolor", ec);
endfunction

function hideshowlegend (h, ~, ca, pos1, pos2)
  isvisible = strcmp (get (h, "visible"), "off");
  if (! isvisible)
    kids = get (h, "children");
    if (any (! strcmp (get (kids, "visible"), "off")))
      isvisible = true;
    endif
  endif

  for i = 1 : numel (ca)
    if (isaxes (ca(i))
        && (isempty (gcbf ()) || strcmp (get (gcbf (), "beingdeleted"),"off"))
        && strcmp (get (ca(i), "beingdeleted"), "off"))
      units = get (ca(i), "units");
      unwind_protect
        set (ca(i), "units", "points");
        if (isvisible)
          set (ca(i), "position", pos2);
        else
          set (ca(i), "position", pos1);
        endif
      unwind_protect_cleanup
        set (ca(i), "units", units);
      end_unwind_protect
    endif
  endfor
endfunction

function deletelegend1 (h, ~, ca)
  if (isaxes (ca)
      && (isempty (gcbf ()) || strcmp (get (gcbf (), "beingdeleted"), "off"))
      && strcmp (get (ca, "beingdeleted"), "off"))
    delete (ca);
  endif
endfunction

function deletelegend2 (h, ~, ca, pos, outpos, t1, hplots)
  for i = 1 : numel (ca)
    if (isaxes (ca(i))
        && (isempty (gcbf ()) || strcmp (get (gcbf (), "beingdeleted"), "off"))
        && strcmp (get (ca(i), "beingdeleted"), "off"))
      if (! isempty (pos) && ! isempty (outpos))
        units = get (ca(i), "units");
        unwind_protect
          set (ca(i), "units", "points");
          set (ca(i), "position", pos, "deletefcn", "");
        unwind_protect_cleanup
          set (ca(i), "units", units);
        end_unwind_protect
      endif
    endif
  endfor
  set (t1, "deletefcn", "");
  delete (t1);
  for i = 1 : numel (hplots)
    if (ishandle (hplots(i)) && strcmp (get (hplots(i), "type"), "line"))
      dellistener (hplots(i), "color");
      dellistener (hplots(i), "linestyle");
      dellistener (hplots(i), "linewidth");
      dellistener (hplots(i), "marker");
      dellistener (hplots(i), "markeredgecolor");
      dellistener (hplots(i), "markerfacecolor");
      dellistener (hplots(i), "markersize");
      dellistener (hplots(i), "displayname");
    endif
  endfor
endfunction

function updateline (h, ~, hlegend, linelength, update_name)

  if (update_name)
    ## When string changes, have to rebuild legend completely
    [hplots, text_strings] = __getlegenddata__ (hlegend);
    legend (get (hplots(1), "parent"), hplots, text_strings);
  else
    kids = get (hlegend, "children");
    ll = lm = [];
    for i = 1 : numel (kids)
      if (get (kids(i), "userdata") == h
          && strcmp (get (kids(i), "type"), "line"))
        if (strcmp (get (kids(i), "marker"), "none"))
          ll = kids(i);
        else
          lm = kids(i);
        endif
      endif
    endfor

    [linestyle, marker, displayname] = ...
      get (h, {"linestyle", "marker", "displayname"}){:};

    if (! isempty (ll))
      [xpos1, ypos1] = get (ll, {"xdata", "ydata"}){:};
      xpos2 = sum (xpos1) / 2;
      ypos2 = ypos1(1);
      delete (ll);
      if (! isempty (lm))
        delete (lm);
      endif
    else
      [xpos2, ypos2] = get (lm, {"xdata", "ydata"}){:};
      xpos1 = xpos2 + [-0.5, 0.5] * linelength;
      ypos1 = [ypos2, ypos2];
      delete (lm);
    endif

    if (! strcmp (linestyle, "none"))
      line ("xdata", xpos1, "ydata", ypos1, "color", get (h, "color"),
            "linestyle", get (h, "linestyle"),
            "linewidth", min (get (h, "linewidth"), 5),
            "marker", "none",
            "userdata", h, "parent", hlegend);
    endif
    if (! strcmp (marker, "none"))
      line ("xdata", xpos2, "ydata", ypos2, "color", get (h, "color"),
            "marker", marker, "markeredgecolor", get (h, "markeredgecolor"),
            "markerfacecolor", get (h, "markerfacecolor"),
            "markersize", min (get (h, "markersize"), 10),
            "linestyle", "none",
            "linewidth", min (get (h, "linewidth"), 5),
            "userdata", h, "parent", hlegend);
    endif
  endif
endfunction


%!demo
%! clf;
%! plot (rand (2));
%! title ('legend called with cellstr and string inputs for labels');
%! h = legend ({'foo'}, 'bar');
%! legend (h, 'location', 'northeastoutside');
%! set (h, 'fontsize', 20);

%!demo
%! clf;
%! plot (rand (3));
%! title ('legend("show") without inputs creates default labels');
%! h = legend ('show');

%!demo
%! clf;
%! x = 0:1;
%! plot (x,x,';I am Blue;', x,2*x, x,3*x,';I am Red;');
%! h = legend ('location', 'northeastoutside');
%! ## Placing legend inside should return axes to original size
%! legend (h, 'location', 'northeast');
%! title ('Blue and Red keys, with Green missing');

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ('incline is blue and decline is green');
%! legend ({'I am blue', 'I am green'}, 'location', 'east');
%! legend hide
%! legend show

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ('Legend with keys in horizontal orientation');
%! legend ({'I am blue', 'I am green'}, ...
%!         'location', 'east', 'orientation', 'horizontal');
%! legend boxoff
%! legend boxon

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ('Legend with box off');
%! legend ({'I am blue', 'I am green'}, 'location', 'east');
%! legend boxoff

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ('Legend with text to the left of key');
%! legend ({'I am blue', 'I am green'}, 'location', 'east');
%! legend left

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ({'Use properties to place legend text to the left of key', ...
%!         'Legend text color is magenta'});
%! h = legend ({'I am blue', 'I am green'}, 'location', 'east');
%! legend ('right');
%! set (h, 'textposition', 'left');
%! set (h, 'textcolor', [1 0 1]);

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ('Legend is hidden')
%! legend ({'I am blue', 'I am green'}, 'location', 'east');
%! legend hide

%!demo
%! clf;
%! x = 0:1;
%! plot (x,x,';I am Blue;', x,2*x,';I am Green;', x,3*x,';I am Red;');
%! title ({'Labels are embedded in call to plot', ...
%!         'Legend is hidden and then shown'});
%! legend boxon
%! legend hide
%! legend show

%!demo
%! clf;
%! x = 0:1;
%! plot (x, x, ';\alpha;',  ...
%!       x, 2*x, ';\beta=2\alpha;',  ...
%!       x, 3*x, ';\gamma=3\alpha;');
%! h = legend ();
%! set (h, 'interpreter', 'tex');
%! title ('Labels with interpreted Greek text');

%!demo
%! clf;
%! plot (rand (2));
%! title ('Labels with TeX interpreter turned off');
%! h = legend ('Hello_World', 'foo^bar');
%! set (h, 'interpreter', 'none');

%!demo
%! clf;
%! plot (1:10, 1:10);
%! title ('a very long label can sometimes cause problems');
%! legend ('hello very big world', 'location', 'northeastoutside');

%!demo
%! clf;
%! labels = {};
%! colororder = get (gca, 'colororder');
%! for i = 1:5
%!   h = plot (1:100, i + rand (100,1)); hold on;
%!   set (h, 'color', colororder(i,:));
%!   labels = {labels{:}, ['Signal ', num2str(i)]};
%! end
%! hold off;
%! title ({'Signals with random offset and uniform noise';
%!         'Legend shown below and outside of plot'});
%! xlabel ('Sample Nr [k]'); ylabel ('Amplitude [V]');
%! legend (labels, 'location', 'southoutside');

%!demo
%! clf;
%! x = linspace (0, 10);
%! plot (x, x);
%! hold on;
%! stem (x, x.^2, 'g');
%! title ('First created object gets first label');
%! legend ('linear');
%! hold off;

%!demo
%! clf;
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! title ('First created object gets first label');
%! legend ('linear');

%!demo
%! clf;
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! title ('Labels are applied in order of object creation');
%! legend ('linear', 'quadratic');

%!demo
%! clf;
%! rand_2x3_data1 = [0.341447, 0.171220, 0.284370; 0.039773, 0.731725, 0.779382];
%! bar (rand_2x3_data1);
%! ylim ([0 1.0]);
%! title ('legend() works for bar graphs (hggroups)');
%! legend ({'1st Bar', '2nd Bar', '3rd Bar'});

%!demo
%! clf;
%! rand_2x3_data2 = [0.44804, 0.84368, 0.23012; 0.72311, 0.58335, 0.90531];
%! bar (rand_2x3_data2);
%! ylim ([0 1.2]);
%! title ('"left" option places text label west of colors');
%! legend ('1st Bar', '2nd Bar', '3rd Bar');
%! legend left;

%!demo
%! clf;
%! x = 0:0.1:7;
%! h = plot (x,sin(x), x,cos(x), x,sin(x.^2/10), x,cos(x.^2/10));
%! title ('Only the sin() objects have keylabels');
%! legend (h([1, 3]), {'sin (x)', 'sin (x^2/10)'}, 'location', 'southwest');

%!demo
%! clf;
%! x = 0:0.1:10;
%! plot (x, sin (x), ';sin (x);');
%! hold all;
%! plot (x, cos (x), ';cos (x);');
%! hold off;
%! title ('legend constructed from multiple plot calls');

%!demo
%! clf;
%! x = 0:0.1:10;
%! plot (x, sin (x), ';sin (x);');
%! hold all;
%! plot (x, cos (x), ';cos (x);');
%! hold off;
%! title ('Specified label text overrides previous labels');
%! legend ({'Sine', 'Cosine'}, 'location', 'northeastoutside');

%!demo
%! clf;
%! x = 0:10;
%! plot (x, rand (11));
%! xlabel ('Indices');
%! ylabel ('Random Values');
%! title ('Legend ''off'' deletes the legend');
%! legend (cellstr (num2str ((1:10)')), 'location', 'northeastoutside');
%! legend off;
%! axis ([0, 10, 0 1]);

%!demo
%! clf;
%! x = (1:5)';
%! subplot (2,2,1);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), 'location', 'northwestoutside');
%! subplot (2,2,2);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), 'location', 'northeastoutside');
%! subplot (2,2,3);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), 'location', 'southwestoutside');
%! subplot (2,2,4);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), 'location', 'southeastoutside');

%!demo
%! clf;
%! plot (rand (2));
%! title ('legend() will warn if extra labels are specified');
%! legend ('Hello', 'World', 'interpreter', 'foobar');

%!demo
%! clf;
%! x = 0:10;
%! y1 = rand (size (x));
%! y2 = rand (size (x));
%! [ax, h1, h2] = plotyy (x, y1, x, y2);
%! title ('plotyy legend test #1: Blue and Green labels');
%! legend ([h1, h2], {'Blue', 'Green'}, 'location', 'south');

%!demo
%! clf;
%! x = 0:10;
%! y1 = rand (size (x));
%! y2 = rand (size (x));
%! [ax, h1, h2] = plotyy (x, y1, x, y2);
%! title ('plotyy legend test #2: Blue and Green labels');
%! legend ({'Blue', 'Green'}, 'location', 'south');

%!demo
%! clf;
%! x = 0:10;
%! y1 = rand (size (x));
%! y2 = rand (size (x));
%! [ax, h1, h2] = plotyy (x, y1, x, y2);
%! title ('plotyy legend test #3: Blue and Green labels');
%! legend ('Blue', 'Green', 'location', 'south');

%!demo % bug 36408
%! clf;
%! option = 'right';
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  title ('Subplots should adjust to the legend placed outside');
%!  legend ({'1'}, 'location', 'northeastoutside');
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  legend ({'1234567890'}, 'location', 'eastoutside');
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  legend ({'12345678901234567890'}, 'location', 'southeastoutside');
%!  legend (option);

%!demo % bug 36408
%! clf;
%! option = 'right';
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  title ('Subplots should adjust to the legend placed outside');
%!  legend ({'1'}, 'location', 'northwestoutside');
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  legend ({'1234567890'}, 'location', 'westoutside');
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  legend ({'12345678901234567890'}, 'location', 'southwestoutside');
%!  legend (option);

%!demo % bug 36408
%! clf;
%! option = 'right';
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  set (gca (), 'yaxislocation', 'right');
%!  xlabel ('xlabel');
%!  ylabel ('ylabel');
%!  title ('Subplots should adjust to the legend placed outside');
%!  legend ({'1'}, 'location', 'northeastoutside');
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  set (gca (), 'yaxislocation', 'right');
%!  xlabel ('xlabel');
%!  ylabel ('ylabel');
%!  legend ({'1234567890'}, 'location', 'eastoutside');
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  set (gca (), 'yaxislocation', 'right');
%!  xlabel ('xlabel');
%!  ylabel ('ylabel');
%!  legend ({'12345678901234567890'}, 'location', 'southeastoutside');
%!  legend (option);

%!demo % bug 36408
%! clf;
%! option = 'right';
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  set (gca (), 'yaxislocation', 'right');
%!  xlabel ('xlabel');
%!  ylabel ('ylabel');
%!  title ('Subplots should adjust to the legend placed outside');
%!  legend ({'1'}, 'location', 'northwestoutside');
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  set (gca (), 'yaxislocation', 'right');
%!  xlabel ('xlabel');
%!  ylabel ('ylabel');
%!  legend ({'1234567890'}, 'location', 'westoutside');
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  set (gca (), 'yaxislocation', 'right');
%!  xlabel ('xlabel');
%!  ylabel ('ylabel');
%!  legend ({'12345678901234567890'}, 'location', 'southwestoutside');
%!  legend (option);

%!demo % bug 36408;
%! clf;
%! option = 'right';
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  set (gca (), 'xaxislocation', 'top');
%!  xlabel ('xlabel');
%!  ylabel ('ylabel');
%!  title ('Subplots should adjust to the legend placed outside');
%!  legend ({'1'}, 'location', 'northwestoutside');
%!  legend (option);
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  set (gca (), 'xaxislocation', 'top');
%!  xlabel ('xlabel');
%!  ylabel ('ylabel');
%!  legend ({'1234567890'}, 'location', 'westoutside');
%!  legend (option);
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  set (gca (), 'xaxislocation', 'top');
%!  xlabel ('xlabel');
%!  ylabel ('ylabel');
%!  legend ({'12345678901234567890'}, 'location', 'southwestoutside');
%!  legend (option);

%!demo % bug 39697
%! clf;
%! plot (1:10);
%! legend ('Legend Text');
%! title ({'Multi-line', 'titles', 'are a', 'problem'});

%!demo
%! clf;
%! colormap (cool (64));
%! surf (peaks ());
%! legend ('peaks()')
%! title ('legend() works for surface objects too');

%!test
%! toolkit = graphics_toolkit ("gnuplot");
%! h = figure ("visible", "off");
%! unwind_protect
%!   position = get (h, "position");
%!   plot (rand (3));
%!   legend ();
%!   filename = sprintf ("%s.eps", tempname ());
%!   print (filename);
%!   unlink (filename);
%!   assert (get (h, "position"), position);
%! unwind_protect_cleanup
%!   close (h);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

%!test
%! ## bug #42035
%! h = figure ("visible", "off");
%! unwind_protect
%!   hax1 = subplot (1,2,1);
%!   plot (1:10);
%!   hax2 = subplot (1,2,2);
%!   plot (1:10);
%!   hleg1 = legend (hax1, "foo");
%!   assert (get (hleg1, "userdata").handle, hax1)
%!   assert (gca (), hax2);
%!   hleg2 = legend ("bar");
%!   assert (get (hleg2, "userdata").handle, gca ())
%! unwind_protect_cleanup
%!   close (h);
%! end_unwind_protect

