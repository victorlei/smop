## Copyright (C) 2015 Pantxo Diribarne
##
##   This program is free software; you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation; either version 3 of the License, or
##   (at your option) any later version.
##
##   This program is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
##   You should have received a copy of the GNU General Public License
##   along with Octave; see the file COPYING.  If not, see
##   <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} annotation (@var{type})
## @deftypefnx {Function File} {} annotation ("line", @var{x}, @var{y})
## @deftypefnx {Function File} {} annotation ("arrow", @var{x}, @var{y})
## @deftypefnx {Function File} {} annotation ("doublearrow", @var{x}, @var{y})
## @deftypefnx {Function File} {} annotation ("textarrow", @var{x}, @var{y})
## @deftypefnx {Function File} {} annotation ("textbox", @var{pos})
## @deftypefnx {Function File} {} annotation ("rectangle", @var{pos})
## @deftypefnx {Function File} {} annotation ("ellipse", @var{pos})
## @deftypefnx {Function File} {} annotation (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} annotation (@var{hf}, @dots{})
## @deftypefnx {Function File} {@var{h} =} annotation (@dots{})
## Draw annotations to emphasize parts of a figure.
##
## You may build a default annotation by specifying only the @var{type}
## of the annotation.
##
## Otherwise you can select the type of annotation and then set its position
## using either @var{x} and @var{y} coordinates for line-based annotations or a
## position vector @var{pos} for others.  In either case, coordinates are
## interpreted using the @qcode{"units"} property of the annotation object.
## The default is @qcode{"normalized"}, which means the lower left hand corner
## of the figure has coordinates @samp{[0 0]} and the upper right hand corner
## @samp{[1 1]}.
##
## If the first argument @var{hf} is a figure handle, then plot into this
## figure, rather than the current figure returned by @code{gcf}.
##
## Further arguments can be provided in the form of @var{prop}/@var{val} pairs
## to customize the annotation appearance.
##
## The optional return value @var{h} is a graphics handle to the created
## annotation object.  This can be used with the @code{set} function to
## customize an existing annotation object.
##
## All annotation objects share two properties:
##
## @itemize
## @item @qcode{"units"}: the units in which coordinates are interpreted.@*
## Its value may be one of @qcode{"centimeters"} | @qcode{"characters"} |
## @qcode{"inches"} | @qcode{"@{normalized@}"} | @qcode{"pixels"} |
## @qcode{"points"}.
##
## @item @qcode{"position"}: a four-element vector [x0 y0 width height].@*
## The vector specifies the coordinates (x0,y0) of the origin of the annotation
## object, its width, and its height.  The width and height may be negative,
## depending on the orientation of the object.
##
## @end itemize
##
## Valid annotation types and their specific properties are described
## below:
##
## @table @asis
## @item @qcode{"line"}
## Constructs a line.  @var{x} and @var{y} must be two-element vectors
## specifying the x and y coordinates of the two ends of the line.
##
## The line can be customized using @qcode{"linewidth"}, @qcode{"linestyle"},
## and @qcode{"color"} properties the same way as for @code{line} objects.
##
## @item @qcode{"arrow"}
## Construct an arrow.  The second point in vectors @var{x} and @var{y}
## specifies the arrowhead coordinates.
##
## Besides line properties, the arrowhead can be customized using
## @qcode{"headlength"}, @qcode{"headwidth"}, and @qcode{"headstyle"}
## properties.  Supported values for @qcode{"headstyle"} property are:
## [@qcode{"diamond"} | @qcode{"ellipse"} | @qcode{"plain"} |
## @qcode{"rectangle"} | @qcode{"vback1"} | @qcode{"@{vback2@}"} |
## @qcode{"vback3"}]
##
## @item @qcode{"doublearrow"}
## Construct a double arrow.  Vectors @var{x} and @var{y} specify the
## arrowhead coordinates.
##
## The line and the arrowhead can be customized as for arrow annotations, but
## some property names are duplicated:
## @qcode{"head1length"}/@qcode{"head2length"},
## @qcode{"head1width"}/@qcode{"head2width"}, etc.  The index 1 marks the
## properties of the arrowhead at the first point in @var{x} and @var{y}
## coordinates.
##
## @item @qcode{"textarrow"}
## Construct an arrow with a text label at the opposite end from the arrowhead.
##
## Use the "string" property to change the text string.
## The line and the arrowhead can be customized as for arrow annotations, and
## the text can be customized using the same properties as @code{text} graphics
## objects.  Note, however, that some text property names are prefixed with
## "text" to distinguish them from arrow properties:
## @qcode{"textbackgroundcolor"}, @qcode{"textcolor"},
## @qcode{"textedgecolor"}, @qcode{"textlinewidth"},
## @qcode{"textmargin"}, @qcode{"textrotation"}.
##
## @item @qcode{"textbox"}
## Construct a box with text inside.  @var{pos} specifies the
## @qcode{"position"} property of the annotation.
##
## Use the "string" property to change the text string.
## You may use @qcode{"backgroundcolor"}, @qcode{"edgecolor"},
## @qcode{"linestyle"}, and @qcode{"linewidth"} properties to customize
## the box background color and edge appearance.  A limited set of @code{text}
## objects properties are also available; Besides @qcode{"font@dots{}"}
## properties, you may also use @qcode{"horizontalalignment"} and
## @qcode{"verticalalignment"} to position the text inside the box.
##
## Finally, the @qcode{"fitboxtotext"} property controls the actual extent of
## the box.  If @qcode{"on"} (the default) the box limits are fitted to the
## text extent.
##
## @item @qcode{"rectangle"}
## Construct a rectangle.  @var{pos} specifies the @qcode{"position"} property
## of the annotation.
##
## You may use @qcode{"facecolor"}, @qcode{"color"}, @qcode{"linestyle"}, and
## @qcode{"linewidth"} properties to customize the rectangle background color
## and edge appearance.
##
## @item @qcode{"ellipse"}
## Construct an ellipse.  @var{pos} specifies the @qcode{"position"} property
## of the annotation.
##
## See @qcode{"rectangle"} annotations for customization.
## @end table
##
## @seealso{xlabel, ylabel, zlabel, title, text, gtext, legend, colorbar}
## @end deftypefn

function varargout = annotation (varargin)

  objtype = "";
  hf = [];
  lims = [];
  x = y = [];
  opts = {};

  nargin = numel (varargin);
  if (nargin == 0)
    print_usage ();
  endif


  ## Parent figure
  if (isfigure (varargin{1}))
    hf = varargin{1};
    varargin = varargin(2:end);
    nargin --;
  endif

  ## Annotation type
  types = {"line", "arrow", "doublearrow", "textarrow", ...
           "textbox", "ellipse", "rectangle"};
  if (ischar (varargin{1}))
    objtype = varargin{1};
    varargin(1) = [];
    nargin --;
  else
    print_usage ();
  endif

  switch (objtype)
    case types(1:4)

      if (nargin == 0)
        lims = [];
      elseif (nargin >= 2)
        x = varargin{1};
        y = varargin{2};
        varargin(1:2) = [];

        if (isnumeric (x) && isnumeric (y)
            && length (x) == 2 && length (y) == 2)
          lims = [x(1) y(1) diff(x) diff(y)];
        else
          error ("annotation: expect 2 elements vectors for X and Y");
        endif
      else
        print_usage ();
      endif
    case types(5:end)
      if (nargin == 0)
        lims = [];
      else
        lims = varargin{1};
        varargin(1) = [];

        if (! isvector (lims) || length (lims) != 4)
          error ("annotation: expect 4 elements vector for POS")
        endif
      endif
    otherwise
      error ("annotation: unknown annotation type %s", objtype)
  endswitch

  ## options
  opts = varargin;
  nopts = numel (opts);
  if (! isempty (opts))
    if (fix (nopts/2) != nopts/2
        || ! all (cellfun (@ischar, opts(1:2:end))))
      warning ("annotation: couldn't parse PROP/VAL pairs, skipping");
      opts = {};
    endif
  endif

  ## create annotation
  showhidden = get (0, "showhiddenhandles");
  set (0, "showhiddenhandles", "on");

  unwind_protect
    if (isempty (hf))
      hf = gcf ();
    endif

    ## Axes
    hca = get (hf, "currentaxes");

    hax = findall (hf, "-depth", 1, "tag", "scribeoverlay");
    if (isempty (hax))
      hax = buildoverlay (hf);
    else
      ## Make sure the annotations are on top of other axes
      axes (hax);
    endif

    ## Build annotation
    htmp = buildannot (hax, objtype, lims);

    ## Set user defined properties
    if (! isempty (opts))
      set (htmp, opts{:});
    endif

  unwind_protect_cleanup
    set (0, "showhiddenhandles", showhidden);
    set (hf, "currentaxes", hca);
  end_unwind_protect

  if (nargout != 0)
    varargout{1} = htmp;
  endif

endfunction

function hax = buildoverlay (hf)

  hax = axes ("parent", hf, "position", [0 0 1 1], ...
              "visible", "off","tag", "scribeoverlay", ...
              "xlim", [0 1], "ylim", [0 1], ...
              "handlevisibility", "off");

  ## hidden property to store figure size in absolute (points)
  ## coordinates
  addproperty ("figsize_points", hax, "axesxmtick", []);
  update_figsize_points (hf, {}, hax);


  listener = {@update_figsize_points, hax};
  addlistener (hf, "position", listener);

  delfcn = @() dellistener (hf, "position", listener);
  set (hax, "deletefcn", delfcn);

endfunction

function update_figsize_points (hf, dummy, hax)

  persistent recursive = false;
  if (! recursive)
    recursive = true;
    units = get (hf, "units");
    set (hf, "units", "points");
    pos = get (hf, "position");
    set (hf, "units", units);

    set (hax, "figsize_points", pos(3:4));
    recursive = false;
  endif

endfunction

function h = buildannot (hax, objtype, pos)

  ## Base hggroup
  h = hggroup ("parent", hax);

  ## Base context menu
  hui = uicontextmenu (get (hax, "parent"));

  ## Add common properties
  if (strcmp (graphics_toolkit (), "gnuplot"))
    ## FIXME: this is a workaround for bug #39394 (gnuplot toolkit)
    defprops = {"position", "axesposition", [0.3 0.3 0.1 0.1], ...
                "units", "textunits", "data"};
  else
    defprops = {"position", "axesposition", [0.3 0.3 0.1 0.1], ...
                "units", "axesunits", "normalized"};
  endif
  addbaseprops (h, defprops);
  setappdata (h, "__former_units__", "normalized");

  ## Add common menus
  prop = "units";
  vals = set (h, prop);
  addbasemenu (hui, h, prop, vals, "Units");

  ## Common updaters
  listener = {@update_position, h, true};

  addlistener (hax, "figsize_points", listener);

  delfcn = @() dellistener (hax, "figsize_points", listener);
  set (h, "deletefcn", delfcn);

  addlistener (h, "units", {@update_position, h});

  ## Now work with normalized coordinates
  if (! isempty (pos))
    set (h, "position", pos);
  endif
  pos = getnormpos (h);

  ## Build annotation object and its specific properties/updaters
  switch (objtype)
    case {"line", "arrow", "doublearrow", "textarrow"}
      ## Add properties
      proptable = lineprops ();
      if (strcmp (objtype, "arrow"))
        proptable = [proptable arrowprops()];
      elseif (strcmp (objtype, "doublearrow"))
        proptable = [proptable dblarrowprops()];
      elseif (strcmp (objtype, "textarrow"))
        proptable = [proptable arrowprops()];
        proptable = [proptable textprops()];
      endif

      addbaseprops (h, proptable);

      ## create line
      hli = line ([pos(1); (pos(1) + pos(3))],
                  [pos(2); (pos(2) + pos(4))],
                  "parent", h, "color", get (h, "color"),
                  "linestyle", get (h, "linestyle"),
                  "linewidth", get (h, "linewidth"));

      linemenu (hui, h);
      set (hli, "uicontextmenu", hui);

      ## create patch(s) and text
      if (strcmp (objtype, "arrow"))
        [x, y] = arrowcoordinates (h);
        hpa = patch (x, y, get (h, "color"), "parent", h,
                    "edgecolor",  get (h, "color"));
        update_arrow (h, {}, "position", hpa);

        arrowmenu (hui, h);
        set (hpa, "uicontextmenu", hui);

      elseif (strcmp (objtype, "doublearrow"))
        [x, y] = arrowcoordinates (h, 1);
        hpa = patch (x, y, get (h, "color"), "parent", h,
                    "edgecolor",  get (h, "color"));

        [x, y] = arrowcoordinates (h, 2);
        hpa(2) = patch (x, y, get (h, "color"), "parent", h,
                    "edgecolor",  get (h, "color"));

        update_arrow (h, {}, "position", hpa);

        dblarrowmenu (hui, h);
        set (hpa, "uicontextmenu", hui);

      elseif (strcmp (objtype, "textarrow"))
        [x, y] = arrowcoordinates (h);
        hpa = patch (x, y, get (h, "color"), "parent", h,
                    "edgecolor",  get (h, "color"));
        update_arrow (h, {}, "position", hpa);

        hte = text (get (h, "position")(1), ...
                   get (h, "position")(2), ...
                   get (h, "string"), "parent", h, ...
                   "color", get (h, "color"));
        propnames = textprops ("names");
        for ii = 1:numel (propnames)
          update_text (h, {}, propnames{ii}, hte);
        endfor
        update_text (h, {}, "position", hte);

        arrowmenu (hui, h);
        textmenu (hui, h);
        set (hpa, "uicontextmenu", hui);
        set (hte, "uicontextmenu", hui);

      endif

      ## updaters
      addlistener (h, "color", {@update_line, "color", hli});
      addlistener (h, "linestyle", {@update_line, "linestyle", hli});
      addlistener (h, "linewidth", {@update_line, "linewidth", hli});
      addlistener (h, "x", {@update_line, "x", hli});
      addlistener (h, "y", {@update_line, "y", hli});
      addlistener (h, "position", {@update_line, "position", hli});

      if (strcmp (objtype, "arrow"))
        addlistener (h, "position", {@update_arrow, "position", hpa});
        addlistener (h, "headwidth", {@update_arrow, "position", hpa});
        addlistener (h, "headstyle", {@update_arrow, "position", hpa});
        addlistener (h, "headlength", {@update_arrow, "position", hpa});
        addlistener (h, "color", {@update_arrow, "color", hpa});
      elseif (strcmp (objtype, "doublearrow"))
        addlistener (h, "position", {@update_arrow, "position", hpa});
        addlistener (h, "head1width",
                     {@update_arrow, "position", [hpa(1) 0]});
        addlistener (h, "head2width",
                     {@update_arrow, "position", [0 hpa(2)]});
        addlistener (h, "head1style",
                     {@update_arrow, "position", [hpa(1) 0]});
        addlistener (h, "head2style",
                     {@update_arrow, "position", [0 hpa(2)]});
        addlistener (h, "head1length",
                     {@update_arrow, "position", [hpa(1) 0]});
        addlistener (h, "head2length",
                     {@update_arrow, "position", [0 hpa(2)]});
        addlistener (h, "color",
                     {@update_arrow, "color", hpa});
      elseif (strcmp (objtype, "textarrow"))
        addlistener (h, "position", {@update_arrow, "position", hpa});
        addlistener (h, "headwidth", {@update_arrow, "position", hpa});
        addlistener (h, "headstyle", {@update_arrow, "position", hpa});
        addlistener (h, "headlength", {@update_arrow, "position", hpa});
        addlistener (h, "color", {@update_arrow, "color", hpa});
        propnames = textprops ("names");
        for ii = 1:numel (propnames)
          addlistener (h, propnames{ii},
                       {@update_text, propnames{ii}, hte});
          if (any (strcmp (propnames{ii},
                           {"fontangle", "fontname", ...
                            "fontsize", "fontweight", ...
                            "horizontalalignment", "string", ...
                            "textmargin", "textrotation", ...
                            "verticalalignment"})))
            addlistener (h, propnames{ii}, ...
                         {@update_text, "position", hte});
          endif
        endfor
        addlistener (h, "position", {@update_text, "position", hte});
        addlistener (h, "color", {@update_text, "color", hte});
      endif

    case {"rectangle", "ellipse"}

      ## Add properties
      addbaseprops (h, rectprops ());

      ## Create rectangle/ellipse
      if (strcmp (objtype, "rectangle"))
        [x, y] = pos2rect (pos);
      else
        [x, y] = pos2ell (pos);
      endif

      hr = patch (x, y, "parent", h);

      propnames = rectprops ("names");
      for ii = 1:numel (propnames)
        update_rect (h, {}, propnames{ii}, hr, objtype);
      endfor

      rectmenu (hui, h);
      set (hr, "uicontextmenu", hui);

      ## Updaters
      addlistener (h, "position", {@update_rect, "position", hr, objtype});
      for ii = 1:numel (propnames)
        addlistener (h, propnames{ii},
                     {@update_rect, propnames{ii}, hr, objtype});
      endfor

    case "textbox"

      ## Add properties
      addbaseprops (h, textboxprops());

      ## Create textbox
      hpa = patch ("parent", h);
      hte = text (pos(1), pos(2), get (h, "string"), "parent", h, ...
                 "color", get (h, "color"));
      update_textbox (h, {}, "position", [hte hpa]);

      propnames = textboxprops ("names");
      for ii = 1:numel (propnames)
        update_textbox (h, {}, propnames{ii}, [hte hpa]);
      endfor

      textboxmenu (hui, h);
      set (hpa, "uicontextmenu", hui);
      set (hte, "uicontextmenu", hui);

      ## Updaters
      addlistener (h, "position", {@update_textbox, "position", [hte hpa]});
      for ii = 1:numel (propnames)
        addlistener (h, propnames{ii},
                     {@update_textbox, propnames{ii}, [hte hpa]});
      endfor
      addlistener (h, "horizontalalignment",
                   {@update_textbox, "position", [hte hpa]});
      addlistener (h, "verticalalignment",
                   {@update_textbox, "position", [hte hpa]});

  endswitch

endfunction

function props = lineprops (varargin)
  ## FIXME: Use "axesx(y)lim" instead of "linex(y)data"
  props = {"color", "color", [0 0 0], ...
           "linestyle",  "linelinestyle", "-", ...
           "linewidth", "linelinewidth", 0.5, ...
           "x", "linexdata", [0.3 0.4], ...
           "y", "lineydata", [0.3 0.4]};
  if (strcmp (varargin, "names"))
    props = props(1:3:end);
  endif
endfunction

function col = basecolors ()
  col = {"blue", "black", "cyan", "green", "magenta", "red", ...
         "white", "yellow", "none"};
endfunction

function linemenu (hui, hpar)
  hm = uimenu ("parent", hui, "label", "Line");

  ## Color
  vals = basecolors ();
  addbasemenu (hm, hpar, "Color", vals);


  ## Linestyle
  vals = set (hpar, "linestyle");
  addbasemenu (hm, hpar, "Linestyle", vals);

  ## Linewidth
  vals = [0.5 1 1.5 2];
  addbasemenu (hm, hpar, "Linewidth", vals);
endfunction

function props = arrowprops (varargin)
  props = {"headlength", "data", 10, ...
           "headstyle",  "radio", "diamond|ellipse|none|plain|rectangle|vback1|{vback2}|vback3", ...
           "headwidth",  "data", 10};
  if (strcmp (varargin, "names"))
    props = props(1:3:end);
  endif
endfunction

function arrowmenu (hui, hpar)
  hm = uimenu ("parent", hui, "label", "Arrowhead");

  ## Headlength/width
  vals = 6:2:16;
  addbasemenu (hm, hpar, "headlength", vals, "Length");
  addbasemenu (hm, hpar, "headwidth", vals, "Width");

  ## Headstyle
  vals = set (hpar, "headstyle");
  addbasemenu (hm, hpar, "Headstyle", vals);
endfunction

function props = dblarrowprops (varargin)
  props = {"head1length", "data", 10, ...
           "head1style",  "radio", "diamond|ellipse|none|plain|rectangle|vback1|{vback2}|vback3", ...
           "head1width",  "data", 10, ...
           "head2length", "data", 10, ...
           "head2style",  "radio", "diamond|ellipse|none|plain|rectangle|vback1|{vback2}|vback3", ...
           "head2width", "data", 10};
  if (strcmp (varargin, "names"))
    props = props(1:3:end);
  endif
endfunction

function dblarrowmenu (hui, hpar)
  hm1 = uimenu ("parent", hui, "label", "Arrowhead #1");
  hm2 = uimenu ("parent", hui, "label", "Arrowhead #2");

  ## Headlength/width
  vals = 6:2:16;
  addbasemenu (hm1, hpar, "head1length", vals, "Length");
  addbasemenu (hm1, hpar, "head1width", vals, "Width");
  addbasemenu (hm2, hpar, "head2length", vals, "Length");
  addbasemenu (hm2, hpar, "head2width", vals, "Width");

  ## Headstyle
  vals = set (hpar, "head1style");
  addbasemenu (hm1, hpar, "head1style", vals, "Headstyle");
  addbasemenu (hm2, hpar, "head2style", vals, "Headstyle");
endfunction

function props = textprops (varargin)
  props = {"fontangle", "textfontangle", "normal", ...
           "fontname",  "textfontname", "Helvetica", ...
           "fontsize", "textfontsize", 10, ...
           "fontunits", "textfontunits", "points", ...
           "fontweight",  "textfontweight", "normal", ...
           "horizontalalignment", "texthorizontalalignment", "left", ...
           "interpreter", "textinterpreter", "tex", ...
           "string", "textstring", "", ...
           "textbackgroundcolor", "textbackgroundcolor", "none", ...
           "textcolor", "textcolor", "k", ...
           "textedgecolor", "textedgecolor", "none", ...
           "textlinewidth", "textlinewidth",0.5, ...
           "textmargin", "textmargin", 5, ...
           "textrotation", "textrotation", 0, ...
           "verticalalignment", "textverticalalignment", "middle"};
  if (strcmp (varargin, "names"))
    props = props(1:3:end);
  endif
endfunction

function stringdlg (hpar, prop)
  def = get (hpar, prop);
  if (iscell (def))
    prompt = arrayfun (@(n) sprintf ("Line #%d:", n), 1:numel (def),
                       "uniformoutput", false);
  else
    prompt = "";
    def = {def};
  endif

  cstr = inputdlg (prompt, prop, 1, def);

  if (! isempty (cstr))
    set (hpar, prop, cstr)
  endif
endfunction

function textmenu (hui, hpar)
  hm = uimenu ("parent", hui, "label", "Text");

  ## String;
  prop = "String";
  fcn = @() stringdlg (hpar, prop);
  uimenu (hm, "label", prop, "callback", fcn);

  ## Font properties
  prop = "textcolor";
  vals = basecolors ();
  addbasemenu (hm, hpar, prop, vals, "Color");
  prop = "fontsize";
  vals = 8:2:20;
  addbasemenu (hm, hpar, prop, vals, "Size");
  prop = "fontangle";
  vals = set (hpar, prop);
  addbasemenu (hm, hpar, prop, vals, "Angle");
  prop = "fontweight";
  vals = set (hpar, prop);
  addbasemenu (hm, hpar, prop, vals, "Weight");
  prop = "textrotation";
  vals = 0:90:270;
  addbasemenu (hm, hpar, prop, vals, "Rotation");

  prop = "horizontalalignment";
  vals = set (hpar, prop);
  addbasemenu (hm, hpar, prop, vals, "Horizontal Alignment", ...
               "separator", "on");
  prop = "verticalalignment";
  vals = set (hpar, prop);
  addbasemenu (hm, hpar, prop, vals, "Vertical Alignment");

  ## FIXME: Add text background properties when they are supported

  prop = "interpreter";
  vals = set (hpar, prop);
  addbasemenu (hm, hpar, prop, vals, "Interpreter", ...
               "separator", "on");

endfunction

function props = textboxprops (varargin)
  props = {"backgroundcolor", "patchfacecolor", "none", ...
           "color", "textcolor", [0 0 0], ...
           "edgecolor", "patchedgecolor", [0 0 0], ...
           "facealpha", "patchfacealpha", 1, ...
           "fontangle", "textfontangle", "normal", ...
           "fontname",  "textfontname", "Helvetica", ...
           "fontsize", "textfontsize", 10, ...
           "fontunits", "textfontunits", "points", ...
           "fontweight",  "textfontweight", "normal", ...
           "horizontalalignment", "texthorizontalalignment", "left", ...
           "interpreter", "textinterpreter", "tex", ...
           "linestyle",  "linelinestyle", "-", ...
           "linewidth", "linelinewidth", 0.5, ...
           "string", "textstring", "", ...
           "fitboxtotext", "radio","{on}|off", ...
           "margin", "data", 5, ...
           "verticalalignment", "textverticalalignment",  "middle"};
  if (strcmp (varargin, "names"))
    props = props(1:3:end);
  endif
endfunction

function textboxmenu (hui, hpar)
  ## Text properties
  hm1 = uimenu ("parent", hui, "label", "Text");

  prop = "String";
  fcn = @() stringdlg (hpar, prop);
  uimenu (hm1, "label", prop, "callback", fcn);

  prop = "Color";
  vals = basecolors ();
  addbasemenu (hm1, hpar, prop, vals);
  prop = "fontsize";
  vals = 8:2:20;
  addbasemenu (hm1, hpar, prop, vals, "Size");
  prop = "fontangle";
  vals = set (hpar, prop);
  addbasemenu (hm1, hpar, prop, vals, "Angle");
  prop = "fontweight";
  vals = set (hpar, prop);
  addbasemenu (hm1, hpar, prop, vals, "Weight");

  prop = "horizontalalignment";
  vals = set (hpar, prop);
  addbasemenu (hm1, hpar, prop, vals, "Horizontal Alignment", ...
               "separator", "on");
  prop = "verticalalignment";
  vals = set (hpar, prop);
  addbasemenu (hm1, hpar, prop, vals, "Vertical Alignment");
  prop = "Margin";
  vals = 2:2:10;
  addbasemenu (hm1, hpar, prop, vals);

  prop = "interpreter";
  vals = set (hpar, prop);
  addbasemenu (hm1, hpar, prop, vals, "Interpreter", ...
               "separator", "on");

  ## Background properties
  hm2 = uimenu ("parent", hui, "label", "Background");

  prop = "fitboxtotext";
  vals = set (hpar, prop);
  addbasemenu (hm2, hpar, prop, vals, "Fit box to text");
  prop = "backgroundcolor";
  vals = basecolors ();
  addbasemenu (hm2, hpar, prop, vals, "Face Color");
  prop = "edgecolor";
  vals = basecolors ();
  addbasemenu (hm2, hpar, prop, vals, "Edge Color");
  prop = "linestyle";
  vals = set (hpar, prop);
  addbasemenu (hm2, hpar, prop, vals, "Line Style");
  prop = "linewidth";
  vals = 0.5:.5:2;
  addbasemenu (hm2, hpar, prop, vals, "Line Width");
endfunction

function props = rectprops (varargin)
  props = {"edgecolor", "patchedgecolor", "k", ...
           "facealpha", "patchfacealpha", 1, ...
           "facecolor", "patchfacecolor", "none", ...
           "linestyle", "patchlinestyle", "-", ...
           "linewidth", "patchlinewidth", 0.5};
  if (strcmp (varargin, "names"))
    props = props(1:3:end);
  endif
endfunction

function rectmenu (hui, hpar)
  prop = "facecolor";
  vals = basecolors ();
  addbasemenu (hui, hpar, prop, vals, "Face Color");
  prop = "edgecolor";
  vals = basecolors ();
  addbasemenu (hui, hpar, prop, vals, "Edge Color");
  prop = "linestyle";
  vals = set (hpar, prop);
  addbasemenu (hui, hpar, prop, vals, "Line Style");
  prop = "linewidth";
  vals = 0.5:.5:2;
  addbasemenu (hui, hpar, prop, vals, "Line Width");
endfunction

function addbaseprops (h, proptable)
  cellfun (@(pname, ptype, parg) addproperty (pname, h, ptype, parg),
           proptable(1:3:end), proptable(2:3:end), proptable(3:3:end));
endfunction

function addbasemenu (hm, hpar, pname, vals, mainlabel = "" )
  if (isempty (mainlabel))
    mainlabel = pname;
  endif

  h = uimenu ("parent", hm, "label", mainlabel);

  is_numeric = ! iscell (vals);
  nv = numel (vals);
  htmp = zeros (1, nv);
  for ii = 1:nv
    if (! is_numeric)
      val = label = vals{ii};
    else
      val = vals(ii);
      label = num2str (val);
    endif

    fcn = @() set (hpar, pname, val);
    htmp(ii) = uimenu (h, "label", label, "callback", fcn);
  endfor
  handle_check (hpar, {}, htmp, pname, is_numeric);
  addlistener (hpar, pname, {@handle_check, htmp, pname, is_numeric})
endfunction

function handle_check (h, dummy, hmenus, prop, is_numeric)
  vals = get (hmenus, "label");
  current = get (h, prop);
  if (is_numeric)
    current = num2str (current);
  endif

  idx = strcmp (vals, current);
  set (hmenus(idx), "checked", "on");
  set (hmenus(! idx), "checked", "off");
endfunction

function update_position (h1, dummy, h, force = false)
  if (! force)
    pos = convertposition (h, getappdata (h, "__former_units__"),
                           get (h, "units"));
    setappdata (h, "__former_units__", get (h, "units"));
    set (h, "position", pos);
  else
    ## FIXME: Inefficient trick to force all objects to be redrawn
    set (h, "position", [0 0 .5 .5],
         "position", get (h, "position"));
  endif
endfunction


function update_line (h, dummy, prop, hli)
  persistent recursive = false;
  if (! recursive)
    switch (prop)
      case "color"
        set (hli, "color", get (h, "color"));
      case "linestyle"
        set (hli, "linestyle", get (h, "linestyle"));
      case "linewidth"
        set (hli, "linewidth", get (h, "linewidth"));
      case "x"
        ## Update position
        x = get (h, "x");
        pos = get (h, "position");
        pos(1) = x(1);
        pos(3) = diff (x);
        recursive = true;
        set (h, "position", pos);
        recursive = false;

        ## Draw in normalized coordinates
        pos = getnormpos (h);
        x = [pos(1) (pos(1) + pos(3))];
        set (hli, "xdata", x);

      case "y"
       ## Update position
        y = get (h, "y");
        pos = get (h, "position");
        pos(2) = y(1);
        pos(4) = diff (y);
        recursive = true;
        set (h, "position", pos);
        recursive = false;

        ## Draw in normalized coordinates
        pos = getnormpos (h);
        y = [pos(2) (pos(2) + pos(4))];
        set (hli, "ydata", y);
      case "position"
        ## Update x and y
        pos = get (h, "position");
        x = [pos(1) (pos(1) + pos(3))];
        y = [pos(2) (pos(2) + pos(4))];

        recursive = true;
        set (h, "x", x);
        set (h, "y", y);
        recursive = false;

        ## Draw in normalized coordinates
        pos = getnormpos (h);
        x = [pos(1) (pos(1) + pos(3))];
        y = [pos(2) (pos(2) + pos(4))];
        set (hli, "xdata", x);
        set (hli, "ydata", y);

    endswitch
  endif
endfunction

function [x, y] = arrowcoordinates (h, nar = [])
  pos = getnormpos (h);
  ppos = norm2pts (h, pos(3:4).');
  ang = angle (complex (ppos(1), ppos(2)));

  if (isempty (nar))
    ln = get (h, "headlength");   # in points
    wd = get (h, "headwidth");
    headstyle = get (h, "headstyle");
    pos = pos(1:2) .+ pos(3:4);
  elseif (nar == 1)
    ln = get (h, "head1length"); # in points
    wd = get (h, "head1width");
    headstyle = get (h, "head1style");
    pos = pos(1:2);
    ang += pi;
  elseif (nar == 2)
    ln = get (h, "head2length"); # in points
    wd = get (h, "head2width");
    headstyle = get (h, "head2style");
    pos = pos(1:2) .+ pos(3:4);
  else
    error ("annotation: %d, no such arrow number")
  endif

  switch (headstyle)
    case "diamond"
      x = [0 -ln/2 -ln -ln/2 0];
      y = [0 -wd/2 0 wd/2 0];
    case "ellipse"
      pts = linspace (0, 2*pi, 12);
      x = ln/2 * (cos (pts) - 1);
      y = wd/2 * sin (pts);
    case "rectangle"
      x = [0 0 -ln -ln 0];
      y = [wd/2 -wd/2 -wd/2 wd/2 wd/2];
    case "vback1"
      x = [0 -ln -0.85*ln -ln 0];
      y = [0 wd/2 0 -wd/2 0];
    case "vback2"
      x = [0 -ln -0.65*ln -ln 0];
      y = [0 wd/2 0 -wd/2 0];
    case "vback3"
      x = [0 -ln -0.2*ln -ln 0];
      y = [0 wd/2 0 -wd/2 0];
    case "plain"
      x = [0 -ln -ln -ln 0];
      y = [0 wd/2 0 -wd/2 0];
    case "none"
      x = [0 0 0];
      y = [0 0 0];
    otherwise
      error ("annotation: \"%s\" headstyle not implemented", headstyle)
  endswitch

  R = [cos(ang) -sin(ang);
       sin(ang) cos(ang)];
  XY = R * [x; y];
  XY = pts2norm (h, XY);
  XY = pos(1:2).' .+ XY;

  x = XY(1,:).';
  y = XY(2,:).';
endfunction

function update_arrow (h, dummy, prop, hpa = [])
  persistent recursive = false;

  nar = [];
  for ii = 1:numel (hpa)
    if (numel (hpa) == 2)
      nar = ii;
    endif
    if (hpa(ii))
      switch (prop)
        case "position"
          [x, y] = arrowcoordinates (h, nar);
          set (hpa(ii), "xdata", x, "ydata", y);
        case "color"
          set (hpa(ii), "facecolor", get (h, "color"));
          set (hpa(ii), "edgecolor", get (h, "color"));
      endswitch
    endif
  endfor
endfunction

function update_text (h, dummy, prop, hte)
  persistent recursive = false;

  if (! recursive)
    switch (prop)
      case "position"
        if (isempty (get (h, "string")))
          return
        endif

        pos = getnormpos (h);

        set (hte, "position", [textcoordinates(hte, pos) 0]);

      otherwise
        if (strncmp (prop, "text", 4))
          set (hte, prop(5:end), get (h, prop));
        else
          set (hte, prop, get (h, prop));
        endif
    endswitch
  endif
endfunction

function update_textbox (h, dummy, prop, htb)
  persistent recursive = false;

  hte = htb(1);
  hpa = htb(2);

  if (! recursive)
    switch (prop)
      case {"edgecolor", "facealpha",
            "linestyle", "linewidth"}
        set (hpa, prop, get (h, prop));
      case {"backgroundcolor"}
        set (hpa, "facecolor", get (h, prop));
      otherwise
        if (! any (strcmp (prop, {"fitboxtotext", "position"})))
          set (hte, prop, get (h, prop));
        endif

        pos = getnormpos (h);

        if (strcmp (get (h, "fitboxtotext"), "on"))
          pos(3:4) = get (hte, "extent")(3:4) .* sign (pos(3:4));
        endif

        [x, y] = pos2rect (pos);
        set (hpa, "xdata", x', "ydata", y');

        switch (get (h, "horizontalalignment"))
          case "left"
            x = x(1);
          case "center"
            x = mean (x(1:2));
          case "right"
            x = x(2);
        endswitch

        switch (get (h, "verticalalignment"))
          case {"top", "cap"}
            y = y(3);
          case "middle"
            y = mean (y(2:3));
          case {"bottom", "baseline"}
            y = y(2);
        endswitch
        set (hte, "position", [x y 0]);
    endswitch
  endif

endfunction

function XY = textcoordinates (hte, pos)
  ## Get the "tight" extent of the text object in points units
  textpos = get(hte, "position");
  rot = get (hte, "rotation");
  units = get (hte, "units");

  set (hte, "rotation", 0, "units", "points", "position", [0 0 0]);
  ext = get (hte, "extent");
  set (hte, "rotation", rot, "units", units, "position", textpos);

  ## Find which one of the 8 following points we should align the
  ## arrow with
  ##  8-----7-----6
  ##  1  text box 5
  ##  2-----3-----4

  ## FIXME: Matlab's horizontal/verticalalignment properties are
  ## interpreted differently: horizontalalignment is passed to the
  ## underlying text object whereas the verticalalignement controls
  ## the vertical alignment of the arrow.

  ang = angle (complex (pos(3), pos(4)));
  rot = rot / 180 * pi;

  [~, pt] = min (abs ((-pi:pi/4:pi) - ang));
  pt -= floor (rot / (pi/4));
  if (pt <= 0)
    pt = rem (pt, 8) + 8;
  elseif (pt > 8)
    pt = rem (pt, 8);
  endif

  ## Compute the text actual "position" property
  dx = ext(3)/2;
  dy = ext(4)/2;
  XY = [-dx -dx 0 dx dx dx 0 -dx;
        0 -dy -dy -dy 0 dy dy dy];

  switch (get (hte, "horizontalalignment"))
    case "left"
      XY(1,:) += dx;
    case "right"
      XY(1,:) -= dx;
  endswitch

  switch (get (hte, "verticalalignment"))
    case {"baseline", "bottom"}
      XY(2,:) += dy;
    case {"cap", "top"}
      XY(2,:) -= dy;
  endswitch

  R = [cos(rot) -sin(rot);
       sin(rot) cos(rot)];
  XY = R * XY;
  XY = pts2norm (get (hte, "parent"), XY);
  XY = pos(1:2) .- XY(:,pt).';

endfunction

function nXY = pts2norm (h, pXY)
  sz = get (get (h,"parent"), "figsize_points");

  nXY(1,:) = pXY(1,:) ./ sz(1);
  nXY(2,:) = pXY(2,:) ./ sz(2);
endfunction

function pXY = norm2pts (h, nXY)
  sz = get (get (h,"parent"), "figsize_points");

  pXY(1,:) = nXY(1,:) .* sz(1);
  pXY(2,:) = nXY(2,:) .* sz(2);
endfunction

function pos = convertposition (h, from, to)
  ## FIXME: handle "characters" units
  pos = get (h, "position");

  ## First convert to normalized coordinates
  sz = get (get (h,"parent"), "figsize_points");
  switch (from)
    case "centimeters"
      pos /= 2.54;
      pos *= 72;
      pos(1:2:end) /= sz(1);
      pos(2:2:end) /= sz(2);
    case "inches"
      pos *= 72;
      pos(1:2:end) /= sz(1);
      pos(2:2:end) /= sz(2);
    case "pixels"
      pos /= get (0, "screenpixelsperinch");
      pos *= 72;
      pos(1:2:end) /= sz(1);
      pos(2:2:end) /= sz(2);
  endswitch

  ## Then convert to requested coordinates
  switch (to)
    case "centimeters"
      sz /= 72;
      sz *= 2.54;
      pos(1:2:end) *= sz(1);
      pos(2:2:end) *= sz(2);
    case "inches"
      sz /= 72;
      pos(1:2:end) *= sz(1);
      pos(2:2:end) *= sz(2);
    case "pixels"
      sz /= 72;
      sz *= get (0, "screenpixelsperinch");
      pos(1:2:end) *= sz(1);
      pos(2:2:end) *= sz(2);
  endswitch

endfunction

function pos = getnormpos (h)
  units = get (h, "units");
  pos = convertposition (h, units, "normalized");
endfunction

function [x, y] = pos2rect (pos)
  x = [pos(1) pos(1)+pos(3) pos(1)+pos(3) pos(1)];
  y = [pos(2) pos(2) pos(2)+pos(4) pos(2)+pos(4)];
endfunction

function [x, y] = pos2ell (pos)
  a = pos(3)/2;
  b = pos(4)/2;

  ## Arbitrarily use 100 points
  ## when it is spread over
  ang = linspace (0, 2*pi, 100);

  x = a * cos (ang);
  y = b * sin (ang);

  x += pos(1) + a;
  y += pos(2) + b;
endfunction

function update_rect (h, dummy, prop, hre, typ)
  persistent recursive = false;

  if (! recursive)
    switch (prop)
      case "position"
        pos = getnormpos (h);
        if (strcmp (typ, "rectangle"))
          [x, y] = pos2rect (pos);
        else
          [x, y] = pos2ell (pos);
        endif

        set (hre, "xdata", x, "ydata", y);
      otherwise
        set (hre, prop, get (h, prop));
    endswitch
  endif
endfunction


## FIXME: the additionnal regular axes is necessary for fltk to draw the
##        annotation axes.
%!demo
%! clf; axes ('visible', 'off');
%! annotation ('textbox', [.25 .9 .5 .09], 'string', ...
%!             {'Right Click on annotation objects', ...
%!              'to customize their appearance'}, ...
%!             'horizontalalignment', 'center', 'fitboxtotext', 'off');
%! annotation ('ellipse', [.2 .2 .6 .6], 'linewidth', 4)
%! ang = pi/2:-pi/2:-pi;
%! lab = {'N', 'W', 'S', 'E'};
%! x0 = 0.5;
%! y0 = 0.5;
%! r = 0.3;
%! for ii = 1:4
%!   x = r * cos (ang(ii)) + x0;
%!   y = r * sin (ang(ii)) + y0;
%!   annotation ('textarrow', [x x0], [y y0], ...
%!               'string', lab{ii},  'fontsize', 20);
%! end
%!
%! h = annotation ('doublearrow', [x0 x0], [y0-r y0+r], ...
%!                 'head1style', 'diamond', 'head1length', 60, ...
%!                 'head2style', 'diamond', 'head2length', 60);

%!demo
%! clf; axes ('visible', 'off');
%! plot (1:10);
%! xlabel ('X-LABEL')
%! ylabel ('LARGE Y-LABEL', 'fontsize', 20)
%! title ('FIGURE LAYOUT', 'fontsize', 24)
%!
%! ti = get (gca, 'tightinset');
%! pos = get (gca, 'position');
%! pos(1:2) = pos(1:2) - ti(1:2);
%! pos(3) = pos(3) + ti (1) + ti (3);
%! pos(4) = pos(4) + ti (2) + ti (4);
%!
%! ht = annotation ('textbox', pos, 'string', ' Position + tighinset', ...
%!                  'fitboxtotext', 'off', 'linestyle', '--', ...
%!                  'edgecolor', 'g', 'linewidth', 3, 'color', 'g', ...
%!                  'verticalalignment', 'bottom', 'fontsize', 15);
%!
%! ho = annotation ('textbox', get (gca, 'outerposition'), ...
%!                  'string', ' Outerposition','fitboxtotext', 'off', ...
%!                  'linestyle', '--', 'edgecolor', 'r', ...
%!                  'linewidth', 3, 'color', 'r', ...
%!                  'verticalalignment', 'bottom', 'fontsize', 15);
%!
%! hi = annotation ('textbox', get (gca, 'position'), ...
%!                  'string', ' Position','fitboxtotext', 'off', ...
%!                  'linestyle', '--', 'edgecolor', 'b', ...
%!                  'linewidth', 3, 'color', 'b', ...
%!                  'verticalalignment', 'bottom', 'fontsize', 15);

%!demo
%! clf; axes ('visible', 'off');
%! h = annotation ('arrow');
%!
%! %% Get allowed headstyles
%! styles = set (h, 'headstyle');
%! delete (h)
%!
%! %% Textbox for the title
%! annotation ('textbox', [0.1 0 0.8 1], 'string', ...
%!             '"headstyle" property:', ...
%!             'backgroundcolor', [0.7 0.7 0.7], 'fontsize', 20, ...
%!             'fitboxtotext', 'off', 'verticalalignment', 'top', ...
%!             'horizontalalignment', 'center');
%!
%! %% Textarrows
%! ns = numel (styles);
%! nrows = ceil (ns/2);
%! dy = 1/nrows;
%! y = 1 - dy/2;
%!
%! jj = 1;
%! for ii = 1:nrows
%!   annotation ('textarrow', [0.3 0.5], [y y], ...
%!               'string', ['"' styles{jj} '"'], 'fontsize', 15, ...
%!               'headstyle', styles{jj}, 'textcolor', 'b');
%!   jj = jj + 1;
%!   if (jj <= ns)
%!     annotation ('textarrow', [0.7 0.5], [y y], ...
%!                 'string', ['"' styles{jj} '"'], 'fontsize', 15, ...
%!                 'headstyle', styles{jj}, 'textcolor', 'b');
%!   jj = jj + 1;
%!   end
%!   y = y - dy;
%! end
%! annotation ('line', [0.5 0.5], [dy/2 1-dy/2], 'linestyle', '-.')

%!demo
%! clf; axes ('visible', 'off');
%!
%! %% Textbox for the title
%! annotation ('textbox', [0.1 0 0.8 1], 'string', ...
%!             'Text arrows: text rotation', ...
%!             'backgroundcolor', [0.7 0.7 0.7], 'fontsize', 20, ...
%!             'fitboxtotext', 'off', 'verticalalignment', 'top', ...
%!             'horizontalalignment', 'center');
%!
%! %% Textarrows
%! for ii = 1:10
%!   rot = floor (rand (1) * 360 / 90) * 90;
%!   annotation ('textarrow', 0.5 + [(0.6 * (rand(1) - .5)) 0], ...
%!               0.5 + [(0.6 * (rand(1) - .5)) 0], ...
%!               'string', 'A text', ...
%!               'headstyle', 'none', 'textrotation', rot);
%! end

%!demo
%! clf; axes ('visible', 'off');
%!
%! %% Textbox for the title
%! annotation ('textbox', [0.1 0 0.8 1], 'string', ...
%!             'Text arrows: text alignment', ...
%!             'backgroundcolor', [0.7 0.7 0.7], 'fontsize', 20, ...
%!             'fitboxtotext', 'off', 'verticalalignment', 'top', ...
%!             'horizontalalignment', 'center');
%!
%! %% Textarrows
%! halig = {'right', 'center', 'left'};
%! ii = 1;
%! for x = .3:.2:.7
%!   annotation ('textarrow', [x .5], [.5 .9], ...
%!               'string', {'Multiple lines', 'text'}, ...
%!               'headstyle', 'none', 'horizontalalignment', halig{ii});
%!   ii = ii + 1;
%! end

%!demo
%! clf; axes ('visible', 'off');
%!
%! x = 0:0.01:2*pi;
%! y = sin (x);
%! plot (x, y)
%!
%! %% Extrema
%! x0 = [pi/2 3*pi/2];
%! y0 = [1 -1];
%!
%! %% Convert axes coordinates into normalized coordinates
%! xl = xlim ();
%! yl = [-1.2 1.5];
%! ylim (yl);
%! x0 = (x0 - xl(1)) / diff(xl);
%! y0 = (y0 - yl(1)) / diff(yl);
%!
%! pos = get (gca (), 'position');
%! x0 = x0*pos(3) + pos(1);
%! y0 = y0*pos(4) + pos(2);
%!
%!
%! %% Textarrows
%! for ii = 1:2
%!   annotation ('doublearrow', [(x0(ii) - .05) (x0(ii) + .05)], ...
%!               [y0(ii) y0(ii)], 'head1style', 'vback3', ...
%!               'head2style', 'vback3', ...
%!               'head1width', 4, 'head2width', 4)
%!   h = annotation ('textarrow', [0.5 x0(ii)], [.85 y0(ii)], ...
%!                   'linestyle', '--', 'headstyle', 'none');
%! end
%! set (h, 'string', 'Extrema', 'fontsize', 15)

## test line properties
%!test
%! hf = figure ("visible", "off");
%! hax = axes ();
%! unwind_protect
%!   h = annotation ("line", [0.2 0.7], [0.2 0.7], "linewidth", 2,
%!                   "linestyle", "--", "color", "r");
%!   hli = get (h, "children");
%!   assert (get (hli, "linewidth"), 2);
%!   assert (get (hli, "linestyle"), "--");
%!   assert (get (hli, "color"), [1 0 0]);
%!   assert (gca (), hax);
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect

## test textarrow properties
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = annotation ("textarrow", [0.2 0.7], [0.2 0.7],
%!                   "string", "Hello!", "fontsize", 20,
%!                   "textrotation", 90, "textcolor", "r");
%!   hte = findobj (h, "-depth", 1, "type", "text");
%!   assert (get (hte, "string"), "Hello!");
%!   assert (get (hte, "fontsize"), 20);
%!   assert (get (hte, "rotation"), 90);
%!   assert (get (hte, "color"), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect

## test textbox properties
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = annotation ("textbox", [0.2 0.2 0.7 0.3], "string", "Hello!",
%!                   "horizontalalignment", "left",
%!                   "verticalalignment", "bottom",
%!                   "backgroundcolor", "r");
%!   hte = findobj (h, "-depth", 1, "type", "text");
%!   hpa = findobj (h, "-depth", 1, "type", "patch");
%!   assert (get (hte, "string"), "Hello!");
%!   assert (get (hte, "verticalalignment"), "bottom");
%!   assert (get (hte, "horizontalalignment"), "left");
%!   assert (get (hpa, "facecolor"), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect

## test units conversion
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = annotation ("arrow", [0.1 0.2],  [0.5 0.2]);
%!   set (h, "units", "inches");
%!   pos = get (h, "position");
%!   x = get (h, "x");
%!   y = get (h, "y");
%!   set (h, "units", "centimeters");
%!   assert (get (h, "position"), pos * 2.54, 20*eps);
%!   assert (get (h, "x"), x * 2.54, 20*eps);
%!   assert (get (h, "y"), y * 2.54, 20*eps);
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect

## test annotated figure
%!test
%! hf1 = figure ("visible", "off");
%! hf2 = figure ("visible", "off");
%! unwind_protect
%!   h = annotation (hf1, "doublearrow");
%!   assert (ancestor (h, "figure"), hf1);
%!   assert (gcf (), hf2);
%! unwind_protect_cleanup
%!   close (hf1)
%!   close (hf2)
%! end_unwind_protect

## Test input validation
%!error <unknown annotation type foo> annotation ("foo")
%!error annotation ([], "foo")
%!error annotation ({})
%!error annotation ("line", [.5 .6])
%!error <expect 2 elements vectors for X and Y> annotation ("line", 1:3, 1:3)
%!error <expect 4 elements vector for POS> annotation ("textbox", 1:3)
