## Copyright (C) 2007-2015 Michael Goffioul
## Copyright (C) 2007-2009 David Bateman
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
## @deftypefn  {Function File} {} area (@var{y})
## @deftypefnx {Function File} {} area (@var{x}, @var{y})
## @deftypefnx {Function File} {} area (@dots{}, @var{lvl})
## @deftypefnx {Function File} {} area (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} area (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} area (@dots{})
## Area plot of the columns of @var{y}.
##
## This plot shows the contributions of each column value to the row sum.
## It is functionally similar to @code{plot (@var{x}, cumsum (@var{y}, 2))},
## except that the area under the curve is shaded.
##
## If the @var{x} argument is omitted it defaults to @code{1:rows (@var{y})}.
## A value @var{lvl} can be defined that determines where the base level of
## the shading under the curve should be defined.  The default level is 0.
##
## Additional property/value pairs are passed directly to the underlying patch
## object.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the hggroup
## object comprising the area patch objects.  The @qcode{"BaseValue"} property
## of the hggroup can be used to adjust the level where shading begins.
##
## Example: Verify identity sin^2 + cos^2 = 1
##
## @example
## @group
## t = linspace (0, 2*pi, 100)';
## y = [sin(t).^2, cos(t).^2];
## area (t, y);
## legend ("sin^2", "cos^2", "location", "NorthEastOutside");
## @end group
## @end example
## @seealso{plot, patch}
## @end deftypefn

function h = area (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("area", varargin{:});

  if (nargin == 0)
    print_usage ();
  endif

  x = y = [];
  bv = 0;

  num_numeric = find (cellfun ("isclass", varargin, "char"), 1) - 1;
  if (isempty (num_numeric))
    num_numeric = nargin;
  endif

  switch (num_numeric)
    case 1
      y = varargin{1};
    case 2
      if (isscalar (varargin{2}))
        y = varargin{1};
        bv = varargin{2};
      else
        x = varargin{1};
        y = varargin{2};
      endif
    case 3
      x = varargin{1};
      y = varargin{2};
      bv = varargin{3};
    otherwise
      print_usage ();
  endswitch

  if (! isreal (x) || ! isreal (y))
    error ("area: X and Y must be real vectors or matrices");
  endif
  if (! isreal (bv) || ! isscalar (bv))
    error ("area: LVL must be a real scalar");
  endif

  if (isvector (y))
    y = y(:);
  endif
  if (isempty (x))
    x = repmat ([1:rows(y)]', 1, columns (y));
  elseif (isvector (x))
    x = repmat (x(:), 1, columns (y));
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    htmp = __area__ (hax, x, y, bv, varargin{num_numeric+1:end});
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction

function retval = __area__ (ax, x, y, bv, varargin)

  y0 = bv * ones (1, rows (y));
  y0 = zeros (1, rows (y));
  retval = [];
  for i = 1: columns (y);

    lc = __next_line_color__ ();

    ## Must occur after __next_line_color__ in order to work correctly.
    hg = hggroup ();
    retval = [retval; hg];
    args = __add_datasource__ ("area", hg, {"x", "y"}, varargin{:});

    x1 = x(:, 1)';
    y1 = y(:, i)';
    addproperty ("xdata", hg, "data", x1);
    addproperty ("ydata", hg, "data", y1);

    addlistener (hg, "xdata", @update_data);
    addlistener (hg, "ydata", @update_data);

    if (i == 1)
      h = patch (ax, [x1(1), x1, fliplr(x1)], [bv, y1, bv*ones(1, length(y1))],
                     lc, "parent", hg);
    else
      y1 = y0 + y1;
      h = patch (ax, [x1(1), x1, fliplr(x1)], [y0(1), y1, fliplr(y0)],
                     lc, "parent", hg);
    endif

    y0 = y1;

    addproperty ("basevalue", hg, "data", bv);
    addlistener (hg, "basevalue", @move_baseline);

    addproperty ("edgecolor", hg, "patchedgecolor", get (h, "edgecolor"));
    addproperty ("facecolor", hg, "patchfacecolor", get (h, "facecolor"));
    addproperty ("linestyle", hg, "patchlinestyle", get (h, "linestyle"));
    addproperty ("linewidth", hg, "patchlinewidth", get (h, "linewidth"));

    addlistener (hg, "edgecolor", @update_props);
    addlistener (hg, "facecolor", @update_props);
    addlistener (hg, "linestyle", @update_props);
    addlistener (hg, "linewidth", @update_props);

    addproperty ("areagroup", hg, "data");
    set (retval, "areagroup", retval);

    ## Matlab property, although Octave does not implement it.
    addproperty ("hittestarea", hg, "radio", "on|{off}", "off");

    if (! isempty (args))
      set (hg, args{:});
    endif
  endfor

endfunction

function update_props (h, d)
  kids = get (h, "children");
  set (kids, "edgecolor", get (h, "edgecolor"),
             "facecolor", get (h, "facecolor"),
             "linestyle", get (h, "linestyle"),
             "linewidth", get (h, "linewidth"));
endfunction

function move_baseline (h, d)
  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      hlist = get (h, "areagroup");
      b0 = get (h, "basevalue");

      for hh = hlist(:)'
        if (hh != h)
          b1 = get (hh, "basevalue");
          if (b1 != b0)
            set (hh, "basevalue", b0);
          endif
        endif
      endfor
      update_data (h, d);
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif
endfunction

function update_data (h, d)
  hlist = get (h, "areagroup");
  bv = get (h, "basevalue");
  for i = 1 : length (hlist)
    hh = hlist(i);
    x1 = get (hh, "xdata")(:);
    y1 = get (hh, "ydata")(:);

    set (get (hh, "children"), "xdata", [x1(1); x1; flipud(x1)]);
    if (i == 1)
      set (get (hh, "children"), "ydata", [bv; y1; bv*ones(length(y1), 1)]);
    else
      y1 = y0 + y1;
      set (get (hh, "children"), "ydata", [y0(1); y1; flipud(y0)]);
    endif

    y0 = y1;
  endfor
endfunction


%!demo
%! ## Verify identity sin^2 + cos^2 = 1
%! clf;
%! t = linspace (0, 2*pi, 100)';
%! y = [sin(t).^2, cos(t).^2];
%! area (t, y);
%! axis tight
%! legend ('sin^2', 'cos^2', 'location', 'NorthEastOutside');
%! title ('area() plot');

%!demo
%! ## Show effects of setting BaseValue
%! clf;
%! x = [-2:0.1:2]';
%! y = x.^2 - 1;
%! subplot (1, 2, 1)
%! area (x, y);
%! title ({'Parabola y = x^2 -1';'BaseValue = 0'});
%! subplot (1, 2, 2)
%! h = area (x, y);
%! set (h, 'basevalue', -1);
%! title ({'Parabola y = x^2 -1';'BaseValue = -1'});

%!demo
%! clf;
%! x = 0:10;
%! y = rand (size (x));
%! h = area (x, y);
%! set (h, 'ydata', sort (get (h, 'ydata')))
%! title ('area() plot of sorted data');

## Test input validation
%!error area ()
%!error area (1,2,3,4)
%!error <X and Y must be real vectors or matrices> area ({1})
%!error <X and Y must be real vectors or matrices> area (1+i)
%!error <X and Y must be real vectors or matrices> area (1:2, {1, 2})
%!error <X and Y must be real vectors or matrices> area (1:2, [1 1+i])
%!error <LVL must be a real scalar> area (1, i)
%!error <LVL must be a real scalar> area (1, 2, ones (2,2))

