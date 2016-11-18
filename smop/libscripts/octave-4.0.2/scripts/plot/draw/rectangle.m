## Copyright (C) 2012-2015 David Bateman
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
## @deftypefn  {Function File} {} rectangle ()
## @deftypefnx {Function File} {} rectangle (@dots{}, "Position", @var{pos})
## @deftypefnx {Function File} {} rectangle (@dots{}, "Curvature", @var{curv})
## @deftypefnx {Function File} {} rectangle (@dots{}, "EdgeColor", @var{ec})
## @deftypefnx {Function File} {} rectangle (@dots{}, "FaceColor", @var{fc})
## @deftypefnx {Function File} {} rectangle (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} rectangle (@dots{})
## Draw a rectangular patch defined by @var{pos} and @var{curv}.
##
## The variable @code{@var{pos}(1:2)} defines the lower left-hand corner of
## the patch and @code{@var{pos}(3:4)} defines its width and height.  By
## default, the value of @var{pos} is @code{[0, 0, 1, 1]}.
##
## The variable @var{curv} defines the curvature of the sides of the rectangle
## and may be a scalar or two-element vector with values between 0 and 1.
## A value of 0 represents no curvature of the side, whereas a value of 1
## means that the side is entirely curved into the arc of a circle.
## If @var{curv} is a two-element vector, then the first element is the
## curvature along the x-axis of the patch and the second along y-axis.
##
## If @var{curv} is a scalar, it represents the curvature of the shorter of the
## two sides of the rectangle and the curvature of the other side is defined
## by
##
## @example
## min (pos(1:2)) / max (pos(1:2)) * curv
## @end example
##
## Additional property/value pairs are passed to the underlying patch command.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## rectangle object.
## @end deftypefn
## @seealso{patch, line, cylinder, ellipsoid, sphere}

function h = rectangle (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("rectangle", varargin{:});

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    htmp = __rectangle__ (hax, varargin{:});
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif
endfunction

function hg = __rectangle__ (hax, varargin)

  iarg = 1;
  pos = [0, 0, 1, 1];
  curv2 = [0, 0];
  ec = [0, 0, 0];
  fc = "none";
  parent = [];

  while (iarg < length (varargin))
    arg = varargin{iarg};
    if (ischar (arg))
      if (strcmpi (arg, "position"))
        pos = varargin{iarg+1};
        varargin(iarg:iarg+1) = [];
        if (! isvector (pos) || numel (pos) != 4)
          error ("rectangle: position must be a 4 element vector");
        endif
      elseif (strcmpi (arg, "curvature"))
        curv2 = varargin{iarg+1};
        varargin(iarg:iarg+1) = [];
        if (! isnumeric (curv2) || (numel (curv2) != 1 && numel (curv2) != 2))
          error ("rectangle: curvature must be a 2-element vector or a scalar");
        endif
        if (any (curv2 < 0) || any (curv2 > 1))
          error ("rectangle: curvature values must be between 0 and 1");
        endif
      elseif (strcmpi (arg, "edgecolor"))
        ec = varargin{iarg+1};
        varargin(iarg:iarg+1) = [];
      elseif (strcmpi (arg, "facecolor"))
        fc = varargin{iarg+1};
        varargin(iarg:iarg+1) = [];
      elseif (strcmpi (arg, "parent"))
        parent = varargin{iarg+1};
        varargin(iarg:iarg+1) = [];
      else
        iarg ++;
      endif
    else
      iarg ++;
    endif
  endwhile

  if (numel (curv2) == 1)
    [a, ai] = min (pos(3:4));
    [b, bi] = max (pos(3:4));
    if (ai < bi)
      curv = [curv2, curv2 .* a ./ b];
    else
      curv = [curv2 .* a ./ b, curv2];
    endif
  else
    curv = curv2;
  endif

  if (all (curv < 0.01))
    ## Special case : no curvature
    x = [pos(1), pos(1) + pos(3), pos(1) + pos(3), pos(1), pos(1)];
    y = [pos(2), pos(2), pos(2) + pos(4), pos(2) + pos(4), pos(2)];
  else
    p = pi / 2 * [0 : 15] / 15;
    c = curv .* pos(3:4) / 2;
    cx = c(1) * sin (p) - c(1);
    cy = c(2) * cos (p) - c(2);
    x = [pos(1) - fliplr(cx), pos(1) + pos(3) + cx, ...
         pos(1) + pos(3) + fliplr(cx), pos(1) - cx, pos(1)];
    y = [pos(2) - fliplr(cy), pos(2) - cy, pos(2) + pos(4) + fliplr(cy), ...
         pos(2) + pos(4) + cy, pos(2) + c(2)];
  endif

  if (! isempty (parent))
    hg = hggroup ("parent", parent);
  else
    hg = hggroup ("parent", hax);
  endif

  h = patch ("xdata", x(:), "ydata", y(:), "facecolor", fc, "edgecolor", ec,
             "parent", hg, varargin{:});

  addproperty ("curvature", hg, "data", curv2);
  addproperty ("position",  hg, "data", pos);
  addproperty ("edgecolor", hg, "patchedgecolor", get (h, "edgecolor"));
  addproperty ("linewidth", hg, "patchlinewidth", get (h, "linewidth"));
  addproperty ("linestyle", hg, "patchlinestyle", get (h, "linestyle"));
  addproperty ("facecolor", hg, "patchfacecolor", get (h, "facecolor"));

  addlistener (hg, "curvature", @update_data);
  addlistener (hg, "position",  @update_data);
  addlistener (hg, "edgecolor", @update_props);
  addlistener (hg, "linewidth", @update_props);
  addlistener (hg, "linestyle", @update_props);
  addlistener (hg, "facecolor", @update_props);
endfunction

function update_data (h, ~)
  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;

      kids = get (h, "children");
      pos = get (h, "position");
      curv2 = get (h, "curvature");

      if (numel (curv2) == 1)
        [a, ai] = min (pos(3:4));
        [b, bi] = max (pos(3:4));
        if (ai < bi)
          curv = [curv2, curv2 .* a ./ b];
        else
          curv = [curv2 .* a ./ b, curv2];
        endif
      else
        curv = curv2;
      endif

      if (all (curv < 0.01))
        ## Special case : no curvature
        x = [pos(1), pos(1) + pos(3), pos(1) + pos(3), pos(1), pos(1)];
        y = [pos(2), pos(2), pos(2) + pos(4), pos(2) + pos(4), pos(2)];
      else
        p = pi / 2 * [0 : 15] / 15;
        c = curv .* pos(3:4) / 2;
        cx = c(1) * sin (p) - c(1);
        cy = c(2) * cos (p) - c(2);
        x = [pos(1) - fliplr(cx), pos(1) + pos(3) + cx, ...
             pos(1) + pos(3) + fliplr(cx), pos(1) - cx, pos(1)];
        y = [pos(2) - fliplr(cy), pos(2) - cy, pos(2) + pos(4) + fliplr(cy), ...
             pos(2) + pos(4) + cy, pos(2) + c(2)];
      endif

      set (kids, "xdata", x, "ydata", y);
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif
endfunction

function update_props (h, ~)
  kids = get (h, "children");
  set (kids, {"edgecolor", "linewidth", "linestyle", "facecolor"},
     get (h, {"edgecolor", "linewidth", "linestyle", "facecolor"}));

endfunction


%!demo
%! clf;
%! axis equal;
%! rectangle ('Position', [0.05, 0.05, 0.9, 0.9], 'Curvature', [0.5, 0.5]);
%! title ('rectangle() with corners curved');

%!demo
%! clf;
%! axis equal;
%! rectangle ('Position', [0.05, 0.05, 0.9, 0.4], 'Curvature', 1.0);
%! title ('rectangle() with sides as complete arcs');

%!demo
%! clf;
%! axis equal;
%! h = rectangle ('Position', [0.05, 0.05, 0.9, 0.4], 'Curvature', 1.0);
%! set (h, 'FaceColor', [0, 1, 0]);
%! title ('rectangle() with FaceColor = green');

