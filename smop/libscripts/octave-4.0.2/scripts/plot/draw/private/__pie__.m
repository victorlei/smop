## Copyright (C) 2007-2015 David Bateman
## Copyright (C) 2010 Kai Habel
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
## @deftypefn {Function File} {@var{hlist} =} __pie__ (caller, @dots{})
## Undocumented internal function.
## @end deftypefn

function hlist = __pie__ (caller, varargin)

  h = varargin{1};
  x = abs (varargin{2});
  iarg = 3;

  if (! isvector (x))
    error ("%s: expecting vector argument", caller);
  endif

  len = length (x);

  have_explode = false;
  have_labels = false;

  while (iarg <= nargin - 1)
    arg = varargin{iarg++};
    if (iscell (arg))
      labels = arg;
      have_labels = true;
      if (numel (x) != numel (labels))
        error ("%s: mismatch in number of labels and data", caller);
      endif
    elseif (isnumeric (arg) || islogical (arg))
      explode = arg;
      have_explode = true;
      if (! size_equal (x, explode))
        error ("%s: mismatch in number of elements in explode and data",
               caller);
      endif
    else
      error ("%s: %s is invalid as an optional argument", caller, class (arg));
    endif
  endwhile

  if (! have_explode)
    explode = zeros (size (x));
  endif

  normalize = true;
  if (sum (x(:)) < 1)
    normalize = false;
  endif

  if (! have_labels)
    if (normalize)
      xp = round (100 * x ./ sum (x));
    else
      xp = round (100 * x);
    endif
    for i = 1:len
      labels{i} = sprintf ("%d%%", xp(i));
    endfor
  endif

  hlist = [];
  refinement = 90;
  phi = 0:refinement:360;
  if (normalize)
    xphi = cumsum (x / sum (x) * 360);
  else
    xphi = cumsum (x * 360);
  endif

  for i = 1:len
    if (i == 1)
      xn = 0 : 360 / refinement : xphi(i);
    else
      xn = xphi(i-1) : 360 / refinement : xphi(i);
    endif

    if (xn(end) != xphi(i))
      xn = [xn, xphi(i)];
    endif

    xn2 = (xn(1) + xn(end)) / 2;
    if (explode (i))
      xoff = - 0.1 * sind (xn2);
      yoff = 0.1 * cosd (xn2);
    else
      xoff = 0;
      yoff = 0;
    endif
    xt = - 1.2 * sind (xn2);
    yt = 1.2 * cosd (xn2);

    if (len == 1)
      set (h, "clim", [1, 2]);
    else
      set (h, "clim", [1, len]);
    endif

    if (strcmp (caller, "pie3"))
      ln = length (xn);
      zlvl = 0.35;
      sx = repmat (xoff + [0, -sind(xn), 0], [2, 1]);
      sy = repmat (yoff + [0, cosd(xn), 0], [2, 1]);
      sz = [zeros(1, ln + 2); zlvl * ones(1, ln + 2)];
      sc = i * ones (size (sz));

      hlist = [hlist;
        patch(xoff + [0, -sind(xn)], yoff + [0, cosd(xn)], zeros (1, ln + 1), i);
        surface(sx, sy, sz, sc);
        patch(xoff + [0, -sind(xn)], yoff + [0, cosd(xn)], zlvl * ones (1, ln + 1), i);
        text(xt, yt, zlvl, labels{i})];

    elseif (strcmp (caller, "pie"))
      if (xt > 0)
        align = "left";
      else
        align = "right";
      endif

      hlist = [hlist; patch(xoff + [0, -sind(xn)], yoff + [0, cosd(xn)], i);
               text(xt, yt, labels{i}, "horizontalalignment", align)];

    else
      error ("__pie__: unknown caller '%s'", caller);
    endif
  endfor

  addlistener (gca, "view", {@update_text_pos, hlist});

  if (strcmp (caller, "pie3"))
    axis ([-1.25, 1.25, -1.25, 1.25, -0.05, 0.4], "equal", "off");
    view (-37.5, 30);
  elseif (strcmp (caller, "pie"))
    axis ([-1.5, 1.5, -1.5, 1.5], "square", "off");
  endif
endfunction

function update_text_pos (all_handles)
  ## Text objects in the foreground should be at the base level.
  ## Text objects in the background should be at the top level.
  ## Text objects on the right side should be aligned to the right
  ## and on the left side to the left.
  tobj = findobj (all_handles, "type", "text");

  ## check if we are called from pie3
  s = findobj (all_handles, "type", "surface");
  is_pie3 = false;
  if (length (s) > 0)
    is_pie3 = true;
  endif

  if (length (tobj) > 0)
    ax = get (tobj(1), "parent");
    azel = get (ax, "view");
    pos = get (tobj, "position");
    if (iscell (pos))
      pos = cell2mat (pos);
    endif
    phi = atand (pos(:,1) ./ pos(:,2));
    [theta, r] = cart2pol (pos(:,1), pos(:,2));
    theta *= 180/pi;
    theta -= azel(1);
    theta = mod (theta, 360);
    ud_mask = (theta > 180);
    lr_mask = (theta > 90) & (theta < 270);
    for i = 1 : length (tobj)
      if (is_pie3)
        if (ud_mask(i))
          set (tobj(i), "position", [pos(i,1), pos(i,2), -0.05]);
        else
          set (tobj(i), "position", [pos(i,1), pos(i,2), 0.40]);
        endif
      endif

      if (lr_mask(i))
        set (tobj(i), "horizontalalignment", "right");
      else
        set (tobj(i), "horizontalalignment", "left");
      endif
    endfor
  endif
endfunction

