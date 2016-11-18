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
## @deftypefn {Function File} {@var{h} =} __clabel__ (@var{c}, @var{v}, @var{hparent}, @var{label_spacing}, @var{z}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

function h = __clabel__ (c, v, hparent, label_spacing, z, varargin)

  hax = ancestor (hparent, "axes");
  units = get (hax, "units");
  set (hax, "units", "points");
  axpos = get (hax, "position");
  set (hax, "units", units);
  lims = axis ();
  xspacing = axpos(3) / (lims(2) - lims (1));
  yspacing = axpos(4) / (lims(4) - lims (3));

  if (isscalar (hparent) && ishandle (hparent)
      && strcmp (get (hparent, "type"), "hggroup"))
    x = get (hparent, "xdata");
    xmin = min (x(:));
    xmax = max (x(:));
    y = get (hparent, "ydata");
    ymin = min (y(:));
    ymax = max (y(:));
  else
    xmin = xmax = ymin = ymax = NaN;
    i = 1;
    while (i < length (c))
      clen = c(2,i);
      data = c(:, i+(1:clen));

      xmin = min ([xmin, data(1,:)]);
      xmax = max ([xmax, data(1,:)]);
      ymin = min ([ymin, data(2,:)]);
      ymax = max ([ymax, data(2,:)]);

      i += clen+1;
    endwhile
  endif

  ## Decode contourc output format and place labels.
  h = [];
  i = 1;
  while (i < length (c))
    clev = c(1,i);
    clen = c(2,i);

    if (! isempty (v) && ! any (v == clev))
      i += clen+1;
      continue;
    endif

    p = bsxfun (@times, c(:, i+(1:clen)), [xspacing; yspacing]);
    d = sqrt (sumsq (diff (p, 1, 2)));
    cumd = cumsum (d);
    td = cumd(end);
    ntag = ceil (td / label_spacing);

    if (all (c(:,i+1) == c(:,i+clen)))
      ## Closed contour
      ## FIXME: This spreads the tags uniformly around the contour which
      ## looks nice, but it does not respect the label_spacing attribute.
      ## Should we follow user input, which can result in two labels being
      ## quite close to each other?
      spacing = td / ntag;
      pos = spacing/2 + spacing*[0:ntag-1];
    else
      ## Open contour
      pos = zeros (1, ntag);
      pos(1) = (td - label_spacing*(ntag - 1)) / 2;
      pos(2:ntag) = pos(1) + label_spacing*[1:ntag-1];
    endif

    tlabel = sprintf ("%.5g", clev);

    for tagpos = pos

      j = find (cumd > tagpos, 1);
      if (isempty (j))
        j = clen;
      endif
      tpos = sum (c(:,i+j-1:i+j), 2) / 2;

      if (   tpos(1) != xmin && tpos(1) != xmax
          && tpos(2) != ymin && tpos(2) != ymax)
        trot = 180 / pi * atan2 (diff (c(2,i+j-1:i+j)),
                                 diff (c(1,i+j-1:i+j)));
        if (abs (trot) > 90)
          trot += 180;
        endif
        if (ischar (z))
          ht = text (tpos(1), tpos(2), clev, tlabel, "rotation", trot,
                     "horizontalalignment", "center", "userdata", clev,
                     "parent", hparent, varargin{:});
        elseif (! isempty (z))
          ht = text (tpos(1), tpos(2), z, tlabel, "rotation", trot,
                     "horizontalalignment", "center", "userdata", clev,
                     "parent", hparent, varargin{:});
        else
          ht = text (tpos(1), tpos(2), tlabel, "rotation", trot,
                     "horizontalalignment", "center", "userdata", clev,
                     "parent", hparent, varargin{:});
        endif
        h = [h; ht];
      endif
    endfor
    i += clen+1;
  endwhile

endfunction


## No test needed for internal helper function.
%!assert (1)

