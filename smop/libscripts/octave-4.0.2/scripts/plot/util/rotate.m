## Copyright (C) 2014-2015 John W. Eaton
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
## @deftypefn  {Function File} {} rotate (@var{h}, @var{dir}, @var{alpha})
## @deftypefnx {Function File} {} rotate (@dots{}, @var{origin})
## Rotate the plot object @var{h} through @var{alpha} degrees around the line
## with direction @var{dir} and origin @var{origin}.
##
## The default value of @var{origin} is the center of the axes object that is
## the parent of @var{h}.
##
## If @var{h} is a vector of handles, they must all have the same parent axes
## object.
##
## Graphics objects that may be rotated are lines, surfaces, patches, and
## images.
## @end deftypefn

function rotate (h, direction, alpha, origin)

  ## Note in doc string about compatibility issues with calculation of
  ## default origin due to possible differences in the auto-scaling
  ## algorithm between Octave and Matlab.

  if (nargin < 3 || nargin > 4)
    print_usage ();
  endif

  is_h = ishandle (h);
  if (is_h)
    ax_list = get (h, "parent");
    if (iscell (ax_list))
      ax_list = cell2mat (ax_list);
    endif
    if (ax_list == ax_list(1))
      ax = ax_list(1);
    else
       error ("rotate: all handles must be children of the same axes object");
    endif
  else
    error ("rotate: H must be an array of one or more graphics handles");
  endif

  if (! (isnumeric (direction) && numel (direction) == 3))
    error ("rotate: invalid direction");
  endif

  if (! (isnumeric (alpha) && isscalar (alpha)))
    error ("rotate: invalid rotation angle");
  endif

  t = get (h, "type");

  is_image = strcmp (t, "image");
  is_line = strcmp (t, "line");
  is_patch = strcmp (t, "patch");
  is_surface = strcmp (t, "surface");

  if (! all (is_image | is_line | is_patch | is_surface))
    error ("rotate: expecting image, line, patch, or surface objects");
  endif

  if (nargin == 4)
    if (! (isnumeric (origin) && numel (origin) == 3))
       error ("rotate: invalid origin");
    endif
  else
    ## Should Z limit be considered when computing origin?

    use_zlim = any (is_patch | is_surface);

    if (! use_zlim && any (is_line))
      idx = find (is_line)';
      for i = idx
        if (! isempty (get (h(i), "zdata")))
          use_zlim = true;
          break;
        endif
      endfor
    endif

    xlim = get (ax, "xlim");
    ylim = get (ax, "ylim");

    a = (xlim(1) + xlim(2)) / 2;
    b = (ylim(1) + ylim(2)) / 2;

    if (use_zlim)
      zlim = get (ax, "zlim");
      c = (zlim(1) + zlim(2)) / 2;
    else
      c = 0;
    endif

    origin = [a, b, c];
  endif

  direction = direction / norm (direction);

  u = direction(1);
  v = direction(2);
  w = direction(3);

  a = origin(1);
  b = origin(2);
  c = origin(3);

  sa = sind (alpha);
  ca = cosd (alpha);

  for i = 1:numel (h)
    x = get (h(i), "xdata");
    y = get (h(i), "ydata");

    if (is_image(i))
      z = zeros (size (x));
    else
      z = get (h(i), "zdata");
      if (isempty (z))
        z = zeros (size (x));
      elseif (isvector (x) && isvector (y) && ! isvector (z))
        [x, y] = meshgrid (x, y);
      endif
    endif

    if (a == 0 && b == 0 && c == 0)
      tmp = (u*x + v*y + w*z) * (1 - ca);

      xr = u*tmp + x*ca + (-w*y + v*z)*sa;
      yr = v*tmp + y*ca + (w*x - u*z)*sa;
      zr = w*tmp + z*ca + (-v*x + u*y)*sa;
    else
      one_m_ca = 1 - ca;
      tmp = u*x + v*y + w*z;

      xr = ((a*(v**2 + w**2) - u*(b*v + c*w - tmp))*one_m_ca
            + x*ca + (-c*v + b*w - w*y + v*z)*sa);
      yr = ((b*(u**2 + w**2) - v*(a*u + c*w - tmp))*one_m_ca
            + y*ca + (c*u - a*w + w*x - u*z)*sa);
      zr = ((c*(u**2 + v**2) - w*(a*u + b*v - tmp))*one_m_ca
            + z*ca + (-b*u + a*v - v*x + u*y)*sa);
    endif

    set (h(i), "xdata", xr, "ydata", yr);

    if (! is_image(i))
      set (h(i), "zdata", zr);
    endif
  endfor

endfunction

## Test input validation
%!shared h1, h2, o1, o2, o3
%! h1 = figure ("visible", "off");
%! o1 = line ();
%! h2 = figure ("visible", "off");
%! o2 = line ();
%! o3 = text (0, 0, "foobar");
%!error rotate ()
%!error rotate (o1)
%!error rotate (o1, [0,0,0]);
%!error <all handles must be children of the same axes object> rotate ([o1, o2], [0,0,0], 90);
%!error <invalid direction> rotate (o1, "foo", 90);
%!error <invalid rotation angle> rotate (o1, [0,0,0], "foo");
%!error <invalid origin> rotate (o1, [0,0,0], 90, "foo");
%!error rotate (o1, [0,0,0], 90, [0,0,0], 1);
%!error <H must be an array of one or more graphics handles> rotate (NaN, [0,0,0], 90);
%!error <expecting image, line, patch, or surface objects> rotate (o3, [0,0,0], 90);
%!test
%! close (h1);
%! close (h2);
