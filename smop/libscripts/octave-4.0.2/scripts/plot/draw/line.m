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
## @deftypefn  {Function File} {} line ()
## @deftypefnx {Function File} {} line (@var{x}, @var{y})
## @deftypefnx {Function File} {} line (@var{x}, @var{y}, @var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} line (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} line (@var{x}, @var{y}, @var{z}, @var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} line (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} line (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} line (@dots{})
## Create line object from @var{x} and @var{y} (and possibly @var{z}) and
## insert in the current axes.
##
## Multiple property-value pairs may be specified for the line object, but they
## must appear in pairs.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle (or vector of handles)
## to the line objects created.
##
## @seealso{image, patch, rectangle, surface, text}
## @end deftypefn

## Author: jwe

function h = line (varargin)

  ## Get axis argument which may be in a 'parent' PROP/VAL pair
  [hax, varargin] = __plt_get_axis_arg__ ("line", varargin{:});

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  htmp = __line__ (hax, varargin{:});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf
%! line ([0 1], [0.8 0.8], 'linestyle', '-', 'color', 'b');
%! line ([0 1], [0.6 0.6], 'linestyle', '--', 'color', 'g');
%! line ([0 1], [0.4 0.4], 'linestyle', ':', 'color', 'r');
%! line ([0 1], [0.2 0.2], 'linestyle', '-.', 'color', 'k');
%! ylim ([0 1]);
%! title ('line() with various linestyles');
%! legend ('"-"', '"--"', '":"', '"-."', 'location', 'eastoutside');

%!demo
%! clf
%! x = 0:0.3:10;
%! y1 = cos (x);
%! y2 = sin (x);
%! subplot (3,1,1);
%!  args = {'color', 'b', 'marker', 's'};
%!  line ([x(:), x(:)], [y1(:), y2(:)], args{:});
%!  title ('Test broadcasting for line()');
%! subplot (3,1,2);
%!  line (x(:), [y1(:), y2(:)], args{:});
%! subplot (3,1,3);
%!  line ([x(:), x(:)+pi/2], y1(:), args{:});
%!  xlim ([0 10]);

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = line;
%!   assert (findobj (hf, "type", "line"), h);
%!   assert (get (h, "xdata"), [0 1], eps);
%!   assert (get (h, "ydata"), [0 1], eps);
%!   assert (get (h, "type"), "line");
%!   assert (get (h, "color"), get (0, "defaultlinecolor"));
%!   assert (get (h, "linestyle"), get (0, "defaultlinelinestyle"));
%!   assert (get (h, "linewidth"), get (0, "defaultlinelinewidth"), eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

