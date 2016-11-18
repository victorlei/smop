## Copyright (C) 1999-2015 Kai Habel
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
## @deftypefn  {Function File} {@var{map_out} =} brighten (@var{beta})
## @deftypefnx {Function File} {@var{map_out} =} brighten (@var{map}, @var{beta})
## @deftypefnx {Function File} {@var{map_out} =} brighten (@var{h}, @var{beta})
## @deftypefnx {Function File} {} brighten (@dots{})
## Brighten or darken a colormap.
##
## The argument @var{beta} must be a scalar between -1 and 1, where a negative
## value darkens and a positive value brightens the colormap.
##
## If the @var{map} argument is omitted, the function is applied to the current
## colormap.
##
## The first argument can also be a valid graphics handle @var{h}, in which
## case @code{brighten} is applied to the colormap associated with this handle.
##
## If no output is specified then the result is written to the current colormap.
## @seealso{colormap, contrast}
## @end deftypefn

function rmap = brighten (arg1, beta)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  h = -1;
  if (nargin == 1)
    beta = arg1;
    m = colormap ();
    h = gcf ();
  else
    if (ishandle (arg1))
      h = arg1;
      m = get (h, "colormap");
    elseif (iscolormap (arg1))
      m = arg1;
    else
      error ("brighten: first argument must be a colormap or a graphics handle");
    endif
  endif

  if (! isscalar (beta) || beta <= -1 || beta >= 1)
    error ("brighten: BETA must be a scalar in the range (-1,1)");
  endif

  if (beta > 0)
    gamma = 1 - beta;
  else
    gamma = 1 / (1 + beta);
  endif

  if (nargout == 0)
    if (ishandle (h))
      set (h, "colormap", m .^ gamma);
    else
      colormap (m .^ gamma);
    endif
  else
    rmap = m .^ gamma;
  endif

endfunction


%!demo
%! ## First figure uses default grayscale colormap
%! figure;
%! colormap (gray (64));
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! title ("default grayscale colormap");
%! pos = get (gcf, "position");
%! pos(1) += pos(3) + 25;
%! ## Second figure uses brightened grayscale colormap
%! figure ("position", pos);
%! colormap (gray (64));
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! brighten (0.5);
%! title ("grayscale colormap brightened by 0.5");

