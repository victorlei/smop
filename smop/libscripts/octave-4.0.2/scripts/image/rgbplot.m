## Copyright (C) 2012-2015 Rik Wehbring
## Copyright (C) 2012-2015 Carnë Draug
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
## @deftypefn  {Function File} {} rgbplot (@var{cmap})
## @deftypefnx {Function File} {} rgbplot (@var{cmap}, @var{style})
## @deftypefnx {Function File} {@var{h} =} rgbplot (@dots{})
## Plot the components of a colormap.
##
## Two different @var{style}s are available for displaying the @var{cmap}:
##
## @table @asis
## @item profile (default)
## Plot the RGB line profile of the colormap for each of the channels (red,
## green and blue) with the plot lines colored appropriately.  Each line
## represents the intensity of each RGB components across the colormap.
##
## @item composite
## Draw the colormap across the X-axis so that the actual index colors are
## visible rather than the individual color components.
##
## @end table
##
## The optional return value @var{h} is a graphics handle to the created plot.
##
## Run @code{demo rgbplot} to see an example of @code{rgbplot} and each style
## option.
## @seealso{colormap}
## @end deftypefn

function h = rgbplot (cmap, style = "profile")

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! iscolormap (cmap))
    error ("rgbplot: CMAP must be a valid colormap");
  elseif (! ischar (style))
    error ("rgbplot: STYLE must be a string");
  endif

  switch (tolower (style))
    case "profile"
      htmp = plot (cmap(:,1),"r", cmap(:,2),"g", cmap(:,3),"b");
      set (gca, 'ytick', 0:0.1:1);
    case "composite"
      htmp = image (1:rows(cmap));
      set (gca, 'ytick', []);
      colormap (cmap);
    otherwise
      error ("rgbplot: unknown style '%s'", style);
  endswitch
  xlabel ("color index");

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! subplot (1, 2, 1);
%! rgbplot (ocean, "profile");
%! subplot (1, 2, 2);
%! rgbplot (ocean, "composite");

## Test input validation
%!error rgbplot ()
%!error rgbplot (1,2)
%!error <CMAP must be a valid colormap> rgbplot ({0 1 0})
%!error <STYLE must be a string> rgbplot ([0 1 0], 2)
%!error <unknown style 'nostyle'> rgbplot ([0 1 0], "nostyle")

