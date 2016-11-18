## Copyright (C) 2014-2015 Carnë Draug
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
## @deftypefn  {Function File} {} im2frame (@var{rgb})
## @deftypefnx {Function File} {} im2frame (@var{x}, @var{map})
## Convert image to movie frame.
##
## A movie frame is simply a struct with the fields @qcode{"cdata"} and
## @qcode{"colormap"}.
##
## Support for N-dimensional images is given when each image projection,
## matrix sizes of @nospell{MxN and MxNx3} for RGB images, is concatenated
## along the fourth dimension.  In such cases, the returned value is a struct
## array.
##
## @seealso{frame2im}
## @end deftypefn

## Author: Carnë Draug <carandraug@octave.org>

function [frame] = im2frame (x, map = [])

  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (ndims (x) > 4)
    error ("im2frame: X and RGB must be a single image");
  endif

  ## Matlab documentation is incorrect.  Singleton 3rd dimension will error
  ## without cmap (no use of default cmap), and cmap is added to the frame
  ## even when image is RGB.

  nchannels = size (x, 3);
  if (nchannels == 3)
    ## RGB image, do nothing
  elseif (nchannels == 1)
    if (nargin < 2)
      error ("im2frame: MAP required for indexed images");
    endif
    [x, map] = ind2x ("im2frame", x, map);
  else
    error ("im2frame: first argument must be indexed or RGB image");
  endif

  ## support N dimensional images and return a struct array
  if (ndims (x) == 4)
    x = reshape (num2cell (x, [1 2 3]), 1, size (x, 4));
  endif

  frame = struct ("cdata", x, "colormap", map);
endfunction


%!function f = make_rgb_f ()
%! f = randi ([0 255], 10, 20, 3);
%!endfunction

%!function f = make_ind_f ()
%! f = randi ([1 100], 10, 20, 3);
%!endfunction

%!test
%! rgb = make_rgb_f ();
%! assert (im2frame (rgb), struct ("cdata", rgb, "colormap", []));

%!test
%! ind = make_ind_f ();
%! cmap = bone (100);
%! assert (im2frame (ind, cmap), struct ("cdata", ind, "colormap", cmap));

%!test
%! rgb1 = make_rgb_f ();
%! rgb2 = make_rgb_f ();
%! rgb3 = make_rgb_f ();
%! rgb4 = make_rgb_f ();
%! assert (im2frame (cat (4, rgb1, rgb2, rgb3, rgb4)),
%!         struct ("cdata", {rgb1, rgb2, rgb3, rgb4}, "colormap", []));

%!test
%! ind1 = make_ind_f ();
%! ind2 = make_ind_f ();
%! ind3 = make_ind_f ();
%! ind4 = make_ind_f ();
%! cmap = bone (100);
%! assert (im2frame (cat (4, ind1, ind2, ind3, ind4), cmap),
%!         struct ("cdata", {ind1, ind2, ind3, ind4}, "colormap", cmap));

