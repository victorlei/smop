## Copyright (C) 1994-2015 John W. Eaton
## Copyright (C) 2012 Carnë Draug
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
## @deftypefn  {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{rgb})
## @deftypefnx {Function File} {[@var{x}, @var{map}] =} rgb2ind (@var{R}, @var{G}, @var{B})
## Convert an image in red-green-blue (RGB) color space to an indexed image.
##
## The input image @var{rgb} can be specified as a single matrix of size
## @nospell{MxNx3}, or as three separate variables, @var{R}, @var{G}, and
## @var{B}, its three color channels, red, green, and blue.
##
## It outputs an indexed image @var{x} and a colormap @var{map} to interpret
## an image exactly the same as the input.  No dithering or other form of color
## quantization is performed.  The output class of the indexed image @var{x}
## can be uint8, uint16 or double, whichever is required to specify the
## number of unique colors in the image (which will be equal to the number
## of rows in @var{map}) in order
##
## Multi-dimensional indexed images (of size @nospell{MxNx3xK}) are also
## supported, both via a single input (@var{rgb}) or its three color channels
## as separate variables.
##
## @seealso{ind2rgb, rgb2hsv, rgb2ntsc}
## @end deftypefn

## FIXME: This function has a very different syntax than the Matlab
##        one of the same name.
##        Octave function does not support N, MAP, DITHER, or TOL arguments.

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [x, map] = rgb2ind (R, G, B)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    rgb = R;
    if (ndims (rgb) > 4 || size (rgb, 3) != 3)
      error ("rgb2ind: argument is not an RGB image");
    else
      R = rgb(:,:,1,:);
      G = rgb(:,:,2,:);
      B = rgb(:,:,3,:);
    endif
  elseif (! size_equal (R, G, B))
    error ("rgb2ind: R, G, and B must have the same size");
  endif

  x = reshape (1:numel (R), size (R));

  map    = unique ([R(:) G(:) B(:)], "rows");
  [~, x] = ismember ([R(:) G(:) B(:)], map, "rows");
  x      = reshape (x, size (R));

  ## a colormap is of class double and values between 0 and 1
  switch (class (R))
    case {"single", "double", "logical"}
      ## do nothing, return the same
    case {"uint8", "uint16"}
      map = double (map) / double (intmax (class (R)));
    case "int16"
      map = (double (im) + 32768) / 65535;
    otherwise
      error ("unsupported image class %s", im_class);
  endswitch

  ## we convert to the smallest class necessary to encode the image. Matlab
  ## documentation does not mention what it does when uint16 is not enough...
  ## When an indexed image is of integer class, there's a -1 offset to the
  ## colormap, hence the adjustment
  if (rows (map) < 256)
    x = uint8 (x - 1);
  elseif (rows (map) < 65536)
    x = uint16 (x - 1);
  else
    ## leave it as double
  endif

endfunction

## Test input validation
%!error rgb2ind ()
%!error rgb2ind (1,2,3,4,5,6,7)
%!error <RGB> rgb2ind (rand (10, 10, 4))

## FIXME: the following tests simply make sure that rgb2ind and ind2rgb
##        reverse each other. We should have better tests for this.

## Typical usage
%!test
%! rgb = rand (10, 10, 3);
%! [ind, map] = rgb2ind (rgb);
%! assert (ind2rgb (ind, map), rgb);
%!
%! ## test specifying the RGB channels separated
%! [ind, map] = rgb2ind (rgb(:,:,1), rgb(:,:,2), rgb(:,:,3));
%! assert (ind2rgb (ind, map), rgb);

## Test N-dimensional images
%!test
%! rgb = rand (10, 10, 3, 10);
%! [ind, map] = rgb2ind (rgb);
%! assert (ind2rgb (ind, map), rgb);
%! [ind, map] = rgb2ind (rgb(:,:,1,:), rgb(:,:,2,:), rgb(:,:,3,:));
%! assert (ind2rgb (ind, map), rgb);

## Test output class
%!test
%! ## this should have more than 65536 unique colors
%! rgb = rand (1000, 1000, 3);
%! [ind, map] = rgb2ind (rgb);
%! assert (class (ind), "double");
%! assert (class (map), "double");
%!
%! ## and this should have between 255 and 65536 unique colors
%! rgb = rand (20, 20, 3);
%! [ind, map] = rgb2ind (rgb);
%! assert (class (ind), "uint16");
%! assert (class (map), "double");
%!
%! ## and this certainly less than 256 unique colors
%! rgb = rand (10, 10, 3);
%! [ind, map] = rgb2ind (rgb);
%! assert (class (ind), "uint8");
%! assert (class (map), "double");

