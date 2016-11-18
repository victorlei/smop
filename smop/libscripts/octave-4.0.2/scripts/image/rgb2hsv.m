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
## @deftypefn  {Function File} {@var{hsv_map} =} rgb2hsv (@var{rgb})
## @deftypefnx {Function File} {@var{hsv_map} =} rgb2hsv (@var{rgb})
## Transform a colormap or image from red-green-blue (RGB) space to
## hue-saturation-value (HSV) space.
##
## A color in the RGB space consists of red, green, and blue intensities.
##
## A color in HSV space is represented by hue, saturation, and value
## (brightness) levels.  Value gives the amount of light in the color.  Hue
## describes the dominant wavelength.  Saturation is the amount of hue mixed
## into the color.
## @seealso{hsv2rgb, rgb2ind, rgb2ntsc}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function hsv_map = rgb2hsv (rgb)

  if (nargin != 1)
    print_usage ();
  endif

  cls = class (rgb);
  if (! any (strcmp (cls, {"uint8", "uint16", "single", "double"})))
    error ("rgb2hsv: invalid data type '%s'", cls);
  elseif (isfloat (rgb) && (any (rgb(:) < 0) || any (rgb(:) > 1)))
    error ("rgb2hsv: floating point images may only contain values between 0 and 1");
  endif

  ## If we have an image convert it into a color map.
  if (isreal (rgb) && ndims (rgb) == 3)
    is_image = true;
    sz = size (rgb);
    rgb = [rgb(:,:,1)(:), rgb(:,:,2)(:), rgb(:,:,3)(:)];
    ## Convert to a double image.
    if (isinteger (rgb))
      low = double (intmin (cls));
      high = double (intmax (cls));
      rgb = (double (rgb) - low) / (high - low);
    endif
  else
    is_image = false;
  endif

  if (! ismatrix (rgb) || columns (rgb) != 3 || issparse (rgb))
    error ("rgb2hsv: input must be a matrix of size Nx3 or MxNx3");
  endif

  ## get the max and min for each row
  s = min (rgb, [], 2);
  v = max (rgb, [], 2);

  ## set hue to zero for undefined values (gray has no hue)
  h = zeros (rows (rgb), 1);
  notgray = (s != v);

  ## blue hue
  idx = (v == rgb(:,3) & notgray);
  if (any (idx))
    h(idx) = 2/3 + 1/6 * (rgb(idx,1) - rgb(idx,2)) ./ (v(idx) - s(idx));
  endif

  ## green hue
  idx = (v == rgb(:,2) & notgray);
  if (any (idx))
    h(idx) = 1/3 + 1/6 * (rgb(idx,3) - rgb(idx,1)) ./ (v(idx) - s(idx));
  endif

  ## red hue
  idx = (v == rgb(:,1) & notgray);
  if (any (idx))
    h(idx) =       1/6 * (rgb(idx,2) - rgb(idx,3)) ./ (v(idx) - s(idx));
  endif
  h(h < 0) += 1;   # correct for negative red

  ## set the saturation
  s(! notgray) = 0;
  s(notgray) = 1 - s(notgray) ./ v(notgray);

  hsv_map = [h, s, v];

  ## FIXME: rgb2hsv does not preserve class of image.
  ##        Should it also convert back to uint8, uint16 for integer images?
  ## If input was an image, convert it back into one.
  if (is_image)
    hsv_map = reshape (hsv_map, sz);
  endif

endfunction


## Test pure colors and gray
%!assert (rgb2hsv ([1 0 0]), [0 1 1])
%!assert (rgb2hsv ([0 1 0]), [1/3 1 1])
%!assert (rgb2hsv ([0 0 1]), [2/3 1 1])
%!assert (rgb2hsv ([0.5 0.5 0.5]), [0 0 0.5])

%!test
%! rgb_map = rand (64, 3);
%! assert (hsv2rgb (rgb2hsv (rgb_map)), rgb_map, 1e-6);

%!test
%! rgb_img = rand (64, 64, 3);
%! assert (hsv2rgb (rgb2hsv (rgb_img)), rgb_img, 1e-6);

## Test input validation
%!error rgb2hsv ()
%!error rgb2hsv (1,2)
%!error <invalid data type 'cell'> rgb2hsv ({1})
%!error <must be a matrix of size Nx3> rgb2hsv (ones (2,2))

