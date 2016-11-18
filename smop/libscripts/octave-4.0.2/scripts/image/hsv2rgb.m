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
## @deftypefn  {Function File} {@var{rgb_map} =} hsv2rgb (@var{hsv_map})
## @deftypefnx {Function File} {@var{rgb_img} =} hsv2rgb (@var{hsv_img})
## Transform a colormap or image from hue-saturation-value (HSV) space to
## red-green-blue (RGB) space.
##
## A color in HSV space is represented by hue, saturation and value
## (brightness) levels.  Value gives the amount of light in the color.  Hue
## describes the dominant wavelength.  Saturation is the amount of hue mixed
## into the color.
##
## A color in the RGB space consists of red, green, and blue intensities.
## @seealso{rgb2hsv, ind2rgb, ntsc2rgb}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function rgb_map = hsv2rgb (hsv_map)

  ## Each color value x = (r,g,b) is calculated with
  ## x = (1-sat)*val+sat*val*f_x(hue)
  ## where f_x(hue) is a piecewise defined function for
  ## each color with f_r(hue-2/3) = f_g(hue) = f_b(hue-1/3).

  if (nargin != 1)
    print_usage ();
  endif

  cls = class (hsv_map);
  if (! any (strcmp (cls, {"uint8", "uint16", "single", "double"})))
    error ("hsv2rgb: invalid data type '%s'", cls);
  elseif (isfloat (hsv_map) && (any (hsv_map(:) < 0) || any (hsv_map(:) > 1)))
    error ("hsv2rgb: floating point images may only contain values between 0 and 1");
  endif

  ## If we have an image convert it into a color map.
  if (isreal (hsv_map) && ndims (hsv_map) == 3)
    is_image = true;
    sz = size (hsv_map);
    hsv_map = [hsv_map(:,:,1)(:), hsv_map(:,:,2)(:), hsv_map(:,:,3)(:)];
    ## Convert to a double image.
    if (isinteger (hsv_map))
      low = double (intmin (cls));
      high = double (intmax (cls));
      hsv_map = (double (hsv_map) - low) / (high - low);
    endif
  else
    is_image = false;
  endif

  if (! isreal (hsv_map) || columns (hsv_map) != 3 || issparse (hsv_map))
    error ("hsv2rgb: input must be a matrix of size Nx3 or MxNx3");
  endif

  ## FIXME: Currently input is validated and an error results if values
  ##        are outside range [0, 1].  We could also simply allow those values
  ##        and re-instate this code to produce saturating semantics.
  ## Trim map to range [0, 1]
  ## hsv_map(hsv_map < 0) = 0;
  ## hsv_map(hsv_map > 1) = 1;

  h = hsv_map(:,1);
  s = hsv_map(:,2);
  v = hsv_map(:,3);
  ## Prefill rgb map with v*(1-s)
  rgb_map = repmat (v .* (1 - s), 1, 3);

  ## red = hue-2/3 : green = hue : blue = hue-1/3
  ## Apply modulo 1 for red and blue to keep within range [0, 1]
  hue = [mod(h - 2/3, 1), h , mod(h - 1/3, 1)];

  ## factor s*v -> f
  f = repmat (s .* v, 1, 3);

  ## add s*v*hue-function to rgb map
  rgb_map += f .* (6 * (hue < 1/6) .* hue
             + (hue >= 1/6 & hue < 1/2)
             + (hue >= 1/2 & hue < 2/3) .* (4 - 6 * hue));

  ## FIXME: hsv2rgb does not preserve class of image.
  ##        Should it also convert back to uint8, uint16 for integer images?
  ## If input was an image, convert it back into one.
  if (is_image)
    rgb_map = reshape (rgb_map, sz);
  endif

endfunction


## Test pure colors
%!assert (hsv2rgb ([0 1 1]), [1 0 0])
%!assert (hsv2rgb ([1 1 1]), [1 0 0])
%!assert (hsv2rgb ([1/3 1 1]), [0 1 0])
%!assert (hsv2rgb ([2/3 1 1]), [0 0 1])

%!test
%! hsv_map = rand (64, 3);
%! assert (rgb2hsv (hsv2rgb (hsv_map)), hsv_map, 1e-6);

%!test
%! hsv_img = rand (64, 64, 3);
%! assert (rgb2hsv (hsv2rgb (hsv_img)), hsv_img, 1e-6);

## Test input validation
%!error hsv2rgb ()
%!error hsv2rgb (1,2)
%!error <invalid data type> hsv2rgb ({1})
%!error <must be a matrix of size Nx3> hsv2rgb (ones (2,2))
%!error <must be a matrix of size Nx3> hsv2rgb (sparse (ones(1,3)))

