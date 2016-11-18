## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn  {Function File} {@var{rgb_map} =} ntsc2rgb (@var{yiq_map})
## @deftypefnx {Function File} {@var{rgb_img} =} ntsc2rgb (@var{yiq_img})
## Transform a colormap or image from luminance-chrominance (NTSC) space to
## red-green-blue (RGB) color space.
##
## Implementation Note:
## The conversion matrix is chosen to be the inverse of the matrix used for
## rgb2ntsc such that
##
## @example
## x == ntsc2rgb (rgb2ntsc (x))
## @end example
##
## @sc{matlab} uses a slightly different matrix where rounding means the
## equality above does not hold.
## @seealso{rgb2ntsc, hsv2rgb, ind2rgb}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function rgb = ntsc2rgb (yiq)

  if (nargin != 1)
    print_usage ();
  endif

  if (! isa (yiq, "double"))
    error ("ntsc2rgb: YIQ must be of type double");
  endif

  ## If we have an image convert it into a color map.
  if (isnumeric (yiq) && ndims (yiq) == 3)
    is_image = true;
    sz = size (yiq);
    yiq = [yiq(:,:,1)(:), yiq(:,:,2)(:), yiq(:,:,3)(:)];
  else
    is_image = false;
  endif

  if (! isreal (yiq) || columns (yiq) != 3 || issparse (yiq))
    error ("ntsc2rgb: input must be a matrix of size Nx3 or NxMx3");
  endif

  ## Conversion matrix constructed from 'inv (rgb2ntsc matrix)'.
  ## See programming notes in rgb2ntsc.m.  Note: Matlab matrix for inverse
  ## is slightly different.  We prefer this matrix so that
  ## x == ntsc2rgb (rgb2ntsc (x)) rather than maintaining strict compatibility
  ## with Matlab.
  trans = [ 1.0,      1.0,      1.0;
            0.95617, -0.27269, -1.10374;
            0.62143, -0.64681,  1.70062 ];

  rgb = yiq * trans;

  ## If input was an image, convert it back into one.
  if (is_image)
    rgb = reshape (rgb, sz);
  endif

endfunction


## Test pure R, G, B colors
%!assert (ntsc2rgb ([.299  .596  .211]), [1 0 0], 1e-5)
%!assert (ntsc2rgb ([.587 -.274 -.523]), [0 1 0], 1e-5)
%!assert (ntsc2rgb ([.114 -.322  .312]), [0 0 1], 1e-5)

%!test
%! rgb_map = rand (64, 3);
%! assert (ntsc2rgb (rgb2ntsc (rgb_map)), rgb_map, 1e-3);

%!test
%! rgb_img = rand (64, 64, 3);
%! assert (ntsc2rgb (rgb2ntsc (rgb_img)), rgb_img, 1e-3);

## Test input validation
%!error ntsc2rgb ()
%!error ntsc2rgb (1,2)
%!error <YIQ must be of type double> ntsc2rgb (uint8 (1))
%!error <must be a matrix of size Nx3 or NxMx3> ntsc2rgb (ones (2,2))

