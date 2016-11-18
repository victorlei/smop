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
## @deftypefn  {Function File} {@var{rgb} =} ind2rgb (@var{x}, @var{map})
## @deftypefnx {Function File} {[@var{R}, @var{G}, @var{B}] =} ind2rgb (@var{x}, @var{map})
## Convert an indexed image to red, green, and blue color components.
##
## The image @var{x} must be an indexed image which will be converted using the
## colormap @var{map}.  If @var{map} does not contain enough colors for the
## image, pixels in @var{x} outside the range are mapped to the last color in
## the map.
##
## The output may be a single RGB image (@nospell{MxNx3} matrix where M and N
## are the original image @var{x} dimensions, one for each of the red, green
## and blue channels).  Alternatively, the individual red, green, and blue
## color matrices of size @nospell{MxN} may be returned.
##
## Multi-dimensional indexed images (of size @nospell{MxNx1xK}) are also
## supported.
##
## @seealso{rgb2ind, ind2gray, hsv2rgb, ntsc2rgb}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [R, G, B] = ind2rgb (x, map)

  if (nargin != 2)
    print_usage ();
  endif
  [x, map] = ind2x ("ind2rgb", x, map);

  ## Compute result
  sz = size (x);
  R = reshape (map(x(:), 1), sz);
  G = reshape (map(x(:), 2), sz);
  B = reshape (map(x(:), 3), sz);

  ## Use ND array if only one output is requested.
  if (nargout <= 1)
    if (ndims (x) == 2)
      R = reshape ([R(:); G(:); B(:)], [sz, 3]);
    elseif (ndims (x) == 4)
      R = permute (reshape ([R(:); G(:); B(:)], [sz(1) sz(2) sz(4) 3]), ...
                   [1 2 4 3]);
    else
      ## we should never reach here since ind2x() should filter them out
      error ("ind2rgb: an indexed image must have 2 or 4 dimensions.");
    endif
  endif

endfunction


%!shared img, map, ergb, rgb, r, g, b
%! img = [2 4 5; 3 2 5; 1 2 4];
%! map = [0.0  0.0  0.0
%!        0.2  0.4  0.6
%!        0.4  0.4  0.5
%!        0.3  0.7  1.0
%!        0.1  0.5  0.8];
%! ergb(:,:,1) = [0.2 0.3 0.1; 0.4 0.2 0.1; 0.0 0.2 0.3];
%! ergb(:,:,2) = [0.4 0.7 0.5; 0.4 0.4 0.5; 0.0 0.4 0.7];
%! ergb(:,:,3) = [0.6 1.0 0.8; 0.5 0.6 0.8; 0.0 0.6 1.0];
%! ## test basic usage with 1 and 3 outputs
%! [rgb] = ind2rgb (img, map);
%! [r, g, b] = ind2rgb (img, map);
%!
%!assert (ergb, rgb)
%!assert (ergb, reshape ([r(:) g(:) b(:)], [size(img) 3]))
%!test
%! ## test correction for integers
%! img = uint8 (img - 1);
%! [rgb] = ind2rgb (img, map);
%! assert (ergb, rgb);
%!test
%! ## Check that values below lower bound are mapped to first color value
%! warning ("off", "Octave:ind2rgb:invalid-idx-img", "local");
%! rgb = ind2rgb ([-1 0 2], gray (64));
%! assert (rgb(:,1:2,:), zeros (1,2,3));
%! assert (rgb(:,3,:), 1/63 * ones (1,1,3));

## Test input validation
%!error ind2rgb ()
%!error ind2rgb (1,2,3)
%!error <X must be an indexed image> ind2rgb (ones (3,3,3), jet (64))
%!error <X must be an indexed image> ind2rgb (1+i, jet (64))
%!error <X must be an indexed image> ind2rgb (sparse (1), jet (64))
%!error <X must be an indexed image> ind2rgb (1.1, jet (64))
%!error <X must be an indexed image> ind2rgb ({1}, jet (64))
%!error <MAP must be a valid colormap> ind2rgb (1, {1})
%!error <MAP must be a valid colormap> ind2rgb (1, 1+i)
%!error <MAP must be a valid colormap> ind2rgb (1, ones (2,2,2))
%!error <MAP must be a valid colormap> ind2rgb (1, ones (2,4))
%!error <MAP must be a valid colormap> ind2rgb (1, [-1])
%!error <MAP must be a valid colormap> ind2rgb (1, [2])

%!warning <contains colors outside of colormap> ind2rgb ([-1 1], jet (64));
%!warning <contains colors outside of colormap> ind2rgb ([0 1 2], gray (5));
%!warning <contains colors outside of colormap> ind2rgb ([1 2 6], gray (5));
%!warning <contains colors outside of colormap> ind2rgb (uint8 ([1 2 5]), gray (5));

