## Copyright (C) 2004 Josep Mones i Teixidor
## Copyright (C) 2012-2015 Rik Wehbring
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
## @deftypefn  {Function File} {[@var{Y}, @var{newmap}] =} cmunique (@var{X}, @var{map})
## @deftypefnx {Function File} {[@var{Y}, @var{newmap}] =} cmunique (@var{RGB})
## @deftypefnx {Function File} {[@var{Y}, @var{newmap}] =} cmunique (@var{I})
## Convert an input image @var{X} to an ouput indexed image @var{Y} which uses
## the smallest colormap possible @var{newmap}.
##
## When the input is an indexed image (@var{X} with colormap @var{map}) the
## output is a colormap @var{newmap} from which any repeated rows have been
## eliminated.  The output image, @var{Y}, is the original input image with
## the indices adjusted to match the new, possibly smaller, colormap.
##
## When the input is an RGB image (an @nospell{MxNx3} array), the output
## colormap will contain one entry for every unique color in the original image.
## In the worst case the new map could have as many rows as the number of
## pixels in the original image.
##
## When the input is a grayscale image @var{I}, the output colormap will
## contain one entry for every unique intensity value in the original image.
## In the worst case the new map could have as many rows as the number of
## pixels in the original image.
##
## Implementation Details:
##
## @var{newmap} is always an Mx3 matrix, even if the input image is
## an intensity grayscale image @var{I} (all three RGB planes are
## assigned the same value).
##
## The output image is of class uint8 if the size of the new colormap is
## less than or equal to 256.  Otherwise, the output image is of class double.
##
## @seealso{rgb2ind, gray2ind}
## @end deftypefn


## Author:  Josep Mones i Teixidor <jmones@puntbarra.com>

function [Y, newmap] = cmunique (X, map)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  cls = class (X);
  if (! any (strcmp (cls, {"uint8", "uint16", "single", "double"})))
    error ("cmunique: X is of invalid data type '%s'", cls);
  endif

  if (nargin == 2)
    ## (X, map) case
    if (! iscolormap (map))
      error ("cmunique: MAP must be a valid colormap");
    endif
    [newmap,i,j] = unique (map, "rows");  # calculate unique colormap
    if (isfloat (X))
      Y = j(X);               # find new indices
    else
      Y = j(double (X) + 1);  # find new indices, switch to 1-based index
    endif
  else
    switch (size (X,3))
      case 1
        ## I case
        [newmap,i,j] = unique (X);               # calculate unique colormap
        newmap = repmat (newmap,1,3);            # get a RGB colormap
        Y = reshape (j, rows (X), columns (X));  # Y is j reshaped
      case 3
        ## RGB case
        ## build a map with all values
        map = [X(:,:,1)(:), X(:,:,2)(:), X(:,:,3)(:)];
        [newmap,i,j] = unique (map, "rows");     # calculate unique colormap
        Y = reshape (j, rows (X), columns (X));  # Y is j reshaped
      otherwise
        error ("cmunique: X is not a valid image");
    endswitch

    ## if image was uint8 or uint16 we have to convert newmap to [0,1] range
    if (isinteger (X))
      newmap = double (newmap) / double (intmax (cls));
    endif
  endif

  if (rows (newmap) <= 256)
    ## convert Y to uint8 and 0-based indexing
    Y = uint8 (Y-1);
  endif

endfunction


%!demo
%! [Y, newmap] = cmunique ([1:4;5:8], [hot(4);hot(4)])
%! ## Both rows are equal since map maps colors to the same value
%! ## cmunique will give the same indices to both

## Check that output is uint8 in short colormaps
%!test
%! [Y, newmap] = cmunique ([1:4;5:8], [hot(4);hot(4)]);
%! assert (Y, uint8 ([0:3;0:3]));
%! assert (newmap, hot (4));

## Check that output is double in bigger
%!test
%! [Y, newmap] = cmunique ([1:300;301:600], [hot(300);hot(300)]);
%! assert (Y, [1:300;1:300]);
%! assert (newmap, hot (300));

## Check boundary case 256
%!test
%! [Y, newmap] = cmunique ([1:256;257:512], [hot(256);hot(256)]);
%! assert (Y, uint8 ([0:255;0:255]));
%! assert (newmap, hot (256));

## Check boundary case 257
%!test
%! [Y, newmap] = cmunique ([1:257;258:514], [hot(257);hot(257)]);
%! assert (Y, [1:257;1:257]);
%! assert (newmap, hot (257));

## Random RGB image
%!test
%! RGB = rand (10,10,3);
%! [Y, newmap] = cmunique (RGB);
%! assert (RGB(:,:,1), newmap(:,1)(Y+1));
%! assert (RGB(:,:,2), newmap(:,2)(Y+1));
%! assert (RGB(:,:,3), newmap(:,3)(Y+1));

## Random uint8 RGB image
%!test
%! RGB = uint8 (rand (10,10,3)*255);
%! RGBd = double (RGB) / 255;
%! [Y, newmap] = cmunique (RGB);
%! assert (RGBd(:,:,1), newmap(:,1)(Y+1));
%! assert (RGBd(:,:,2), newmap(:,2)(Y+1));
%! assert (RGBd(:,:,3), newmap(:,3)(Y+1));

## Random uint16 RGB image
%!test
%! RGB = uint16 (rand (10,10,3)*65535);
%! RGBd = double (RGB) / 65535;
%! [Y, newmap] = cmunique (RGB);
%! assert (RGBd(:,:,1), newmap(:,1)(Y+1));
%! assert (RGBd(:,:,2), newmap(:,2)(Y+1));
%! assert (RGBd(:,:,3), newmap(:,3)(Y+1));

## Random I image
%!test
%! I = rand (10,10);
%! [Y, newmap] = cmunique (I);
%! assert (I, newmap(:,1)(Y+1));
%! assert (I, newmap(:,2)(Y+1));
%! assert (I, newmap(:,3)(Y+1));

## Random uint8 I image
%!test
%! I = uint8 (rand (10,10)*256);
%! Id = double (I) / 255;
%! [Y, newmap] = cmunique (I);
%! assert (Id, newmap(:,1)(Y+1));
%! assert (Id, newmap(:,2)(Y+1));
%! assert (Id, newmap(:,3)(Y+1));

## Random uint16 I image
%!test
%! I = uint16 (rand (10,10)*65535);
%! Id = double (I) / 65535;
%! [Y, newmap] = cmunique (I);
%! assert (Id, newmap(:,1)(Y+1));
%! assert (Id, newmap(:,2)(Y+1));
%! assert (Id, newmap(:,3)(Y+1));

## Test input validation
%!error cmpermute ()
%!error cmpermute (1,2,3)
%!error <X is of invalid data type> cmunique (uint32 (magic (16)))
%!error <MAP must be a valid colormap> cmunique (1, "a")
%!error <MAP must be a valid colormap> cmunique (1, i)
%!error <MAP must be a valid colormap> cmunique (1, ones (3,3,3))
%!error <MAP must be a valid colormap> cmunique (1, ones (3,2))
%!error <MAP must be a valid colormap> cmunique (1, [-1 1 1])
%!error <MAP must be a valid colormap> cmunique (1, [2 1 1])

