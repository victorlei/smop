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
## @deftypefn  {Function File} {} imshow (@var{im})
## @deftypefnx {Function File} {} imshow (@var{im}, @var{limits})
## @deftypefnx {Function File} {} imshow (@var{im}, @var{map})
## @deftypefnx {Function File} {} imshow (@var{rgb}, @dots{})
## @deftypefnx {Function File} {} imshow (@var{filename})
## @deftypefnx {Function File} {} imshow (@dots{}, @var{string_param1}, @var{value1}, @dots{})
## @deftypefnx {Function File} {@var{h} =} imshow (@dots{})
## Display the image @var{im}, where @var{im} can be a 2-dimensional
## (grayscale image) or a 3-dimensional (RGB image) matrix.
##
## If @var{limits} is a 2-element vector @code{[@var{low}, @var{high}]}, the
## image is shown using a display range between @var{low} and @var{high}.  If
## an empty matrix is passed for @var{limits}, the display range is computed
## as the range between the minimal and the maximal value in the image.
##
## If @var{map} is a valid color map, the image will be shown as an indexed
## image using the supplied color map.
##
## If a file name is given instead of an image, the file will be read and shown.
##
## If given, the parameter @var{string_param1} has value @var{value1}.
## @var{string_param1} can be any of the following:
##
## @table @asis
## @item @qcode{"displayrange"}
## @var{value1} is the display range as described above.
##
## @item @qcode{"colormap"}
## @var{value1} is the colormap to use when displaying an indexed image.
##
## @item @qcode{"xdata"}
## If @var{value1} is a two element vector, it must contain horizontal axis
## limits in the form [xmin xmax]; Otherwise @var{value1} must be a vector and
## only the first and last elements will be used for xmin and xmax respectively.
##
## @item @qcode{"ydata"}
## If @var{value1} is a two element vector, it must contain vertical axis
## limits in the form [ymin ymax]; Otherwise @var{value1} must be a vector and
## only the first and last elements will be used for ymin and ymax respectively.
##
## @end table
##
## The optional return value @var{h} is a graphics handle to the image.
## @seealso{image, imagesc, colormap, gray2ind, rgb2ind}
## @end deftypefn

## Author: Stefan van der Walt  <stefan@sun.ac.za>
## Author: Soren Hauberg <hauberg at gmail dot com>
## Adapted-By: jwe

function h = imshow (im, varargin)

  if (nargin == 0)
    print_usage ();
  endif

  display_range = NA;
  truecolor = false;
  indexed = false;
  xdata = ydata = [];

  ## Get the image.
  if (ischar (im))
    [im, map] = imread (im);
    indexed = true;
    colormap (map);
  endif

  nd = ndims (im);

  if (! ((isnumeric (im) || islogical (im)) && (nd == 2 || nd == 3)))
    error ("imshow: IM must be an image or the filename of an image");
  endif

  if (nd == 2)
    if (! indexed)
      colormap (gray ());
    endif
  elseif (size (im, 3) == 3)
    if (ismember (class (im), {"uint8", "uint16", "double", "single"}))
      truecolor = true;
    else
      error ("imshow: TrueColor image must be uint8, uint16, double, or single");
    endif
  else
    error ("imshow: expecting MxN or MxNx3 matrix for image");
  endif

  narg = 1;
  while (narg <= numel (varargin))
    arg = varargin{narg++};
    if (isnumeric (arg))
      if (numel (arg) == 2 || isempty (arg))
        display_range = arg;
      elseif (columns (arg) == 3)
        indexed = true;
        if (iscolormap (arg))
          colormap (arg);
        else
          error ("imshow: invalid colormap MAP");
        endif
      elseif (! isempty (arg))
        error ("imshow: argument number %d is invalid", narg);
      endif
    elseif (ischar (arg))
      switch (tolower (arg))
        case "colormap"
          map = varargin{narg++};
          if (iscolormap (map))
            colormap (map);
          else
            error ("imshow: invalid colormap");
          endif
        case "displayrange"
          display_range = varargin{narg++};
        case "parent"
          warning ("imshow: parent argument is not implemented");
        case {"truesize", "initialmagnification"}
          warning ("image: zoom argument ignored -- use GUI features");
        case "xdata"
          xdata = varargin{narg++};
          if (! isvector (xdata))
            error ("imshow: xdata must be a vector")
          endif
          xdata = [xdata(1) xdata(end)];
        case "ydata"
          ydata = varargin{narg++};
          if (! isvector (ydata))
            error ("imshow: ydata must be a vector")
          endif
          ydata = [ydata(1) ydata(end)];
        otherwise
          warning ("imshow: unrecognized property %s", arg);
          narg++;
      endswitch
    else
      error ("imshow: argument number %d is invalid", narg);
    endif
  endwhile

  ## Check for complex images.
  if (iscomplex (im))
    warning ("imshow: only showing real part of complex image");
    im = real (im);
  endif

  ## Set default display range if display_range not set yet.
  if (isempty (display_range))
    display_range = [min(im(:)), max(im(:))];
  elseif (isna (display_range))
    t = class (im);
    switch (t)
      case {"double", "single", "logical"}
        display_range = [0, 1];
      case {"uint8", "uint16", "int16"}
        display_range = [intmin(t), intmax(t)];
      otherwise
        error ("imshow: invalid data type for image");
    endswitch
  endif

  if (isfloat (im))
    nans = isnan (im(:));
    if (any (nans))
      warning ("Octave:imshow-NaN",
               "imshow: pixels with NaN or NA values are set to minimum pixel value");
      im(nans) = display_range(1);
    endif
  endif

  ## FIXME: Commented out 2014/05/01.  imagesc and 'clim' will automatically
  ## take care of displaying out-of-range data clamped to the limits.
  ## Eventually, this can be deleted if no problems arise.
  ## Clamp the image to the range boundaries
  ##if (! (truecolor || indexed || islogical (im)))
  ##  low = display_range(1);
  ##  high = display_range(2);
  ##  im(im < low) = low;
  ##  im(im > high) = high;
  ##endif

  if (truecolor || indexed)
    htmp = image (xdata, ydata, im);
  else
    htmp = imagesc (xdata, ydata, im, display_range);
    set (gca (), "clim", display_range);
  endif
  set (gca (), "visible", "off", "view", [0, 90],
               "ydir", "reverse", "layer", "top");
  axis ("image");

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! imshow ("default.img");

%!demo
%! clf;
%! imshow ("default.img");
%! colormap (autumn (64));

%!demo
%! clf;
%! [I, M] = imread ("default.img");
%! imshow (I, M);

%!demo
%! clf;
%! [I, M] = imread ("default.img");
%! [R, G, B] = ind2rgb (I, M);
%! imshow (cat (3, R, G*0.5, B*0.8));

%!demo
%! clf;
%! imshow (rand (100, 100));

%!demo
%! clf;
%! imshow (rand (100, 100, 3));

%!demo
%! clf;
%! imshow (100*rand (100, 100, 3));

%!demo
%! clf;
%! imshow (rand (100, 100));
%! colormap (jet (64));

## Test input validation
%!error imshow ()
%!error <IM must be an image> imshow ({"cell"})
%!error <TrueColor image must be uint8> imshow (ones (3,3,3, "uint32"))
%!error <TrueColor image must be uint8> imshow (ones (3,3,3, "int16"))
%!error <expecting MxN or MxNx3 matrix> imshow (ones (4,4,4))

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   fail ("imshow ([1,1], [2 0 0])", "invalid colormap MAP");
%!   fail ("imshow ([1,1], [1 0 0 0])", "argument number 2 is invalid");
%!   fail ('imshow ([1,1], "colormap", [2 0 0])', "invalid colormap");
%!   fail ('imshow ([1,1], "xdata", ones (2,2))', "xdata must be a vector");
%!   fail ('imshow ([1,1], "ydata", ones (2,2))', "ydata must be a vector");
%!   fail ('imshow ([1,1], "foobar")', "warning", "unrecognized property foobar")
%!   fail ("imshow ([1,1], {1})", "argument number 2 is invalid");
%!   fail ("imshow ([1+i,1-i])", "warning", "only showing real part of complex image");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

