## Copyright (C) 2008-2015 John W. Eaton
## Copyright (C) 2013-2015 Carnë Draug
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
## @deftypefn  {Function File} {} imwrite (@var{img}, @var{filename})
## @deftypefnx {Function File} {} imwrite (@var{img}, @var{filename}, @var{ext})
## @deftypefnx {Function File} {} imwrite (@var{img}, @var{map}, @var{filename})
## @deftypefnx {Function File} {} imwrite (@dots{}, @var{param1}, @var{val1}, @dots{})
## Write images in various file formats.
##
## The image @var{img} can be a binary, grayscale, RGB, or multi-dimensional
## image.  The size and class of @var{img} should be the same as what should
## be expected when reading it with @code{imread}: the 3rd and 4th dimensions
## reserved for color space, and multiple pages respectively.  If it's an
## indexed image, the colormap @var{map} must also be specified.
##
## If @var{ext} is not supplied, the file extension of @var{filename} is used
## to determine the format.  The actual supported formats are dependent on
## options made during the build of Octave.  Use @code{imformats} to check
## the support of the different image formats.
##
## Depending on the file format, it is possible to configure the writing of
## images with @var{param}, @var{val} pairs.  The following options are
## supported:
##
## @table @samp
## @item Alpha
## Alpha (transparency) channel for the image.  This must be a matrix with
## same class, and number of rows and columns of @var{img}.  In case of a
## multipage image, the size of the 4th dimension must also match and the third
## dimension must be a singleton.  By default, image will be completely opaque.
##
## @item DelayTime
## For formats that accept animations (such as GIF), controls for how long a
## frame is displayed until it moves to the next one.  The value must be scalar
## (which will applied to all frames in @var{img}), or a vector of length
## equal to the number of frames in @var{im}.  The value is in seconds, must
## be between 0 and 655.35, and defaults to 0.5.
##
## @item DisposalMethod
## For formats that accept animations (such as GIF), controls what happens to
## a frame before drawing the next one.  Its value can be one of the
## following strings: "doNotSpecify" (default); "leaveInPlace"; "restoreBG";
## and "restorePrevious", or a cell array of those string with length equal
## to the number of frames in @var{img}.
##
## @item LoopCount
## For formats that accept animations (such as GIF), controls how many times
## the sequence is repeated.  A value of Inf means an infinite loop (default),
## a value of 0 or 1 that the sequence is played only once (loops zero times),
## while a value of 2 or above loops that number of times (looping twice means
## it plays the complete sequence 3 times).  This option is ignored when there
## is only a single image at the end of writing the file.
##
## @item Quality
## Set the quality of the compression.  The value should be an integer
## between 0 and 100, with larger values indicating higher visual quality and
## lower compression.  Defaults to 75.
##
## @item WriteMode
## Some file formats, such as TIFF and GIF, are able to store multiple images
## in a single file.  This option specifies if @var{img} should be appended
## to the file (if it exists) or if a new file should be created for it
## (possibly overwriting an existing file).  The value should be the string
## @qcode{"Overwrite"} (default), or @qcode{"Append"}.
##
## Despite this option, the most efficient method of writing a multipage
## image is to pass a 4 dimensional @var{img} to @code{imwrite}, the same
## matrix that could be expected when using @code{imread} with the option
## @qcode{"Index"} set to @qcode{"all"}.
##
## @end table
##
## @seealso{imread, imfinfo, imformats}
## @end deftypefn

function imwrite (varargin)
  if (nargin < 2)
    print_usage ();
  endif
  [filename, ext] = imwrite_filename (varargin{2:end});

  fmt = imformats (ext);
  ## When there is no match, fmt will be a 1x1 structure with
  ## no fields, so we can't just use `isempty (fmt)'.
  if (numfields (fmt) == 0)
    if (isempty (ext))
      error ("imwrite: no extension found for %s to identify the image format",
             filename);
    endif
    warning ("imwrite: unlisted image format %s (see imformats). Trying to save anyway.",
             ext);
    __imwrite__ (varargin{:});
  else
    fmt.write (varargin{:});
  endif

endfunction

## Test input validation
%!error imwrite ()                            # Wrong # of args
%!error imwrite (1)                           # Wrong # of args
%!error imwrite ({"cell"}, "filename.jpg")    # Wrong class for img
%!error imwrite (1, [], "filename.jpg")       # Empty image map
%!error imwrite (1, 2, 3)                     # No filename specified
%!error imwrite (1, "filename")               # No fmt specified
%!error imwrite (1, "filename", "junk")       # Invalid fmt specified
%!error imwrite ([], "filename.jpg")          # Empty img matrix
%!error imwrite (spones (2), "filename.jpg")  # Invalid sparse img

%!function [r, cmap, a] = write_and_read (format, varargin)
%!  filename = [tempname() format];
%!  unwind_protect
%!    imwrite (varargin{1}, filename, varargin{2:end});
%!    [r, cmap, a] = imread (filename, "Index", "all");
%!  unwind_protect_cleanup
%!    unlink (filename);
%!  end_unwind_protect
%!endfunction

## typical usage with grayscale uint8 images
%!testif HAVE_MAGICK
%! gray  = randi (255, 10, 10, 1, "uint8");
%! r  = write_and_read (".tif", gray);
%! assert (r, gray)

## grayscale uint8 images with alpha channel
%!testif HAVE_MAGICK
%! gray  = randi (255, 10, 10, 1, "uint8");
%! alpha = randi (255, 10, 10, 1, "uint8");
%! [r, ~, a] = write_and_read (".tif", gray, "Alpha", alpha);
%! assert (r, gray)
%! assert (a, alpha)

## multipage grayscale uint8 images
%!testif HAVE_MAGICK
%! gray  = randi (255, 10, 10, 1, 5, "uint8");
%! r     = write_and_read (".tif", gray);
%! assert (r, gray)

## multipage RGB uint8 images with alpha channel
%!testif HAVE_MAGICK
%! gray  = randi (255, 10, 10, 3, 5, "uint8");
%! alpha = randi (255, 10, 10, 1, 5, "uint8");
%! [r, ~, a] = write_and_read (".tif", gray, "Alpha", alpha);
%! assert (r, gray)
%! assert (a, alpha)

## typical usage with RGB uint8 images
%!testif HAVE_MAGICK
%! rgb = randi (255, 10, 10, 3, "uint8");
%! r = write_and_read (".tif", rgb);
%! assert (r, rgb)

## RGB uint8 images with alpha channel
%!testif HAVE_MAGICK
%! rgb   = randi (255, 10, 10, 3, "uint8");
%! alpha = randi (255, 10, 10, 1, "uint8");
%! [r, ~, a] = write_and_read (".tif", rgb, "Alpha", alpha);
%! assert (r, rgb)
%! assert (a, alpha)

## multipage RGB uint8 images
%!testif HAVE_MAGICK
%! rgb = randi (255, 10, 10, 3, 5, "uint8");
%! r = write_and_read (".tif", rgb);
%! assert (r, rgb)

## multipage RGB uint8 images with alpha channel
%!testif HAVE_MAGICK
%! rgb   = randi (255, 10, 10, 3, 5, "uint8");
%! alpha = randi (255, 10, 10, 1, 5, "uint8");
%! [r, ~, a] = write_and_read (".tif", rgb, "Alpha", alpha);
%! assert (r, rgb)
%! assert (a, alpha)

%!testif HAVE_MAGICK
%! gray = repmat (uint8 (0:255), 100, 1);
%! [g] = write_and_read (".jpeg", gray);
%! assert (g, gray, 2)

%!testif HAVE_MAGICK
%! gray = repmat (uint8 (0:255), 100, 1);
%! [g] = write_and_read (".jpeg", gray, "quality", 100);
%! assert (g, gray)

