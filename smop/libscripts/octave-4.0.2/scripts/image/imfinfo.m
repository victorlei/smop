## Copyright (C) 2008-2015 Soren Hauberg
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
## @deftypefn  {Function File} {@var{info} =} imfinfo (@var{filename})
## @deftypefnx {Function File} {@var{info} =} imfinfo (@var{url})
## @deftypefnx {Function File} {@var{info} =} imfinfo (@dots{}, @var{ext})
## Read image information from a file.
##
## @code{imfinfo} returns a structure containing information about the image
## stored in the file @var{filename}.  If there is no file @var{filename},
## and @var{ext} was specified, it will look for a file named @var{filename}
## and extension @var{ext}, i.e., a file named @var{filename}.@var{ext}.
##
## The output structure @var{info} contains the following fields:
##
## @table @samp
## @item Filename
## The full name of the image file.
##
## @item FileModDate
## Date of last modification to the file.
##
## @item FileSize
## Number of bytes of the image on disk
##
## @item Format
## Image format (e.g., @qcode{"jpeg"}).
##
## @item Height
## Image height in pixels.
##
## @item Width
## Image Width in pixels.
##
## @item BitDepth
## Number of bits per channel per pixel.
##
## @item ColorType
## Image type.  Value is @qcode{"grayscale"}, @qcode{"indexed"},
## @qcode{"truecolor"}, @qcode{"CMYK"}, or @qcode{"undefined"}.
##
## @item XResolution
## X resolution of the image.
##
## @item YResolution
## Y resolution of the image.
##
## @item ResolutionUnit
## Units of image resolution.  Value is @qcode{"Inch"},
## @qcode{"Centimeter"}, or @qcode{"undefined"}.
##
## @item DelayTime
## Time in 1/100ths of a second (0 to 65535) which must expire before
## displaying the next image in an animated sequence.
##
## @item LoopCount
## Number of iterations to loop an animation.
##
## @item ByteOrder
## Endian option for formats that support it.  Value is @qcode{"little-endian"},
## @qcode{"big-endian"}, or @qcode{"undefined"}.
##
## @item Gamma
## Gamma level of the image.  The same color image displayed on two different
## workstations may look different due to differences in the display monitor.
##
## @item Quality
## JPEG/MIFF/PNG compression level.  Value is an integer in the range [0 100].
##
## @item DisposalMethod
## Only valid for GIF images, control how successive frames are rendered (how
## the preceding frame is disposed of) when creating a GIF animation.  Values
## can be @qcode{"doNotSpecify"}, @qcode{"leaveInPlace"}, @qcode{"restoreBG"},
## or @qcode{"restorePrevious"}.  For non-GIF files, value is an empty string.
##
## @item Chromaticities
## Value is a 1x8 Matrix with the x,y chromaticity values for white, red,
## green, and blue points, in that order.
##
## @item Comment
## Image comment.
##
## @item Compression
## Compression type.  Value can be @qcode{"none"}, @qcode{"bzip"},
## @qcode{"fax3"}, @qcode{"fax4"}, @qcode{"jpeg"}, @qcode{"lzw"},
## @qcode{"rle"}, @qcode{"deflate"}, @qcode{"lzma"}, @qcode{"jpeg2000"},
## @qcode{"jbig2"}, @qcode{"jbig2"}, or @qcode{"undefined"}.
##
## @item Colormap
## Colormap for each image.
##
## @item Orientation
## The orientation of the image with respect to the rows and columns.  Value
## is an integer between 1 and 8 as defined in the TIFF 6 specifications, and
## for @sc{matlab} compatibility.
##
## @item Software
## Name and version of the software or firmware of the camera or image input
## device used to generate the image.
##
## @item Make
## The manufacturer of the recording equipment.  This is the manufacture of the
## @nospell{DSC}, scanner, video digitizer or other equipment that generated
## the image.
##
## @item Model
## The model name or model number of the recording equipment as mentioned on
## the field @qcode{"Make"}.
##
## @item DateTime
## The date and time of image creation as defined by the Exif standard, i.e.,
## it is the date and time the file was changed.
##
## @item ImageDescription
## The title of the image as defined by the Exif standard.
##
## @item Artist
## Name of the camera owner, photographer or image creator.
##
## @item Copyright
## Copyright notice of the person or organization claiming rights to the image.
##
## @item DigitalCamera
## A struct with information retrieved from the Exif tag.
##
## @item GPSInfo
## A struct with geotagging information retrieved from the Exif tag.
## @end table
##
## @seealso{imread, imwrite, imshow, imformats}
## @end deftypefn

## Author: Soren Hauberg <hauberg@gmail.com>

function info = imfinfo (filename, varargin)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (! ischar (filename))
    error ("imfinfo: FILENAME must be a string");
  elseif (nargin > 1 && ! ischar (ext))
    error ("imfinfo: EXT must be a string");
  endif
  info = imageIO ("imfinfo", @__imfinfo__, "info", filename, varargin{:});
endfunction

## This test is the same as the similar one in imread. imfinfo must check
## if file exists before calling __imfinfo_. This test confirm this.
%!testif HAVE_MAGICK
%! fmt = fmt_ori = imformats ("jpg");
%! fmt.info = @true;
%! error_thrown = false;
%! imformats ("update", "jpg", fmt);
%! unwind_protect
%!   try
%!     imread ("I sure hope this file does not exist.jpg");
%!   catch
%!     error_thrown = true;
%!   end_try_catch
%! unwind_protect_cleanup
%!   imformats ("update", "jpg", fmt_ori);
%! end_unwind_protect
%! assert (error_thrown, true);

