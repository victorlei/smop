## Copyright (C) 2014-2015 Massimiliano Fasi
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
## @deftypefn  {Function File} {} hgsave (@var{filename})
## @deftypefnx {Function File} {} hgsave (@var{h}, @var{filename})
## @deftypefnx {Function File} {} hgsave (@var{h}, @var{filename}, @var{fmt})
## Save the graphics handle @var{h} to the file @var{filename} in the format
## @var{fmt}.
##
## If unspecified, @var{h} is the current figure as returned by @code{gcf}.
##
## When @var{filename} does not have an extension the default filename
## extension @file{.ofig} will be appended.
##
## If present, @var{fmt} should be one of the following:
##
## @itemize @bullet
## @item @option{-binary}, @option{-float-binary}
##
## @item @option{-hdf5}, @option{-float-hdf5}
##
## @item @option{-V7}, @option{-v7}, @code{-7}, @option{-mat7-binary}
##
## @item @option{-V6}, @option{-v6}, @code{-6}, @option{-mat6-binary}
##
## @item @option{-text}
##
## @item @option{-zip}, @option{-z}
## @end itemize
##
## When producing graphics for final publication use @code{print} or
## @code{saveas}.  When it is important to be able to continue to edit a
## figure as an Octave object, use @code{hgsave}/@code{hgload}.
## @seealso{hgload, hdl2struct, saveas, print}
## @end deftypefn

## Author: Massimiliano Fasi

function hgsave (h, filename, fmt = "-binary")

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  ## Check input arguments
  if (nargin == 1 && ischar (h))
    filename = h;
    h = get (0, "currentfigure");
    if (isempty (h))
      error ("hgsave: No current figure to save");
    endif
  elseif (! (ishandle (h) && ischar (filename)))
    print_usage ();
  endif

  ## Check file extension
  [~, ~, ext] = fileparts (filename);
  if (isempty (ext))
    filename = [filename ".ofig"];
  endif

  s_oct40 = hdl2struct (h);
  save (fmt, filename, "s_oct40");

endfunction


%!testif HAVE_MAGICK
%! toolkit = graphics_toolkit ();
%! graphics_toolkit ("gnuplot");
%! unwind_protect
%!   h1 = figure ("visible", "off");
%!   x = 0:0.1:2*pi;
%!   y1 = sin (x);
%!   y2 = exp (x - 1);
%!   ax = plotyy (x,y1, x-1,y2, @plot, @semilogy);
%!   xlabel ("X");
%!   ylabel (ax(1), "Axis 1");
%!   ylabel (ax(2), "Axis 2");
%!   axes (ax(1));
%!   text (0.5, 0.5, "Left Axis", ...
%!         "color", [0 0 1], "horizontalalignment", "center");
%!   axes (ax(2));
%!   text (4.5, 80, "Right Axis", ...
%!         "color", [0 0.5 0], "horizontalalignment", "center");
%!   ftmp = [tempname() ".ofig"];
%!   png1 = [tempname() ".png"];
%!   png2 = [tempname() ".png"];
%!   unwind_protect
%!     hgsave (h1, ftmp);
%!     print (h1, png1);
%!     [img1, map1, alpha1] = imread (png1);
%!     h2 = hgload (ftmp);
%!     print (h2, png2);
%!     [img2, map2, alpha2] = imread (png2);
%!   unwind_protect_cleanup
%!     unlink (ftmp);
%!     unlink (png1);
%!     unlink (png2);
%!   end_unwind_protect
%!   assert (img1, img2);
%!   assert (map1, map2);
%!   assert (alpha1, alpha2);
%! unwind_protect_cleanup
%!   close (h1);
%!   close (h2);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

## Test input validation
%!error hgsave ()
%!error hgsave (1, 2, 3, 4)
%!error hgsave ("abc", "def")

