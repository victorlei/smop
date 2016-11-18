## Copyright (C) 2014-2015 Carnë Draug
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
## @deftypefn {Function File} {[@var{x}, @var{map}] =} frame2im (@var{f})
## Convert movie frame to indexed image.
##
## A movie frame is simply a struct with the fields @qcode{"cdata"} and
## @qcode{"colormap"}.
##
## Support for N-dimensional images or movies is given when @var{f} is a
## struct array.  In such cases, @var{x} will be a @nospell{MxNx1xK or MxNx3xK}
## for indexed and RGB movies respectively, with each frame concatenated
## along the 4th dimension.
##
## @seealso{im2frame}
## @end deftypefn

## Author: Carnë Draug <carandraug@octave.org>

function [x, map] = frame2im (frame)

  if (nargin != 1)
    print_usage ();
  elseif (! all (isfield (frame, {"cdata", "colormap"})))
    error ("frame2im: F must be a struct with the fields colormap and cdata");
  endif

  n = numel (frame);
  if (n == 0)
    error ("frame2im: FRAME is empty");
  else
    x   = [frame.cdata];
    map = frame(1).colormap;
  endif

  ## support for N dimensional images if we receive a struct array
  if (n > 1)
    x = reshape (x, rows (x), columns (x) / n, n, size (frame(1).cdata, 3));
    x = permute (x, [1 2 4 3]);
  endif

endfunction


%!function f = make_rgb_f ()
%! f = randi ([0 255], 10, 20, 3);
%!endfunction

%!function f = make_ind_f ()
%! f = randi ([1 100], 10, 20, 3);
%!endfunction

%!test
%! x = make_ind_f ();
%! cmap = jet (100);
%! frame = struct ("cdata", x, "colormap", cmap);
%! [rx, rcmap] = frame2im (frame);
%! assert (rx, x);
%! assert (rcmap, cmap);

%!test
%! rgb = make_rgb_f ();
%! frame = struct ("cdata", rgb, "colormap", []);
%! [rrgb, rcmap] = frame2im (frame);
%! assert (rrgb, rgb);
%! assert (rcmap, []);

%!test
%! f1 = make_rgb_f ();
%! f2 = make_rgb_f ();
%! f3 = make_rgb_f ();
%! f4 = make_rgb_f ();
%! rgb = {f1, f2, f3, f4};
%! movie = struct ("cdata", rgb, "colormap", []);
%! [rx, rcmap] = frame2im (movie);
%! assert (rx, cat (4, f1, f2, f3, f4));
%! assert (rcmap, []);

%!test
%! f1 = make_ind_f ();
%! f2 = make_ind_f ();
%! f3 = make_ind_f ();
%! f4 = make_ind_f ();
%! ind = {f1, f2, f3, f4};
%! cmap = jet (100);
%! movie = struct ("cdata", ind, "colormap", cmap);
%! [rx, rcmap] = frame2im (movie);
%! assert (rx, cat (4, f1, f2, f3, f4));
%! assert (rcmap, cmap);

