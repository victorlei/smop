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
## @deftypefn  {Function File} {[@var{Y}, @var{newmap}] =} cmpermute (@var{X}, @var{map})
## @deftypefnx {Function File} {[@var{Y}, @var{newmap}] =} cmpermute (@var{X}, @var{map}, @var{index})
## Reorder colors in a colormap.
##
## When called with only two arguments, @code{cmpermute} randomly rearranges
## the colormap @var{map} and returns a new colormap @var{newmap}.  It also
## returns the indexed image @var{Y} which is the equivalent of the original
## input image @var{X} when displayed using @var{newmap}.
##
## When called with an optional third argument the order of colors in the new
## colormap is defined by @var{index}.
##
## @strong{Caution:} @code{index} should not have repeated elements or the
## function will fail.
##
## @end deftypefn

## Author:  Josep Mones i Teixidor <jmones@puntbarra.com>

function [Y, newmap] = cmpermute (X, map, index)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  cls = class (X);
  if (! any (strcmp (cls, {"uint8", "uint16", "single", "double"})))
    error ("cmpermute: invalid data type '%s'", cls);
  endif

  if (! isreal (X) || issparse (X)
      || (isfloat (X) && (any (X(:) < 1 || any (X(:) != fix (X(:)))))))
    error ("cmpermute: X must be an indexed image");
  endif

  if (! iscolormap (map))
    error ("cmpermute: MAP must be a valid colormap");
  endif

  if (nargin < 3)
    index = randperm (rows (map));
  elseif (! isvector (index) || length (index) != rows (map))
    error ("cmpermute: invalid parameter INDEX");
  endif

  ## new colormap
  newmap = map(index,:);

  ## build reverse index
  rindex = zeros (size (index));
  rindex(index) = 1:length (index);

  ## preserve class of input image in output
  if (strcmp (cls, "double"))
    Y = rindex(X);
  elseif (strcmp (cls, "single"))
    rindex = single (rindex);
    Y = rindex(X);
  else
    ## adapt indices
    rindex = feval (cls, rindex - 1);
    ## 0-based indices
    Y = rindex(single (X) + 1);
  endif

endfunction


%!demo
%! [Y, newmap] = cmpermute ([1:4], hot (4), 4:-1:1)
%! ## colormap will be arranged in reverse order (so will image)

%!shared X, map
%! X = uint8 (magic (16));
%! [X, map] = cmunique (X);

%!test # random permutation, 0-based index
%! [Y, newmap] = cmpermute (X, map);
%! ## test we didn't lose colors
%! assert (sort (map), sortrows (newmap));
%! ## test if images are equal
%! assert (map(double (X)+1), newmap(double (Y)+1));

%!test # reverse map, 0-based index
%! [Y, newmap] = cmpermute (X, map, rows (map):-1:1);
%! ## we expect a reversed colormap
%! assert (flipud (newmap), map);
%! ## we expect reversed indices in image
%! assert (X, max (Y(:)) - Y);

%!shared X,map
%! X = uint16 (magic (20));
%! [X, map] = cmunique (X);

%!test # random permutation, 1-based index
%! [Y, newmap] = cmpermute (X, map);
%! ## test we didn't lose colors
%! assert (sort (map), sortrows (newmap));
%! ## test if images are equal
%! assert (map(X), newmap(Y));

%!test # reverse map, 1-based index
%! [Y, newmap] = cmpermute (X, map, rows (map):-1:1);
%! ## we expect a reversed colormap
%! assert (newmap (rows (newmap):-1:1,:), map);
%! ## we expect reversed indices in image
%! assert (X, max (Y(:)) + 1 - Y);

## Test input validation
%!error cmpermute ()
%!error cmpermute (1,2,3,4)
%!error <invalid data type 'uint32'> cmpermute (uint32 (magic (16)), jet (256))
%!error <X must be an indexed image> cmpermute (1+i, jet (256))
%!error <X must be an indexed image> cmpermute (sparse (1), jet (256))
%!error <X must be an indexed image> cmpermute (0, jet (256))
%!error <X must be an indexed image> cmpermute (1.5, jet (256))
%!error <MAP must be a valid colormap> cmpermute (1, "a")
%!error <MAP must be a valid colormap> cmpermute (1, i)
%!error <MAP must be a valid colormap> cmpermute (1, ones (3,3,3))
%!error <MAP must be a valid colormap> cmpermute (1, ones (3,2))
%!error <MAP must be a valid colormap> cmpermute (1, [-1 1 1])
%!error <MAP must be a valid colormap> cmpermute (1, [2 1 1])
%!error <invalid parameter INDEX> cmpermute (1, [0 1 0;1 0 1], ones (3))
%!error <invalid parameter INDEX> cmpermute (1, [0 1 0;1 0 1], 1:3)

