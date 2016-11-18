## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Function File} {@var{idx} =} dsearchn (@var{x}, @var{tri}, @var{xi})
## @deftypefnx {Function File} {@var{idx} =} dsearchn (@var{x}, @var{tri}, @var{xi}, @var{outval})
## @deftypefnx {Function File} {@var{idx} =} dsearchn (@var{x}, @var{xi})
## @deftypefnx {Function File} {[@var{idx}, @var{d}] =} dsearchn (@dots{})
## Return the index @var{idx} of the closest point in @var{x} to the elements
## @var{xi}.
##
## If @var{outval} is supplied, then the values of @var{xi} that are not
## contained within one of the simplices @var{tri} are set to @var{outval}.
## Generally, @var{tri} is returned from @code{delaunayn (@var{x})}.
## @seealso{dsearch, tsearch}
## @end deftypefn

function [idx, d] = dsearchn (x, tri, xi, outval)
  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  if (nargin == 2)
    [idx, d] = __dsearchn__ (x, tri);
  else
    [idx, d] = __dsearchn__ (x, xi);
    if (nargin == 4)
      idx2 = isnan (tsearchn (x, tri, xi));
      idx(idx2) = outval;
      d(idx2) = outval;
    endif
  endif
endfunction


%!shared x, tri
%! x = [-1,-1;-1,1;1,-1];
%! tri = [1,2,3];
%!assert (dsearchn (x,tri,[1,1/3]), 3)
%!assert (dsearchn (x,tri,[1,1/3],NaN), NaN)
%!assert (dsearchn (x,tri,[1,1/3],NA), NA)
%!assert (dsearchn (x,tri,[1/3,1]), 2)
%!assert (dsearchn (x,tri,[1/3,1],NaN), NaN)
%!assert (dsearchn (x,tri,[1/3,1],NA), NA)

