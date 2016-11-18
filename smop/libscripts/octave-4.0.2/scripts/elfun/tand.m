## Copyright (C) 2006-2015 David Bateman
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
## @deftypefn {Function File} {} tand (@var{x})
## Compute the tangent for each element of @var{x} in degrees.
##
## Returns zero for elements where @code{@var{x}/180} is an integer and
## @code{Inf} for elements where @code{(@var{x}-90)/180} is an integer.
## @seealso{atand, tan}
## @end deftypefn

## Author: David Bateman <dbateman@free.fr>

function y = tand (x)

  if (nargin != 1)
    print_usage ();
  endif

  I0 = x / 180;
  I90 = (x-90) / 180;
  y = tan (I0 .* pi);
  y(I0 == fix (I0) & isfinite (I0)) = 0;
  y(I90 == fix (I90) & isfinite (I90)) = Inf;

endfunction


%!assert (tand (10:10:80), tan (pi*[10:10:80]/180), -10*eps)
%!assert (tand ([0, 180, 360]) == 0)
%!assert (tand ([90, 270]) == Inf)

%!error tand ()
%!error tand (1, 2)

