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
## @deftypefn {Function File} {} cosd (@var{x})
## Compute the cosine for each element of @var{x} in degrees.
##
## Returns zero for elements where @code{(@var{x}-90)/180} is an integer.
## @seealso{acosd, cos}
## @end deftypefn

## Author: David Bateman <dbateman@free.fr>

function y = cosd (x)

  if (nargin != 1)
    print_usage ();
  endif

  I = x / 180;
  y = cos (I .* pi);
  I = I + 0.5;
  y(I == fix (I) & isfinite (I)) = 0;

endfunction


%!assert (cosd (0:10:80), cos (pi*[0:10:80]/180), -10*eps)
%!assert (cosd ([0, 180, 360]) != 0)
%!assert (cosd ([90, 270]) == 0)

%!error cosd ()
%!error cosd (1, 2)

