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
## @deftypefn {Function File} {} asind (@var{x})
## Compute the inverse sine in degrees for each element of @var{x}.
## @seealso{sind, asin}
## @end deftypefn

## Author: David Bateman <dbateman@free.fr>

function y = asind (x)

  if (nargin != 1)
    print_usage ();
  endif

  y = asin (x) .* 180 ./ pi;

endfunction


%!assert (asind (0:0.1:1), 180/pi * asin (0:0.1:1), -10*eps)

%!error asind ()
%!error asind (1, 2)

