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
## @deftypefn {Function File} {} cscd (@var{x})
## Compute the cosecant for each element of @var{x} in degrees.
## @seealso{acscd, csc}
## @end deftypefn

## Author: David Bateman <dbateman@free.fr>

function y = cscd (x)

  if (nargin != 1)
    print_usage ();
  endif

  y = 1 ./ sind (x);

endfunction


%!assert (cscd (10:10:90), csc (pi*[10:10:90]/180), -10*eps)
%!assert (cscd ([0, 180, 360]) == Inf)
%!assert (cscd ([90, 270]) != Inf)

%!error cscd ()
%!error cscd (1, 2)

