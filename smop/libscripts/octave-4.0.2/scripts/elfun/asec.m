## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn {Mapping Function} {} asec (@var{x})
## Compute the inverse secant in radians for each element of @var{x}.
## @seealso{sec, asecd}
## @end deftypefn

## Author: jwe

function y = asec (x)

  if (nargin != 1)
    print_usage ();
  endif

  y = acos (1 ./ x);

endfunction


%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! x = [1, 2*rt3/3, rt2, 2, -2, -rt2, -2*rt3/3, -1];
%! assert (asec (x), v, sqrt (eps));

%!error asec ()
%!error asec (1, 2)

