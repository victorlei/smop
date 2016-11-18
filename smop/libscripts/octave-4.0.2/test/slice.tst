## Copyright (C) 2007-2015 John W. Eaton
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

%!function x = set_slice (size, dim, slice)
%!  x = ones (size);
%!  switch (dim)
%!    case 11
%!      x(slice) = 2;
%!    case 21
%!      x(slice, :) = 2;
%!    case 22
%!      x(:, slice) = 2;
%!    case 31
%!      x(slice, :, :) = 2;
%!    case 32
%!      x(:, slice, :) = 2;
%!    case 33
%!      x(:, :, slice) = 2;
%!    otherwise
%!      error ("invalid dim, '%d'", dim);
%!  endswitch
%!endfunction

## size = [2 0]
%!assert (set_slice ([2 0], 11, []), zeros ([2 0]))
%!error id=Octave:invalid-resize set_slice ([2 0], 11, 1)
%!error id=Octave:invalid-resize set_slice ([2 0], 11, 2)
%!error id=Octave:invalid-resize set_slice ([2 0], 11, 3)
%!assert (set_slice ([2 0], 21, []), zeros ([2 0]))
%!assert (set_slice ([2 0], 21, 1), zeros ([2 0]))
%!assert (set_slice ([2 0], 21, 2), zeros ([2 0]))
%!assert (set_slice ([2 0], 21, 3), zeros ([3 0]))
%!assert (set_slice ([2 0], 22, []), zeros ([2 0]))
%!assert (set_slice ([2 0], 22, 1), [2 2]')
%!assert (set_slice ([2 0], 22, 2), [0 0;2 2]')
%!assert (set_slice ([2 0], 22, 3), [0 0;0 0;2 2]')
%!assert (set_slice ([2 0], 31, []), zeros ([2 0]))
%!assert (set_slice ([2 0], 31, 1), zeros ([2 0]))
%!assert (set_slice ([2 0], 31, 2), zeros ([2 0]))
%!assert (set_slice ([2 0], 31, 3), zeros ([3 0]))
%!assert (set_slice ([2 0], 32, []), zeros ([2 0]))
%!assert (set_slice ([2 0], 32, 1), [2 2]')
%!assert (set_slice ([2 0], 32, 2), [0 0;2 2]')
%!assert (set_slice ([2 0], 32, 3), [0 0;0 0;2 2]')
%!assert (set_slice ([2 0], 33, []), zeros ([2 0]))
%!assert (set_slice ([2 0], 33, 1), zeros ([2 0]))
%!assert (set_slice ([2 0], 33, 2), zeros ([2 0 2]))
%!assert (set_slice ([2 0], 33, 3), zeros ([2 0 3]))

## size = [0 2]
%!assert (set_slice ([0 2], 11, []), zeros ([0 2]))
%!assert (set_slice ([0 2], 11, 1), 2)
%!assert (set_slice ([0 2], 11, 2), [0, 2])
%!assert (set_slice ([0 2], 11, 3), [0, 0, 2])
%!assert (set_slice ([0 2], 21, []), zeros ([0 2]))
%!assert (set_slice ([0 2], 21, 1), [2 2])
%!assert (set_slice ([0 2], 21, 2), [0 0;2 2])
%!assert (set_slice ([0 2], 21, 3), [0 0;0 0;2 2])
%!assert (set_slice ([0 2], 22, []), zeros ([0 2]))
%!assert (set_slice ([0 2], 22, 1), zeros ([0 2]))
%!assert (set_slice ([0 2], 22, 2), zeros ([0 2]))
%!assert (set_slice ([0 2], 22, 3), zeros ([0 3]))
%!assert (set_slice ([0 2], 31, []), zeros ([0 2]))
%!assert (set_slice ([0 2], 31, 1), [2 2])
%!assert (set_slice ([0 2], 31, 2), [0 0;2 2])
%!assert (set_slice ([0 2], 31, 3), [0 0;0 0;2 2])
%!assert (set_slice ([0 2], 32, []), zeros ([0 2]))
%!assert (set_slice ([0 2], 32, 1), zeros ([0 2]))
%!assert (set_slice ([0 2], 32, 2), zeros ([0 2]))
%!assert (set_slice ([0 2], 32, 3), zeros ([0 3]))
%!assert (set_slice ([0 2], 33, []), zeros ([0 2]))
%!assert (set_slice ([0 2], 33, 1), zeros ([0 2]))
%!assert (set_slice ([0 2], 33, 2), zeros ([0 2 2]))
%!assert (set_slice ([0 2], 33, 3), zeros ([0 2 3]))

## size = [2 1]
%!assert (set_slice ([2 1], 11, []), ones ([2 1]))
%!assert (set_slice ([2 1], 11, 1), [2 1]')
%!assert (set_slice ([2 1], 11, 2), [1 2]')
%!assert (set_slice ([2 1], 11, 3), [1 1 2]')
%!assert (set_slice ([2 1], 11, 4), [1 1 0 2]')
%!assert (set_slice ([2 1], 21, []), ones ([2 1]))
%!assert (set_slice ([2 1], 21, 1), [2 1]')
%!assert (set_slice ([2 1], 21, 2), [1 2]')
%!assert (set_slice ([2 1], 21, 3), [1 1 2]')
%!assert (set_slice ([2 1], 21, 4), [1 1 0 2]')
%!assert (set_slice ([2 1], 22, []), ones ([2 1]))
%!assert (set_slice ([2 1], 22, 1), [2 2]')
%!assert (set_slice ([2 1], 22, 2), [1 1;2 2]')
%!assert (set_slice ([2 1], 22, 3), [1 1;0 0;2 2]')
%!assert (set_slice ([2 1], 31, []), ones ([2 1]))
%!assert (set_slice ([2 1], 31, 1), [2 1]')
%!assert (set_slice ([2 1], 31, 2), [1 2]')
%!assert (set_slice ([2 1], 31, 3), [1 1 2]')
%!assert (set_slice ([2 1], 31, 4), [1 1 0 2]')
%!assert (set_slice ([2 1], 32, []), ones ([2 1]))
%!assert (set_slice ([2 1], 32, 1), [2 2]')
%!assert (set_slice ([2 1], 32, 2), [1 1;2 2]')
%!assert (set_slice ([2 1], 32, 3), [1 1;0 0;2 2]')
%!assert (set_slice ([2 1], 33, []), ones ([2 1]))
%!assert (set_slice ([2 1], 33, 1), [2 2]')
%!assert (set_slice ([2 1], 33, 2), reshape ([1 1 2 2],[2 1 2]))
%!assert (set_slice ([2 1], 33, 3), reshape ([1 1 0 0 2 2],[2 1 3]))

## size = [1 2]
%!assert (set_slice ([1 2], 11, []), full (ones ([1 2])))
%!assert (set_slice ([1 2], 11, 1), [2 1])
%!assert (set_slice ([1 2], 11, 2), [1 2])
%!assert (set_slice ([1 2], 11, 3), [1 1 2])
%!assert (set_slice ([1 2], 11, 4), [1 1 0 2])
%!assert (set_slice ([1 2], 21, []), full (ones ([1 2])))
%!assert (set_slice ([1 2], 21, 1), [2 2])
%!assert (set_slice ([1 2], 21, 2), [1 1;2 2])
%!assert (set_slice ([1 2], 21, 3), [1 1;0 0;2 2])
%!assert (set_slice ([1 2], 22, []), full (ones ([1 2])))
%!assert (set_slice ([1 2], 22, 1), [2 1])
%!assert (set_slice ([1 2], 22, 2), [1 2])
%!assert (set_slice ([1 2], 22, 3), [1 1 2])
%!assert (set_slice ([1 2], 22, 4), [1 1 0 2])
%!assert (set_slice ([1 2], 31, []), full (ones ([1 2])))
%!assert (set_slice ([1 2], 31, 1), [2 2])
%!assert (set_slice ([1 2], 31, 2), [1 1;2 2])
%!assert (set_slice ([1 2], 31, 3), [1 1;0 0;2 2])
%!assert (set_slice ([1 2], 32, []), full (ones ([1 2])))
%!assert (set_slice ([1 2], 32, 1), [2 1])
%!assert (set_slice ([1 2], 32, 2), [1 2])
%!assert (set_slice ([1 2], 32, 3), [1 1 2])
%!assert (set_slice ([1 2], 32, 4), [1 1 0 2])
%!assert (set_slice ([1 2], 33, []), full (ones ([1 2])))
%!assert (set_slice ([1 2], 33, 1), [2 2])
%!assert (set_slice ([1 2], 33, 2), reshape ([1 1 2 2],[1 2 2]))
%!assert (set_slice ([1 2], 33, 3), reshape ([1 1 0 0 2 2],[1 2 3]))

## size = [2 2]
%!assert (set_slice ([2 2], 11, []), ones ([2 2]))
%!assert (set_slice ([2 2], 11, 1), [2 1;1 1])
%!assert (set_slice ([2 2], 11, 2), [1 1;2 1])
%!assert (set_slice ([2 2], 11, 3), [1 2;1 1])
%!assert (set_slice ([2 2], 11, 4), [1 1;1 2])
%!error id=Octave:invalid-resize set_slice ([2 2], 11, 5)
%!error id=Octave:invalid-resize set_slice ([2 2], 11, 6)
%!assert (set_slice ([2 2], 21, []), ones ([2 2]))
%!assert (set_slice ([2 2], 21, 1), [2 2;1 1])
%!assert (set_slice ([2 2], 21, 2), [1 1;2 2])
%!assert (set_slice ([2 2], 21, 3), [1 1;1 1;2 2])
%!assert (set_slice ([2 2], 21, 4), [1 1;1 1;0 0;2 2])
%!assert (set_slice ([2 2], 22, []), ones ([2 2]))
%!assert (set_slice ([2 2], 22, 1), [2 2;1 1]')
%!assert (set_slice ([2 2], 22, 2), [1 1;2 2]')
%!assert (set_slice ([ 2 2], 22, 3), [1 1;1 1;2 2]')
%!assert (set_slice ([2 2], 22, 4), [1 1;1 1;0 0;2 2]')
%!assert (set_slice ([2 2], 31, []), ones ([2 2]))
%!assert (set_slice ([2 2], 31, 1), [2 2;1 1])
%!assert (set_slice ([2 2], 31, 2), [1 1;2 2])
%!assert (set_slice ( [2 2], 31, 3), [1 1;1 1;2 2])
%!assert (set_slice ([2 2], 31, 4), [1 1;1 1;0 0;2 2])
%!assert (set_slice ([2 2], 32, []), ones ([2 2]))
%!assert (set_slice ([2 2], 32, 1), [2 2;1 1]')
%!assert (set_slice ([2 2], 32, 2), [1 1;2 2]')
%!assert (set_slice ([ 2 2], 32, 3), [1 1;1 1;2 2]')
%!assert (set_slice ([2 2], 32, 4), [1 1;1 1;0 0;2 2]')
%!assert (set_slice ([2 2], 33, []), ones ([2 2]))
%!assert (set_slice ([2 2], 33, 1), [2 2;2 2])
%!assert (set_slice ([2 2], 33, 2), reshape ([1 1 1 1 2 2 2 2],[2 2 2]))
%!assert (set_slice ([ 2 2], 33, 3), reshape ([1 1 1 1 0 0 0 0 2 2 2 2],[2 2 3]))
