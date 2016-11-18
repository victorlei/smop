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

## Test values of range

%!assert (full (1:9), [ 1 2 3 4 5 6 7 8 9 ])
%!assert (full (1:0.4:3), [ 1.0 1.4 1.8 2.2 2.6 3.0 ])
%!assert (full (9:1), zeros (1,0))
%!assert (full (9:-1:1), [ 9 8 7 6 5 4 3 2 1 ])
%!assert (full (1:-1:9), zeros (1,0))
%!assert (full (1:1:1), 1)
%!assert (full (i:2i:10i), zeros (1,0))

## Test mixing integer range with other types

%!shared expect, r, z
%! expect = [ 1 2 3 4 5 6 7 8 9
%!            0 0 0 0 0 0 0 0 0 ];
%! z = zeros (1,9);
%! r = 1:9;

%!assert ([ r ; z                  ], expect)
%!assert ([ r ; single(z)          ], single (expect))
%!assert ([ r ; logical(z)         ], expect)
%!assert ([ r ; sparse(z)          ], sparse (expect))
%!assert ([ r ; sparse(logical(z)) ], sparse (expect))

%!assert ([ r ; int8(z)            ], int8 (expect))
%!assert ([ r ; int16(z)           ], int16 (expect))
%!assert ([ r ; int32(z)           ], int32 (expect))
%!assert ([ r ; int64(z)           ], int64 (expect))
%!assert ([ r ; uint8(z)           ], uint8 (expect))
%!assert ([ r ; uint16(z)          ], uint16 (expect))
%!assert ([ r ; uint32(z)          ], uint32 (expect))
%!assert ([ r ; uint64(z)          ], uint64 (expect))

## Test mixing non-integer range with other types

%!shared expect, r, z
%! expect = [ 1.0 1.4 1.8 2.2 2.6 3.0
%!            0   0   0   0   0   0   ];
%! z = zeros (1,6);
%! r = 1:0.4:3;

%!assert ([ r ; z                  ], expect)
%!assert ([ r ; single(z)          ], single (expect))
%!assert ([ r ; logical(z)         ], expect)
%!assert ([ r ; sparse(z)          ], sparse (expect))
%!assert ([ r ; sparse(logical(z)) ], sparse (expect))

%!assert ([ r ; int8(z)            ], int8 (expect))
%!assert ([ r ; int16(z)           ], int16 (expect))
%!assert ([ r ; int32(z)           ], int32 (expect))
%!assert ([ r ; int64(z)           ], int64 (expect))
%!assert ([ r ; uint8(z)           ], uint8 (expect))
%!assert ([ r ; uint16(z)          ], uint16 (expect))
%!assert ([ r ; uint32(z)          ], uint32 (expect))
%!assert ([ r ; uint64(z)          ], uint64 (expect))

## Test corner cases of ranges (base and limit)

%!shared r, rrev, rneg
%! r = -0:3;
%! rrev = 3:-1:-0;
%! rneg = -3:-0;

%!assert (full (r), [-0 1 2 3])
%!assert (signbit (full (r)), logical ([1 0 0 0]))
%!assert (r(1), -0)
%!assert (signbit (r(1)), true)
%!assert (signbit (r(1:2)), logical ([1 0]))
%!assert (signbit (r(2:-1:1)), logical ([0 1]))
%!assert (signbit (r([2 1 1 3])), logical ([0 1 1 0]))

%!assert (full (rrev), [3 2 1 -0])
%!assert (signbit (full (rrev)), logical ([0 0 0 1]))
%!assert (rrev(4), -0)
%!assert (signbit (rrev(4)), true)
%!assert (signbit (rrev(3:4)), logical ([0 1]))
%!assert (signbit (rrev(4:-1:3)), logical ([1 0]))
%!assert (signbit (rrev([1 4 4 2])), logical ([0 1 1 0]))

%!assert (min (r), -0)
%!assert (signbit (min (r)), true)
%!assert (min (rrev), -0)
%!assert (signbit (min (rrev)), true)

%!assert (max (rneg), -0)
%!assert (signbit (max (rneg)), true)

%!assert (sort (r, "descend"), [3 2 1 -0])
%!assert (signbit (sort (r, "descend")), logical ([0 0 0 1]))
%!assert (signbit (sort (rrev, "ascend")), logical ([1 0 0 0]))

## Test sorting of ranges (bug #45739)

%!shared r, rrev
%! r = 1:2:10;
%! rrev = 10:-2:1;

%!assert (sort (r, "descend"), [9 7 5 3 1])
%!assert (sort (rrev, "ascend"), [2 4 6 8 10])

