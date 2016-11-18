## Copyright (C) 2015 Rik Wehbring
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

## Test ordering of complex values by magnitude and then by phase
%!test
%! x = [0 i 1+i 2 3i 3+4i];
%! assert (sort (x, "descend"), fliplr (x));
%! assert (sort (single (x), "descend"), fliplr (single (x)));

%!test
%! x = [1, -1, i, -i];
%! xs = [-i, 1, i, -1];
%! assert (sort (x), xs);
%! assert (sort (x, "descend"), fliplr (xs));
%! assert (sort (single (x)), single (xs));
%! assert (sort (single (x), "descend"), fliplr (single (xs)));

## bug #44071, issorted incorrect because it uses different sort routine.
%!assert (issorted ([1, -1, i, -i]), false)
%!assert (issorted (single ([1, -1, i, -i])), false)

## bug #43313, -1 is both '>' and '==' to (-1 - 0i)
%!test
%! assert (complex(-1,0) == complex(-1,-0), true);
%! assert (complex(-1,0) > complex(-1,-0), false);
%! assert (complex(-1,0) < complex(-1,-0), false);

## Test that sort and issorted both agree on boundary case
%!test
%! x = [complex(-1,0), complex(-1,-0), i, -i, 1];
%! xs = sort (x);
%! xf = single (x);
%! xfs = sort (xf);
%! assert (issorted (xs));
%! assert (issorted (xfs));
%! assert (double (xfs), xs);

## Finally, test that sort and issorted agree on NaNs
%!test
%! x = [complex(NaN,-1), complex(NaN,NaN), ...
%!      complex(-1,0), complex(-1,-0), i, -i, 1, ...
%!      complex(1,NaN)];
%! xs = sort (x);
%! xf = single (x);
%! xfs = sort (xf);
%! assert (issorted (xs));
%! assert (issorted (xfs));
%! assert (double (xfs), xs);

