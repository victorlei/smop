## Copyright (C) 2006-2015 John W. Eaton
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

%!test
%! a = [];
%! fail ("a(0);");

%!shared a
%! a = 2;
%!assert (a(1), 2);
%!error id=Octave:index-out-of-bounds a(logical ([1,1]))

%!shared a
%! a = [9,8,7,6];
%!assert (isempty (a(logical ([0,0,0,0]))))
%!assert (a(logical ([1,1,1,1])), [9,8,7,6])
%!assert (a(logical ([0,1,1,0])), [8,7])
%!assert (a(logical ([1,1])), [9,8])

%!shared a
%! a = [9,8;7,6];
%!assert (isempty (a(logical ([0,0,0,0]))))
%!assert (a(logical ([1,1,1,1])), [9,7,8,6])
%!assert (a(logical ([0,1,1,0])), [7,8])
%!assert (a(logical (0:1),logical (0:1)), 6)
%!assert (a(logical (0:1),2:-1:1), [6,7])
%!assert (a(logical (0:1),logical ([0,1])), 6)
%!assert (a(logical (0:1),[2,1]), [6,7])
%!assert (a(logical (0:1),:), [7,6])
%!assert (a(logical (0:1),1), 7)
%!assert (a(logical (0:1),logical ([1,1])), [7,6])
%!assert (a(2:-1:1,logical (0:1)), [6;8])
%!assert (a(2:-1:1,logical ([0,1])), [6;8])
%!assert (a(2:-1:1,logical ([1,1])), [7,6;9,8])
%!assert (a(logical ([0,1]),logical (0:1)), 6)
%!assert (a(logical ([0,1]),2:-1:1), [6,7])
%!assert (a(logical ([0,1]),logical ([0,1])), 6)
%!assert (a(logical ([0,1]),[2,1]), [6,7])
%!assert (a(logical ([0,1]),:), [7,6])
%!assert (a(logical ([0,1]),1), 7)
%!assert (a(logical ([0,1]),logical ([1,1])), [7,6])
%!assert (a([2,1],logical (0:1)), [6;8])
%!assert (a([2,1],logical ([0,1])), [6;8])
%!assert (a([2,1],logical ([1,1])), [7,6;9,8])
%!assert (a(:,logical (0:1)), [8;6])
%!assert (a(:,logical ([0,1])), [8;6])
%!assert (a(:,logical ([1,1])), [9,8;7,6])
%!assert (a(1,logical (0:1)), 8)
%!assert (a(1,logical ([0,1])), 8)
%!assert (a(1,logical ([1,1])), [9,8])
%!assert (a(logical ([1,1]),logical (0:1)), [8;6])
%!assert (a(logical ([1,1]),2:-1:1), [8,9;6,7])
%!assert (a(logical ([1,1]),logical ([0,1])), [8;6])
%!assert (a(logical ([1,1]),[2,1]), [8,9;6,7])
%!assert (a(logical ([1,1]),:), [9,8;7,6])
%!assert (a(logical ([1,1]),1), [9;7])
%!assert (a(logical ([1,1]),logical ([1,1])), [9,8;7,6])

