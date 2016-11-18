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
%! a = 1;
%! b = 2;
%! c = 3;
%!
%! ## "end" is part of test, check not using "endswitch"
%! switch (0) case 1 x = a; case 2 x = b; otherwise x = c; end
%! switch (1) case 1 y = a; case 2 y = b; otherwise y = c; endswitch
%! switch (2) case 1 z = a; case 2 z = b; otherwise z = c; endswitch
%! switch (3) case 1 p = a; case 2 p = b; otherwise p = c; endswitch
%!
%! assert (x == c && y == a && z == b && p == c);

%!test
%! a = 1;
%! b = 2;
%! c = 3;
%!
%! x = zeros (1, 4);
%!
%! k = 1;
%!
%! for i = 0:3
%! switch (i)
%!   case a
%!     x(k) = a;
%!   case b
%!     x(k) = b;
%!   otherwise
%!     x(k) = c;
%!   endswitch
%!   k++;
%! endfor
%!
%! assert (all (x == [3, 1, 2, 3]));

%!test
%! a = 1;
%! b = 2;
%! c = 3;
%!
%! x = zeros (1, 4);
%!
%! k = 1;
%!
%! for i = 0:3
%!   switch (i)
%!     case a
%!       x(k) = a;
%!   endswitch
%!   k++;
%! endfor
%!
%! assert (all (x == [0, 1, 0, 0]));

%!test
%! a = 1;
%!
%! switch (1)
%!   otherwise
%!     a = 2;
%! endswitch
%!
%! assert (a == 2);


%!error <syntax error> eval ("switch endswitch")

%!error <syntax error> eval ("switch case endswitch")

%!error <syntax error> eval ("switch 1 default 1; endswitch")

%% test parsing of single-quoted character string appearing immediately
%% after a switch case
%!test
%! switch (1)
%!   case 1
%!     'foo';
%!     x = 13;
%! endswitch
%! assert (x, 13);
