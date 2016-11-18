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

%!shared x, a, b
%! x = [1,2];
%! a = 1;
%! b = 2;

%!test
%! y = [a... # comments here ok
%! b];
%! assert (y, x);

## FIXME: Next 3 tests use '\' continuation outside of a double-quoted string
##        This behavior is deprecated and will be removed at some point.
##        When it does these
%!test
%! x = [1;2];
%! y = [a... # comments here ok
%! ;\
%!
%! b];
%! assert (y, x);

%!assert (1 + ...
%! 2 - \# comments here ok
%! 3 / ... # comments here ok
%! -1,6);

%!function y = f (a,...
%!                b,  ...
%!                c,  ...   % comments ok
%!                x,  # continuation characters not required in parens
%!                y,  \# but they should work too.
%!                z)
%!
%!  y = 1;
%!endfunction
%!
%!assert (f (), 1)

# String continuation using '\'
%!assert (["abc\
%! def"], "abc def")

%!test
%!assert (1 == 1
%! && 2 == 2
%! || 3 == 5);

%!test
%! x = [1, ...
%!
%! ...
%!
%! 2];
%! y = [1;2];
%! assert (y, x);

%!test
%! x = [1 ,...
%! 2];
%! y = [1,2];
%! assert (y, x);

%!test
%! x = [ 1 , ...
%! 2];
%! y = [1,2];
%! assert  (y, x);

%!test
%! x = [ 1 , ...anything after the ... is ignored
%! 2];
%! y = [1,2];
%! assert  (y, x);
