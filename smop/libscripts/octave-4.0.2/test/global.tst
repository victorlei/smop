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
%! global G = 1;
%! assert (G,1);

%!function f ()
%!  global G;
%!  assert (G,1);
%!endfunction
%!test
%! global G = 1;
%! f;

%!function f ()
%!  fail ("G");
%!endfunction
%!test
%! global G = 1;
%! f ();

%!function f ()
%!  global H = 1;
%!endfunction
%!test
%! f;
%! fail ("H");

%!function f ()
%!  global H = 1;
%!endfunction
%!test
%!function g ()
%!  fail ("H");
%!test
%! g ();

%!function f ()
%!  global H = 1;
%!endfunction
%!function g ()
%!  global H;
%!  assert (H,1);
%!endfunction
%!test
%! f ();
%! g ();

%!test
%!function f ()
%!  global H = 1;
%!endfunction
%!test
%! fail ("H");

%!function f ()
%!  global H = 1;
%!endfunction
%!function g ()
%!  global H;
%!  assert (H,1);
%!endfunction
%!test
%! f;
%! clear H;
%! g;

