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
%! m = [3 2];
%! assert (all (m == (3:-1:2)));

%!test
%! m = [3,
%! 2];
%! assert (all (m == (3:-1:2)'));

%!test
%! a = 2;
%! assert ([a - 1], 1);

%!test
%! m = [3,2];
%! fail ("[m ']");

%!assert (all ([3 2] == (3:-1:2)));

%!assert (all ([3, 2] == (3:-1:2)));

%!test
%! m = [3,2];
%! assert (all ([m (1)] == (3:-1:1)));

%!test
%! m = [3,2];
%! assert ([m(1)],3);

%!test
%! a = 2;
%! assert ([a- 1], 1);

%!test
%! a = 1;
%! assert (all ([a -1] == (1:-2:-1)));

%!test
%! wsn = warning ("query", "Octave:str-to-num");
%! warning ("off", "Octave:str-to-num");
%! assert ("d" + 0, 100);
%! warning (wsn.state, "Octave:str-to-num");

%!test
%! wsn = warning ("query", "Octave:str-to-num");
%! warning ("on", "Octave:str-to-num");
%! fail ("'d' + 0", "warning");
%! warning (wsn.state, "Octave:str-to-num");

%!test
%! wir = warning ("query", "Octave:imag-to-real");
%! warning ("off", "Octave:imag-to-real");
%! assert (eye (1+i), 1);
%! warning (wir.state, "Octave:imag-to-real");

%!test
%! wir = warning ("query", "Octave:imag-to-real");
%! warning ("on", "Octave:imag-to-real");
%! fail ("eye (1+i)", "warning");
%! warning (wir.state, "Octave:imag-to-real");

%!test
%! wrre = warning ("query", "Octave:resize-on-range-error");
%! warning ("off", "Octave:resize-on-range-error");
%! clear a;
%! a(2) = 1; a(3) = 2;
%! assert (all (a == [0,1,2]));
%! warning (wrre.state, "Octave:resize-on-range-error");

%!test
%! clear a;
%! a(1) = 1; a(2) = 2;
%! assert (all (a == [1,2]));

%!test
%! ped = print_empty_dimensions ();
%! print_empty_dimensions (0);
%! a = cell (1, 1);
%! b = type ("-q", "a");
%! assert (!isempty (findstr (b{1}, "[]")));
%! assert (isempty (findstr (b{1} ,"[](0x0)")));
%! print_empty_dimensions (ped);

%!test
%! ped = print_empty_dimensions ();
%! print_empty_dimensions (1);
%! a = cell (1, 1);
%! b = type ("-q", "a");
%! assert (!isempty (findstr (b{1}, "[](0x0)")));
%! print_empty_dimensions (ped);

%!assert (all (size (inv ([])) == [0, 0]));

%!assert (all (svd ([]) == zeros (0, 1)));

%!test
%! sp = save_precision ();
%! save_precision (1);
%! x = pi;
%! nm = tempname ();
%! save ("-text", nm, "x");
%! clear x;
%! load (nm);
%! unlink (nm);
%! assert (x,3);
%! save_precision (sp);

%!test
%! sp = save_precision ();
%! save_precision (5);
%! x = pi;
%! nm = tempname ();
%! save ("-text", nm, "x");
%! clear x;
%! load (nm);
%! unlink (nm);
%! assert (x, 3.1416);
%! save_precision (sp);

%% FIXME: How to capture standard output for comparison?
%!function f ()
%! 1
%!endfunction
%!#test
%! sf = silent_functions ();
%! silent_functions (0);
%! f
%! assert (??);
%! silent_functions (sf);

%% FIXME: Same problem as above!!!
%!function f ()
%! 1
%!endfunction
%!#test
%! sf = silent_functions ();
%! silent_functions (1);
%! f
%! assert (??);
%! silent_functions (sf);

%!test
%! wndz = warning ("query", "Octave:neg-dim-as-zero");
%! warning ("on", "Octave:neg-dim-as-zero");
%! fail ("eye (-1) == []", "warning", "converting negative dimension");
%! warning (wndz.state, "Octave:neg-dim-as-zero");

%!test
%! wndz = warning ("query", "Octave:neg-dim-as-zero");
%! warning ("off", "Octave:neg-dim-as-zero");
%! assert (all (size (eye (-1)) == [0, 0]));
%! warning (wndz.state, "Octave:neg-dim-as-zero");

%!test
%! watv = warning ("query", "Octave:assign-as-truth-value");
%! warning ("on", "Octave:assign-as-truth-value");
%! fail ("if (x = 1) 1; endif", "warning", "assignment used as truth value");
%! warning (watv.state, "Octave:assign-as-truth-value");

%!test
%! wdbz = warning ("query", "Octave:divide-by-zero");
%! warning ("off", "Octave:divide-by-zero");
%! assert (isinf (1/0));
%! warning (wdbz.state, "Octave:divide-by-zero");

%!test
%! wdbz = warning ("query", "Octave:divide-by-zero");
%! warning ("on", "Octave:divide-by-zero");
%! a = 1;
%! b = 0;
%! fail ("isinf (a/b);", "warning", "division by zero");
%! warning (wdbz.state, "Octave:divide-by-zero");

