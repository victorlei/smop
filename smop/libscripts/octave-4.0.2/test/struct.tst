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
%! s.a = 1;
%! c = fieldnames (s);
%! assert (iscell (c) && strcmp (c{1}, "a"));

%!test
%! s.a.b = 1;
%! c = fieldnames (s.a);
%! assert (iscell (c) && strcmp (c{1}, "b"));

%!error <Invalid call to fieldnames> fieldnames ();

%!test
%! s.a = 1;
%! fail ("fieldnames (s, 1)", "Invalid call to fieldnames");

%!error fieldnames (1);

%!test
%! s.aaa = 1;
%! s.a = 2;
%! assert (isfield (s, "a"));

%!test
%! s.aaa = 1;
%! s.a = 2;
%! assert (!(isfield (s, "b")));

%!error <Invalid call to isfield> isfield ();

%!test
%! s.aaa = 1;
%! s.a = 2;
%! fail ("isfield (s, 'a', 3);", "Invalid call to isfield");

%!assert (isfield (1, "m") == 0);

%!test
%! s.a = 2;
%! assert (isfield (s, 2) == 0);

%!assert (!(isstruct (1)))

%!assert (!(isstruct ([1, 2])))

%!assert (!(isstruct ([])))

%!assert (!(isstruct ([1, 2; 3, 4])))

%!assert (!(isstruct ("t")))

%!assert (!(isstruct ("test")))

%!assert (!(isstruct (["test"; "ing"])))

%!assert (!(isstruct ({1})))

%!test
%! s.a = 1;
%! assert (isstruct (s));

%!test
%! s.a.b = 1;
%! assert (isstruct (s.a));

%!error <Invalid call to isstruct> isstruct ();

%!test
%! s.a = 1;
%! fail ("isstruct (s, 1)", "Invalid call to isstruct");

## increment element of matrix stored in struct array field
%!test
%! a = struct ("c", {[1, 2, 3], [4, 5, 6], [7, 8, 9]});
%! a(2).c(3)++;
%! assert (a(2).c, [4, 5, 7]);

## create struct array by assignment to cs-list
%!test
%! [a(1:2).x] = deal (1, 3);
%! assert (a, struct ("x", {1, 3}));
%! assert ({a(1:2).x}, {1, 3});

## assign to subrange of struct array field
%!test
%! b = struct ("name", {"a", "b", "c"; "d", "e", "f"}, "value", 100);
%! [b(1:2, [1,3]).name] = deal ("aaa", "ddd", "ccc", "fff");
%! assert ({b.name}, {"aaa", "ddd", "b", "e", "ccc", "fff"});

## index into nested struct arrays
%!test
%! a = struct ("name", {"a", "b", "c"; "d", "e", "f"}, "value", 0);
%! a(2).value = a;
%! assert (a(2).value(2,3).name, "f");

## assign to subrange of field in nested struct array
%!test
%! b = struct ("name", {"a", "b", "c"; "d", "e", "f"}, "value", 0);
%! b(3, 1).value = b;
%! [b(3, 1).value(1, [1, 3]).name] = deal ("aaa", "ccc");
%! assert (size (b), [3, 3]);
%! assert (b(3,1).value(1, 3).name, "ccc");

## test 4-dimensional struct array
%!test
%! c(4, 4, 4, 4).name  = "a";
%! c(3, 3, 3, 3).value = 1;
%! assert (c(2,2,2,2), struct ("name", [], "value", []));

## assign to subrange of field in 4D struct array
%!test
%! c(4, 4, 4, 4).name  = "a";
%! c(3, 3, 3, 3).value = 1;
%! [c([1, 3], 2, :, [3, 4]).value] = deal (1);
%! assert (length (find ([c.value] == 1)), 17);
%! assert (length (find ([c.value])), 17);

## swap elements of struct array
%!test
%! b = struct ("name", {"a", "b", "c"; "d", "e", "f"}, "value", 0);
%! [b([2, 1], [3, 1]).name] = deal (b([1, 2], [1, 2]).name);
%! assert ({b.name}, {"e", "b", "b", "e", "d", "a"});

## test internal ordering of struct array fields
%!test
%! c(4, 4, 4, 4).value = 3;
%! c(1, 2, 3, 4).value = 2;
%! c(3, 3, 3, 3).value = 1;
%! d = reshape ({c.value}, size (c));
%! assert ([d{4, 4, 4, 4}, d{1, 2, 3, 4}, d{3, 3, 3, 3}],
%!         [3, 2, 1]);

## test assignment to mixed cs-list of field element subranges
%!test
%! b = struct ("name", {"a", "b", "c"; "d", "e", "f"}, "value", 100);
%! [b(1:2, [1, 3]).name, b(2, 1:3).value] = ...
%!   deal (1, 2, 3, 4, "5", "6", "7");
%! assert ({b.name}, {1, 2, "b", "e", 3, 4});
%! assert ({b.value}, {100, "5", 100, "6", 100, "7"});

%!error <a cs-list cannot be further indexed>
%! [a(1:3).x] = deal ([1, 5], [3, 7], [8, 9]);
%! [a(2:3).x(2)] = deal (10, 11);

%!error <a cs-list cannot be further indexed>
%! [a(1:3).x] = deal ([1, 5], [3, 7], [8, 9]);
%! a(2:3).x(2);

%!error id=Octave:index-out-of-bounds
%! a(1).x.x = 1;
%! a(2).x;

%!error <invalid number of output arguments for constant expression>
%! a = struct ("value", {1, 2, 3, 4, 5});
%! [a(2:4).value] = 1;

%!error <invalid assignment to cs-list outside multiple assignment>
%! c(4, 4, 4, 4).name  = "a";
%! c(3, 3, 3, 3).value = 1;
%! c([1, 3], 2, :, [3, 4]).value = 1;

## test lazy copying in structs: nested assignment to self
%!test
%! a.a = 1;
%! a.b = a;
%! a.b.c = a;
%! assert (a.b.c.b, struct ("a", 1));

## test lazy copying in structs: indirect nested assignment to self
%!test
%! a.a = 1;
%! a.b = 2;
%! b.c = a;
%! b.d = 3;
%! c.d = b;
%! c.e = 4;
%! a.b = c;
%! a.b.e = a;
%! assert (a.b.e.b.d.c, struct ("a", 1, "b", 2));

## test lazy copying in structs: nested assignment via function
%!function aa = do_nest (a);
%!  aa   = a;
%!  aa.b = a;
%!endfunction
%!test
%! a.c = 1;
%! a = do_nest (a);
%! a = do_nest (a);
%! a = do_nest (a);
%! assert (a.b.b.b, struct ("c", 1));

## test lazy copying in structs: nested assignment via function
%!function aa = do_nest (a);
%!  aa   = a;
%!  aa.b = a;
%!  aa.b.c = aa;
%!endfunction
%!test
%! a.c = 1;
%! a = do_nest (a);
%! a = do_nest (a);
%! a = do_nest (a);
%! assert (a.b.c.b.b.c.b.b.c.b, struct ("c", 1));

## test lazy copying in structs: nested assignment on different levels.
%!test
%! a.b = 1;
%! b.c = a;
%! b.d.e = a;
%! b.f.g.h = a;
%! b.i.j.k.l = a;
%! a.m = b;
%! a.m.c.b = a;
%! assert (a.m.c.b.m.i.j.k.l, struct ("b", 1));

## test indexed assignment into empty struct array
%!test
%! s = resize (struct (), 3,2);
%! s(3).foo = 42;
%! assert (s(3), struct ("foo", 42));

%!error id=Octave:index-out-of-bounds
%! s = resize (struct (),3,2);
%! s(3).foo = 42;
%! s(7);
