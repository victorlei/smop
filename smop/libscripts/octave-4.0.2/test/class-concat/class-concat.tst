%!test
%! f = foo ();
%! x = [f,f];
%! assert (size (x), [1, 2])
%! assert (class (x), "foo")

%!test
%! f = foo ();
%! x = [f,f];
%! tmp = num2cell (x);
%! assert (iscell (tmp))
%! assert (size (tmp), [1, 2])
%! assert (class (tmp{1}), "foo")
%! assert (class (tmp{2}), "foo")
