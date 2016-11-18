%!function r = f1 ()
%!  ls = svd (1);
%!  r = eval ("ls -1;");
%!endfunction
%!function r = f2 ()
%!  [u,ls,v] = svd (1);
%!  r = eval ("ls -1;");
%!endfunction
%!function r = f3 (ls)
%!  r = eval ("ls -1;");
%!endfunction

%!test
%! ## Windows systems can't run "ls -1"
%! if (! ispc ())
%!   assert (f1 (), 0);
%!   assert (f2 (), 0);
%!   assert (ischar (f3 ()), true);
%!   assert (f3 (1), 0);
%! endif
