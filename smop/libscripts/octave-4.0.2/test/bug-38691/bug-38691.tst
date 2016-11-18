%!test
%! unwind_protect
%!   addpath dir1;
%!   [d1_r, d1_f1, d1_f2, d1_f3] = func1 (0);
%!   addpath dir2;
%!   [d2_r, d2_f1, d2_f2, d2_f3] = func1 (0);
%!   assert (d1_r, 0);
%!   assert (d2_r, 1);
%!   assert (d1_f1, "dir1/func1");
%!   assert (d1_f2, "dir1/func2");
%!   assert (d1_f3, "dir1/func3");
%!   assert (d2_f1, "dir2/func1");
%!   assert (d2_f2, "dir2/func2");
%!   assert (d2_f3, "dir2/func3");
%! unwind_protect_cleanup
%!   rmpath dir1;
%!   rmpath dir2;
%! end_unwind_protect
