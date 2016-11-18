function [r, f1, f2, f3] = func1 (x)
  f1 = "dir2/func1";
  [r, f2, f3] = feval ("func2", x);
endfunction
