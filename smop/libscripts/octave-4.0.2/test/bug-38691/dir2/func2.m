function [r, f2, f3] = func2 (x)
  f2 = "dir2/func2";
  [r, f3] = feval ("func3", x);
endfunction
