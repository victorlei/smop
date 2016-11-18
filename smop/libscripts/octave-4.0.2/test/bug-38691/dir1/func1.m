function [r, f1, f2, f3] = func1 (x)
  f1 = "dir1/func1";
  [r, f2, f3] = feval ("func2", x);
endfunction

function [r, f2, f3] = func2 (x)
  f2 = "dir1/func2";
  [r, f3] = feval ("func3", x);
endfunction

function [r, f3] = func3 (x)
  f3 = "dir1/func3";
  r = x;
endfunction
