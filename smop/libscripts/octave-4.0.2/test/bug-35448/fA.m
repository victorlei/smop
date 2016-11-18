# fA.m
function y = fA (x, f)
  global gfun
  if nargin < 2
    y = fA (x, gfun);
  else
    w = feval (f, x);
    y = feval (@fB, w);
  endif
endfunction
