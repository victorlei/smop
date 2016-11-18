%!test
%! global gfun
%! gfun = @fB;
%! y = fA (e);
%! assert (y, e);

%!test
%! global gfun
%! gfun = @fC;
%! y = fA (e);
%! assert (y, e);
