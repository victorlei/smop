%!test
%! % Work around MATLAB bug where f(x)(y) is invalid syntax
%! % (This bug does not apply to Octave)
%!
%! C = @(fcn,x) fcn(x);
%! C2 = @(fcn,x,y) fcn(x,y);
%!
%! % Church Booleans
%! T = @(t,f) t;
%! F = @(t,f) f;
%!
%! % Church Numerals
%! Zero  = @(fcn,x) x;
%! One   = @(fcn,x) fcn(x);
%! Two   = @(fcn,x) fcn(fcn(x));
%! Three = @(fcn,x) fcn(fcn(fcn(x)));
%! Four  = @(fcn,x) fcn(fcn(fcn(fcn(x))));
%!
%! % Arithmetic Operations
%! Inc = @(a) @(f,x) f(a(f,x)); % Increment
%! Add = @(a,b) @(f,x) a(f,b(f,x));
%! Mult = @(a,b) @(f,x) a(@(x) b(f,x),x);
%! Dec = @(a) @(f,x) C(a(@(g) @(h) h(g(f)), @(u) x), @(u) u); % Decrement
%! Sub = @(a,b) b(Dec, a);
%!
%! % Renderer - Convert church numeral to "real" number
%! Render = @(n) n(@(n) n+1,0);
%!
%! % Predicates
%! Iszero = @(n) n(@(x) F, T);
%!
%! % Y combinator implements recursion
%! Ycomb = @(f) C(@(g) f(@(x) C(g(g), x)), ...
%!                @(g) f(@(x) C(g(g), x)));
%!
%! Factorial = Ycomb(@(f) @(n) C(C2(Iszero(n), ...
%!                   @(d) One, @(d) Mult(n, f(Dec(n)))),0));
%!
%! assert (Render (Factorial (Two)), 2)
%! assert (Render (Factorial (Three)), 6)
%! assert (Render (Factorial (Four)), 24)
