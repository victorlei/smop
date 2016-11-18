## Copyright (C) 2000-2015 Paul Kienzle
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

## -*- texinfo -*-
## @deftypefn  {Function File} {} speed (@var{f}, @var{init}, @var{max_n}, @var{f2}, @var{tol})
## @deftypefnx {Function File} {[@var{order}, @var{n}, @var{T_f}, @var{T_f2}] =} speed (@dots{})
##
## Determine the execution time of an expression (@var{f}) for various input
## values (@var{n}).
##
## The @var{n} are log-spaced from 1 to @var{max_n}.  For each @var{n}, an
## initialization expression (@var{init}) is computed to create any data needed
## for the test.  If a second expression (@var{f2}) is given then the
## execution times of the two expressions are compared.  When called without
## output arguments the results are printed to stdout and displayed
## graphically.
##
## @table @code
## @item @var{f}
## The code expression to evaluate.
##
## @item @var{max_n}
## The maximum test length to run.  The default value is 100.  Alternatively,
## use @code{[min_n, max_n]} or specify the @var{n} exactly with
## @code{[n1, n2, @dots{}, nk]}.
##
## @item @var{init}
## Initialization expression for function argument values.  Use @var{k} for
## the test number and @var{n} for the size of the test.  This should compute
## values for all variables used by @var{f}.  Note that @var{init} will be
## evaluated first for @math{k = 0}, so things which are constant throughout
## the test series can be computed once.  The default value is
## @code{@var{x} = randn (@var{n}, 1)}.
##
## @item @var{f2}
## An alternative expression to evaluate, so that the speed of two
## expressions can be directly compared.  The default is @code{[]}.
##
## @item @var{tol}
## Tolerance used to compare the results of expression @var{f} and expression
## @var{f2}.  If @var{tol} is positive, the tolerance is an absolute one.
## If @var{tol} is negative, the tolerance is a relative one.  The default is
## @code{eps}.  If @var{tol} is @code{Inf}, then no comparison will be made.
##
## @item @var{order}
## The time complexity of the expression @math{O(a*n^p)}.  This is a
## structure with fields @code{a} and @code{p}.
##
## @item @var{n}
## The values @var{n} for which the expression was calculated @strong{AND}
## the execution time was greater than zero.
##
## @item @var{T_f}
## The nonzero execution times recorded for the expression @var{f} in seconds.
##
## @item @var{T_f2}
## The nonzero execution times recorded for the expression @var{f2} in seconds.
## If required, the mean time ratio is simply @code{mean (T_f ./ T_f2)}.
##
## @end table
##
## The slope of the execution time graph shows the approximate power of the
## asymptotic running time @math{O(n^p)}.  This power is plotted for the
## region over which it is approximated (the latter half of the graph).  The
## estimated power is not very accurate, but should be sufficient to
## determine the general order of an algorithm.  It should indicate if, for
## example, the implementation is unexpectedly @math{O(n^2)} rather than
## @math{O(n)} because it extends a vector each time through the loop rather
## than pre-allocating storage.  In the current version of Octave, the
## following is not the expected @math{O(n)}.
##
## @example
## speed ("for i = 1:n, y@{i@} = x(i); endfor", "", [1000, 10000])
## @end example
##
## @noindent
## But it is if you preallocate the cell array @code{y}:
##
## @example
## @group
## speed ("for i = 1:n, y@{i@} = x(i); endfor", ...
##        "x = rand (n, 1); y = cell (size (x));", [1000, 10000])
## @end group
## @end example
##
## An attempt is made to approximate the cost of individual operations, but
## it is wildly inaccurate.  You can improve the stability somewhat by doing
## more work for each @code{n}.  For example:
##
## @example
## speed ("airy(x)", "x = rand (n, 10)", [10000, 100000])
## @end example
##
## When comparing two different expressions (@var{f}, @var{f2}), the slope of
## the line on the speedup ratio graph should be larger than 1 if the new
## expression is faster.  Better algorithms have a shallow slope.  Generally,
## vectorizing an algorithm will not change the slope of the execution time
## graph, but will shift it relative to the original.  For example:
##
## @example
## @group
## speed ("sum (x)", "", [10000, 100000], ...
##        "v = 0; for i = 1:length (x), v += x(i); endfor")
## @end group
## @end example
##
## The following is a more complex example.  If there was an original version
## of @code{xcorr} using for loops and a second version using an FFT, then
## one could compare the run speed for various lags as follows, or for a fixed
## lag with varying vector lengths as follows:
##
## @example
## @group
## speed ("xcorr (x, n)", "x = rand (128, 1);", 100,
##        "xcorr_orig (x, n)", -100*eps)
## speed ("xcorr (x, 15)", "x = rand (20+n, 1);", 100,
##        "xcorr_orig (x, n)", -100*eps)
## @end group
## @end example
##
## Assuming one of the two versions is in xcorr_orig, this would compare their
## speed and their output values.  Note that the FFT version is not exact, so
## one must specify an acceptable tolerance on the comparison @code{100*eps}.
## In this case, the comparison should be computed relatively, as
## @code{abs ((@var{x} - @var{y}) ./ @var{y})} rather than absolutely as
## @code{abs (@var{x} - @var{y})}.
##
## Type @kbd{example ("speed")} to see some real examples or
## @kbd{demo ("speed")} to run them.
## @end deftypefn

## FIXME: consider two dimensional speedup surfaces for functions like kron.
function [__order, __test_n, __tnew, __torig] = speed (__f1, __init, __max_n = 100, __f2 = "", __tol = eps)

  if (nargin < 1 || nargin > 6)
    print_usage ();
  endif

  if (nargin < 2 || isempty (__init))
    __init = "x = randn (n, 1)";
  endif

  if (isempty (__max_n))
    __max_n = 100;
  endif

  __numtests = 15;

  ## Let user specify range of n.
  if (isscalar (__max_n))
    __min_n = 1;
    assert (__max_n > __min_n);
    __test_n = logspace (0, log10 (__max_n), __numtests);
  elseif (length (__max_n) == 2)
    [__min_n, __max_n] = deal (__max_n(1), __max_n(2));
    assert (__min_n >= 1);
    assert (__max_n > __min_n);
    __test_n = logspace (log10 (__min_n), log10 (__max_n), __numtests);
  else
    assert (all (__max_n > 0));
    __test_n = __max_n;
  endif
  ## Force n to be an integer.
  __test_n = unique (round (__test_n));
  assert (__test_n >= 1);

  __torig = __tnew = zeros (size (__test_n));

  ## Print and plot the data if no output is requested.
  do_display = (nargout == 0);

  if (do_display)
    disp (["testing " __f1 "\ninit: " __init]);
  endif

  ## Add semicolon closure to all code fragments in case user has not done so.
  __init(end+1) = ";";
  __f1(end+1) = ";";
  if (! isempty (__f2))
    __f2(end+1) = ";";
  endif

  ## Make sure the functions are freshly loaded by evaluating them at
  ## test_n(1); first have to initialize the args though.
  n = 1;
  k = 0;
  eval (__init);
  eval (__f1);
  if (! isempty (__f2))
    eval (__f2);
  endif

  ## Run the tests.
  for k = 1:length (__test_n)
    n = __test_n(k);
    eval (__init);

    if (do_display)
      printf ("n%i = %i  ", k, n);
      fflush (stdout);
    endif

    eval (["__t = time();" __f1 "__v1=ans; __t = time()-__t;"]);
    if (__t < 0.25)
      eval (["__t2 = time();" __f1 "__t2 = time()-__t2;"]);
      eval (["__t3 = time();" __f1 "__t3 = time()-__t3;"]);
      __t = min ([__t, __t2, __t3]);
    endif
    __tnew(k) = __t;

    if (! isempty (__f2))
      eval (["__t = time();" __f2 "__v2=ans; __t = time()-__t;"]);
      if (__t < 0.25)
        eval (["__t2 = time();" __f2 "__t2 = time()-__t2;"]);
        eval (["__t3 = time();" __f2 "__t3 = time()-__t3;"]);
        __t = min ([__t, __t2, __t3]);
      endif
      __torig(k) = __t;
      if (! isinf(__tol))
        assert (__v1, __v2, __tol);
      endif
    endif

  endfor

  ## Drop times of zero.
  if (isempty (__f2))
    zidx = (__tnew < 100*eps);
    __test_n(zidx) = [];
    __tnew(zidx) = [];
  else
    zidx = (__tnew < 100*eps | __torig < 100*eps);
    __test_n(zidx) = [];
    __tnew(zidx) = [];
    __torig(zidx) = [];
  endif

  if (isempty (__test_n))
    error (["speed: All running times were zero.\n",
            "error: speed: Choose larger MAX_N or do more work per function evaluation"]);
  endif

  ## Approximate time complexity and return it if requested.
  tailidx = ceil (length (__test_n)/2):length (__test_n);
  p = polyfit (log (__test_n(tailidx)), log (__tnew(tailidx)), 1);
  if (nargout > 0)
    __order.p = p(1);
    __order.a = exp (p(2));
  endif

  if (do_display)
    figure;
    ## Strip semicolon added to code fragments before displaying
    __init(end) = "";
    __f1(end) = "";
    if (! isempty (__f2))
      __f2(end) = "";
    endif
  endif

  if (do_display && isempty (__f2))

    loglog (__test_n, __tnew*1000, "*-g;execution time;");
    xlabel ("test length");
    ylabel ("best execution time (ms)");
    title ({__f1, ["init: " __init]});

  elseif (do_display)

    subplot (1, 2, 1);
    semilogx (__test_n, __torig./__tnew,
             ["-*r;" strrep(__f1, ";", ".") " / " strrep(__f2, ";", ".") ";"],
              __test_n, __tnew./__torig,
             ["-*g;", strrep(__f2, ";", ".") " / " strrep(__f1, ";", ".") ";"]);
    title ("Speedup Ratio");
    xlabel ("test length");
    ylabel ("speedup ratio");

    subplot (1, 2, 2);
    loglog (__test_n, __tnew*1000,
            ["*-g;" strrep(__f1,";",".") ";"],
            __test_n, __torig*1000,
            ["*-r;" strrep(__f2,";",".") ";"]);
    title ({"Execution Times", ["init: " __init]});
    xlabel ("test length");
    ylabel ("best execution time (ms)");

    ratio = mean (__torig ./ __tnew);
    printf ("\n\nMean runtime ratio = %.3g for '%s' vs '%s'\n",
            ratio, __f2, __f1);

  endif

  if (do_display)

    ## Plot time complexity approximation (using milliseconds).
    figure;   # Open second plot window

    order = round (10*p(1))/10;
    if (order >= 0.1)
      order = sprintf ("O(n^%g)", order);
    else
      order = "O(1)";
    endif
    v = polyval (p, log (__test_n(tailidx)));

    loglog (__test_n(tailidx), exp (v) * 1000, sprintf ("b;%s;", order));
    title ({"Time Complexity", __f1});
    xlabel ("test length");

    ## Get base time to 1 digit of accuracy.
    dt = exp (p(2));
    dt = floor (dt/10^floor (log10 (dt)))*10^floor (log10 (dt));
    if (log10 (dt) >= -0.5)
      time = sprintf ("%g s", dt);
    elseif (log10 (dt) >= -3.5)
      time = sprintf ("%g ms", dt*1e3);
    elseif (log10 (dt) >= -6.5)
      time = sprintf ("%g us", dt*1e6);
    else
      time = sprintf ("%g ns", dt*1e9);
    endif

    ## Display nicely formatted complexity.
    printf ("\nFor %s:\n", __f1);
    printf ("  asymptotic power: %s\n", order);
    printf ("  approximate time per operation: %s\n", time);

  endif

endfunction


## FIXME: Demos with declared functions do not work.  See bug #31815.
##        A workaround has been hacked by not declaring the functions
##        but using eval to create them in the proper context.
##        Unfortunately, we can't remove them from the user's workspace
##        because of another bug (#34497).
%!demo
%! fstr_build_orig = cstrcat (
%!   "function x = build_orig (n)\n",
%!   "  ## extend the target vector on the fly\n",
%!   "  for i=0:n-1, x([1:100]+i*100) = 1:100; endfor\n",
%!   "endfunction");
%! fstr_build = cstrcat (
%!   "function x = build (n)\n",
%!   "  ## preallocate the target vector\n",
%!   "  x = zeros (1, n*100);\n",
%!   "  for i=0:n-1, x([1:100]+i*100) = 1:100; endfor\n",
%!   "endfunction");
%!
%! disp ("-----------------------");
%! disp (fstr_build_orig);
%! disp ("-----------------------");
%! disp (fstr_build);
%! disp ("-----------------------");
%!
%! ## Eval functions strings to create them in the current context
%! eval (fstr_build_orig);
%! eval (fstr_build);
%!
%! disp ("Preallocated vector test.\nThis takes a little while...");
%! speed ("build (n)", "", 1000, "build_orig (n)");
%! clear -f build build_orig
%! disp ("-----------------------");
%! disp ("Note how much faster it is to pre-allocate a vector.");
%! disp ("Notice the peak speedup ratio.");

%!demo
%! fstr_build_orig = cstrcat (
%!   "function x = build_orig (n)\n",
%!   "  for i=0:n-1, x([1:100]+i*100) = 1:100; endfor\n",
%!   "endfunction");
%! fstr_build = cstrcat (
%!   "function x = build (n)\n",
%!   "  idx = [1:100]';\n",
%!   "  x = idx(:,ones(1,n));\n",
%!   "  x = reshape (x, 1, n*100);\n",
%!   "endfunction");
%!
%! disp ("-----------------------");
%! disp (fstr_build_orig);
%! disp ("-----------------------");
%! disp (fstr_build);
%! disp ("-----------------------");
%!
%! ## Eval functions strings to create them in the current context
%! eval (fstr_build_orig);
%! eval (fstr_build);
%!
%! disp ("Vectorized test.\nThis takes a little while...");
%! speed ("build (n)", "", 1000, "build_orig (n)");
%! clear -f build build_orig
%! disp ("-----------------------");
%! disp ("This time, the for loop is done away with entirely.");
%! disp ("Notice how much bigger the speedup is than in example 1.");

## FIXME: Tests are known to fail on operating systems with low resolution
##        timers such as MinGW.  Therefore, tests are made xtests so that false
##        failures are not reported.  However, it might be better to either
##        force the tests to do more work, or use %!testif to check the OS.
%!xtest
%! [order, n, T_f1, T_f2] = speed ("airy (x)", "x = rand (n, 10)", [100, 1000]);
%! assert (isstruct (order));
%! assert (size (order), [1, 1]);
%! assert (fieldnames (order), {"p"; "a"});
%! assert (isnumeric (n));
%! assert (length (n) > 10);
%! assert (isnumeric (T_f1));
%! assert (size (T_f1), size (n));
%! assert (isnumeric (T_f2));
%! assert (length (T_f2) > 10);

%!xtest
%! [order, n, T_f1, T_f2] = speed ("sum (x)", "", [100, 1000], "v = 0; for i = 1:length (x), v += x(i); endfor");
%! assert (isstruct (order));
%! assert (size (order), [1, 1]);
%! assert (fieldnames (order), {"p"; "a"});
%! assert (isnumeric (n));
%! assert (length (n) > 10);
%! assert (isnumeric (T_f1));
%! assert (size (T_f1), size (n));
%! assert (isnumeric (T_f2));
%! assert (length (T_f2) > 10);

## Test input validation
%!error speed ()
%!error speed (1, 2, 3, 4, 5, 6, 7)

