## Copyright (C) 2003,2012 Andy Adler
## Copyright (C) 2002, 2013 N.J.Higham
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
## @deftypefn  {Function File} {@var{x} =} fminsearch (@var{fun}, @var{x0})
## @deftypefnx {Function File} {@var{x} =} fminsearch (@var{fun}, @var{x0}, @var{options})
## @deftypefnx {Function File} {[@var{x}, @var{fval}] =} fminsearch (@dots{})
##
## Find a value of @var{x} which minimizes the function @var{fun}.
##
## The search begins at the point @var{x0} and iterates using the
## @nospell{Nelder & Mead} Simplex algorithm (a derivative-free method).  This
## algorithm is better-suited to functions which have discontinuities or for
## which a gradient-based search such as @code{fminunc} fails.
##
## Options for the search are provided in the parameter @var{options} using the
## function @code{optimset}.  Currently, @code{fminsearch} accepts the options:
## @qcode{"TolX"}, @qcode{"MaxFunEvals"}, @qcode{"MaxIter"}, @qcode{"Display"}.
## For a description of these options, see @code{optimset}.
##
## On exit, the function returns @var{x}, the minimum point, and @var{fval},
## the function value thereof.
##
## Example usages:
##
## @example
## @group
## fminsearch (@@(x) (x(1)-5).^2+(x(2)-8).^4, [0;0])
##
## fminsearch (inline ("(x(1)-5).^2+(x(2)-8).^4", "x"), [0;0])
## @end group
## @end example
## @seealso{fminbnd, fminunc, optimset}
## @end deftypefn

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("fminsearch");

## FIXME: Add support for "exitflag" output variable
## FIXME: Add support for "output" output variable
## FIXME: For Display option, add 'final' and 'notify' options.  Not too hard.
## FIXME: Add support for OutputFcn.  See fminunc for a template
## FIXME: Add support for exiting based on TolFun.  See fminunc for an idea.

function [x, fval] = fminsearch (fun, x0, options = struct ())

  ## Get default options if requested.
  if (nargin == 1 && ischar (fun) && strcmp (fun, "defaults"))
    x = optimset ("Display", "notify", "FunValCheck", "off",
                  "MaxFunEvals", 400, "MaxIter", 400,
                  "OutputFcn", [],
                  "TolFun", 1e-7, "TolX", 1e-4);
    return;
  endif

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  x = nmsmax (fun, x0, options);

  if (isargout (2))
    fval = feval (fun, x);
  endif

endfunction

##NMSMAX  Nelder-Mead simplex method for direct search optimization.
##        [x, fmax, nf] = NMSMAX(FUN, x0, STOPIT, SAVIT) attempts to
##        maximize the function FUN, using the starting vector x0.
##        The Nelder-Mead direct search method is used.
##        Output arguments:
##               x    = vector yielding largest function value found,
##               fmax = function value at x,
##               nf   = number of function evaluations.
##        The iteration is terminated when either
##               - the relative size of the simplex is <= STOPIT(1)
##                 (default 1e-3),
##               - STOPIT(2) function evaluations have been performed
##                 (default inf, i.e., no limit), or
##               - a function value equals or exceeds STOPIT(3)
##                 (default inf, i.e., no test on function values).
##        The form of the initial simplex is determined by STOPIT(4):
##           STOPIT(4) = 0: regular simplex (sides of equal length, the default)
##           STOPIT(4) = 1: right-angled simplex.
##        Progress of the iteration is not shown if STOPIT(5) = 0 (default 1).
##           STOPIT(6) indicates the direction (ie. minimization or
##                   maximization.) Default is 1, maximization.
##                   set STOPIT(6)=-1 for minimization
##        If a non-empty fourth parameter string SAVIT is present, then
##        'SAVE SAVIT x fmax nf' is executed after each inner iteration.
##        NB: x0 can be a matrix.  In the output argument, in SAVIT saves,
##            and in function calls, x has the same shape as x0.
##        NMSMAX(fun, x0, STOPIT, SAVIT, P1, P2,...) allows additional
##        arguments to be passed to fun, via feval(fun,x,P1,P2,...).
## References:
## N. J. Higham, Optimization by direct search in matrix computations,
##    SIAM J. Matrix Anal. Appl, 14(2): 317-333, 1993.
## C. T. Kelley, Iterative Methods for Optimization, Society for Industrial
##    and Applied Mathematics, Philadelphia, PA, 1999.

## From Matrix Toolbox
## Copyright (C) 2002, 2013 N.J.Higham
## www.maths.man.ac.uk/~higham/mctoolbox
##
## Modifications for Octave by A.Adler 2003

function [stopit, savit, dirn, trace, tol, maxiter] = parse_options (options, x );

  ## Tolerance for cgce test based on relative size of simplex.
  stopit(1) = tol = optimget (options, "TolX", 1e-4);

  ## Max no. of f-evaluations.
  stopit(2) = optimget (options, "MaxFunEvals", length (x) * 200);

  ## Max no. of iterations
  maxiter = optimget (options, "MaxIter", length (x) * 200);

  ## Default target for f-values.
  stopit(3) = Inf;  # FIXME: expose this parameter to the outside

  ## Default initial simplex.
  stopit(4) = 0;    # FIXME: expose this parameter to the outside

  ## Default: show progress.
  display = optimget (options, "Display", "notify");
  if (strcmp (display, "iter"))
    stopit(5) = 1;
  else
    stopit(5) = 0;
  endif
  trace = stopit(5);

  ## Use function to minimize, not maximize
  stopit(6) = dirn = -1;

  ## File name for snapshots.
  savit = [];  # FIXME: expose this parameter to the outside

endfunction

function [x, fmax, nf] = nmsmax (fun, x, options, savit, varargin)

  [stopit, savit, dirn, trace, tol, maxiter] = parse_options (options, x);

  if (strcmpi (optimget (options, "FunValCheck", "off"), "on"))
    ## Replace fcn with a guarded version.
    fun = @(x) guarded_eval (fun, x);
  endif

  x0 = x(:);  # Work with column vector internally.
  n = length (x0);

  V = [zeros(n,1) eye(n)];
  f = zeros (n+1,1);
  V(:,1) = x0;
  f(1) = dirn * feval (fun,x,varargin{:});
  fmax_old = f(1);

  if (trace)
    fprintf ("f(x0) = %9.4e\n", f(1));
  endif

  k = 0; m = 0;

  ## Set up initial simplex.
  scale = max (norm (x0,Inf), 1);
  if (stopit(4) == 0)
    ## Regular simplex - all edges have same length.
    ## Generated from construction given in reference [18, pp. 80-81] of [1].
    alpha = scale / (n*sqrt (2)) * [sqrt(n+1)-1+n, sqrt(n+1)-1];
    V(:,2:n+1) = (x0 + alpha(2)*ones (n,1)) * ones (1,n);
    for j = 2:n+1
      V(j-1,j) = x0(j-1) + alpha(1);
      x(:) = V(:,j);
      f(j) = dirn * feval (fun,x,varargin{:});
    endfor
  else
    ## Right-angled simplex based on co-ordinate axes.
    alpha = scale * ones(n+1,1);
    for j=2:n+1
      V(:,j) = x0 + alpha(j)*V(:,j);
      x(:) = V(:,j);
      f(j) = dirn * feval (fun,x,varargin{:});
    endfor
  endif
  nf = n+1;
  how = "initial  ";

  [~,j] = sort (f);
  j = j(n+1:-1:1);
  f = f(j);
  V = V(:,j);

  alpha = 1;  beta = 1/2;  gamma = 2;

  while (1)   # Outer (and only) loop.
    k++;

    if (k > maxiter)
      msg = "Exceeded maximum iterations...quitting\n";
      break;
    endif

    fmax = f(1);
    if (fmax > fmax_old)
      if (! isempty (savit))
        x(:) = V(:,1);
        eval (["save " savit " x fmax nf"]);
      endif
    endif
    if (trace)
      fprintf ("Iter. %2.0f,", k);
      fprintf (["  how = " how "  "]);
      fprintf ("nf = %3.0f,  f = %9.4e  (%2.1f%%)\n", nf, fmax, ...
               100*(fmax-fmax_old)/(abs(fmax_old)+eps));
    endif
    fmax_old = fmax;

    ## Three stopping tests from MDSMAX.M

    ## Stopping Test 1 - f reached target value?
    if (fmax >= stopit(3))
      msg = "Exceeded target...quitting\n";
      break;
    endif

    ## Stopping Test 2 - too many f-evals?
    if (nf >= stopit(2))
      msg = "Max no. of function evaluations exceeded...quitting\n";
      break;
    endif

    ## Stopping Test 3 - converged?   This is test (4.3) in [1].
    v1 = V(:,1);
    size_simplex = norm (V(:,2:n+1)-v1(:,ones (1,n)),1) / max (1, norm (v1,1));
    if (size_simplex <= tol)
      msg = sprintf ("Simplex size %9.4e <= %9.4e...quitting\n", ...
                      size_simplex, tol);
      break;
    endif

    ##  One step of the Nelder-Mead simplex algorithm
    ##  NJH: Altered function calls and changed CNT to NF.
    ##       Changed each 'fr < f(1)' type test to '>' for maximization
    ##       and re-ordered function values after sort.

    vbar = (sum (V(:,1:n)')/n)';  # Mean value
    vr = (1 + alpha)*vbar - alpha*V(:,n+1);
    x(:) = vr;
    fr = dirn * feval (fun,x,varargin{:});
    nf = nf + 1;
    vk = vr;  fk = fr; how = "reflect, ";
    if (fr > f(n))
      if (fr > f(1))
        ve = gamma*vr + (1-gamma)*vbar;
        x(:) = ve;
        fe = dirn * feval (fun,x,varargin{:});
        nf = nf + 1;
        if (fe > f(1))
          vk = ve;
          fk = fe;
          how = "expand,  ";
        endif
      endif
    else
      vt = V(:,n+1);
      ft = f(n+1);
      if (fr > ft)
        vt = vr;
        ft = fr;
      endif
      vc = beta*vt + (1-beta)*vbar;
      x(:) = vc;
      fc = dirn * feval (fun,x,varargin{:});
      nf = nf + 1;
      if (fc > f(n))
        vk = vc; fk = fc;
        how = "contract,";
      else
        for j = 2:n
          V(:,j) = (V(:,1) + V(:,j))/2;
          x(:) = V(:,j);
          f(j) = dirn * feval (fun,x,varargin{:});
        endfor
        nf = nf + n-1;
        vk = (V(:,1) + V(:,n+1))/2;
        x(:) = vk;
        fk = dirn * feval (fun,x,varargin{:});
        nf = nf + 1;
        how = "shrink,  ";
      endif
    endif
    V(:,n+1) = vk;
    f(n+1) = fk;
    [~,j] = sort(f);
    j = j(n+1:-1:1);
    f = f(j);
    V = V(:,j);

  endwhile   # End of outer (and only) loop.

  ## Finished.
  if (trace)
    fprintf (msg);
  endif
  x(:) = V(:,1);

endfunction

## A helper function that evaluates a function and checks for bad results.
function y = guarded_eval (fun, x)

  y = fun (x);

  if (! (isreal (f)))
    error ("fminsearch:notreal", "fminsearch: non-real value encountered");
  elseif (any (isnan (f(:))))
    error ("fminsearch:isnan", "fminsearch: NaN value encountered");
  elseif (any (isinf (f(:))))
    error ("fminsearch:isinf", "fminsearch: Inf value encountered");
  endif

endfunction


%!demo
%! fcn = @(x) (x(1)-5).^2 + (x(2)-8).^4
%! x0 = [0;0];
%! [xmin, fval] = fminsearch (fcn, x0)

%!assert (fminsearch (@sin, 3, optimset ("MaxIter", 3)), 4.8750, 1e-4)
%!assert (fminsearch (@sin, 3, optimset ("MaxIter", 30)), 4.7124, 1e-4)
%!shared c
%! c = 1.5;
%!assert (fminsearch (@(x) x(1).^2+c*x(2).^2,[1;1]), [0;0], 1e-4)

