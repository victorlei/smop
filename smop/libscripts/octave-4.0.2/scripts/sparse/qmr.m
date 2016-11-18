## Copyright (C) 2014-2015 Nathan Podlich
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{x} =} qmr (@var{A}, @var{b}, @var{rtol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0})
## @deftypefnx {Function File} {@var{x} =} qmr (@var{A}, @var{b}, @var{rtol}, @var{maxit}, @var{P})
## @deftypefnx {Function File} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} qmr (@var{A}, @var{b}, @dots{})
## Solve @code{A x = b} using the Quasi-Minimal Residual iterative method
## (without look-ahead).
##
## @itemize @minus
## @item @var{rtol} is the relative tolerance, if not given or set to [] the
## default value 1e-6 is used.
##
## @item @var{maxit} the maximum number of outer iterations, if not given or
## set to [] the default value @code{min (20, numel (b))} is used.
##
## @item @var{x0} the initial guess, if not given or set to [] the default
## value @code{zeros (size (b))} is used.
## @end itemize
##
## @var{A} can be passed as a matrix or as a function handle or inline
## function @code{f} such that @code{f(x, "notransp") = A*x} and
## @code{f(x, "transp") = A'*x}.
##
## The preconditioner @var{P} is given as @code{P = M1 * M2}.  Both @var{M1}
## and @var{M2} can be passed as a matrix or as a function handle or inline
## function @code{g} such that @code{g(x, "notransp") = M1 \ x} or
## @code{g(x, "notransp") = M2 \ x} and @code{g(x, "transp") = M1' \ x} or
## @code{g(x, "transp") = M2' \ x}.
##
## If called with more than one output parameter
##
## @itemize @minus
## @item @var{flag} indicates the exit status:
##
## @itemize @minus
## @item 0: iteration converged to the within the chosen tolerance
##
## @item 1: the maximum number of iterations was reached before convergence
##
## @item 3: the algorithm reached stagnation
## @end itemize
##
## (the value 2 is unused but skipped for compatibility).
##
## @item @var{relres} is the final value of the relative residual.
##
## @item @var{iter} is the number of iterations performed.
##
## @item @var{resvec} is a vector containing the residual norms at each
##       iteration.
## @end itemize
##
## References:
##
## @enumerate
## @item
## @nospell{R. Freund and N. Nachtigal}, @cite{QMR: a quasi-minimal residual
## method for non-Hermitian linear systems}, @nospell{Numerische Mathematik},
## 1991, 60, pp. 315-339.
##
## @item
## @nospell{ R. Barrett, M. Berry, T. Chan, J. Demmel, J. Donato, J. Dongarra},
## @nospell{ V. Eijkhour, R. Pozo, C. Romine, and H. van der Vorst},
## @cite{Templates for the solution of linear systems: Building blocks
## for iterative methods}, SIAM, 2nd ed., 1994.
## @end enumerate
##
## @seealso{bicg, bicgstab, cgs, gmres, pcg}
## @end deftypefn

## Author: Nathan Podlich <nathan.podlich@gmail.com>

function [x, flag, relres, iter, resvec] = qmr (A, b, tol, maxit, M1, M2, x0)

  if (nargin >= 2 && isvector (full (b)))

    if (ischar (A))
      fun = str2func (A);
      Ax  = @(x) feval (fun, x, "notransp");
      Atx = @(x) feval (fun, x, "transp");
    elseif (isa (A, "function_handle"))
      Ax  = @(x) feval (A, x, "notransp");
      Atx = @(x) feval (A, x, "transp");
    elseif (isnumeric (A) && issquare (A))
      Ax  = @(x) A  * x;
      Atx = @(x) A' * x;
    else
      error ("qmr: A must be a function or square matrix");
    endif

    if (nargin < 3 || isempty (tol))
      tol = 1e-6;
    endif

    if (nargin < 4 || isempty (maxit))
      maxit = min (rows (b), 20);
    else
      maxit = fix (maxit);
    endif

    if (nargin < 5 || isempty (M1))
      M1m1x = @(x, ignore) x;
      M1tm1x = M1m1x;
    elseif (ischar (M1))
      fun = str2func (M1);
      M1m1x  = @(x) feval (fun, x, "notransp");
      M1tm1x = @(x) feval (fun, x, "transp");
    elseif (isa (M1, "function_handle"))
      M1m1x  = @(x) feval (M1, x, "notransp");
      M1tm1x = @(x) feval (M1, x, "transp");
    elseif (isnumeric (M1) && ismatrix (M1))
      M1m1x  = @(x) M1  \ x;
      M1tm1x = @(x) M1' \ x;
    else
      error ("qmr: preconditioner M1 must be a function or matrix");
    endif

    if (nargin < 6 || isempty (M2))
      M2m1x = @(x, ignore) x;
      M2tm1x = M2m1x;
    elseif (ischar (M2))
      fun = str2func (M2);
      M2m1x  = @(x) feval (fun, x, "notransp");
      M2tm1x = @(x) feval (fun, x, "transp");
    elseif (isa (M2, "function_handle"))
      M2m1x  = @(x) feval (M2, x, "notransp");
      M2tm1x = @(x) feval (M2, x, "transp");
    elseif (isnumeric (M2) && ismatrix (M2))
      M2m1x  = @(x) M2  \ x;
      M2tm1x = @(x) M2' \ x;
    else
      error ("qmr: preconditioner M2 must be a function or matrix");
    endif

    if (nargin < 7 || isempty (x0))
      x = zeros (size (b));
    else
      x = x0;
    endif

    r = b - Ax (x);

    bnorm = norm (b);
    res0 = norm (r);
    if (nargout > 4)
      resvec(1) = res0;
    endif
    vt = r;

    y = M1m1x (vt);

    rho0 = norm (y);
    wt = r;

    z = M2tm1x (wt);

    xi1 = norm (z);
    gamma0 = 1;
    eta0 = -1;
    flag = 1;
    for iter=1:1:maxit
      ## If rho0 == 0 or xi1 == 0, method fails.
      v = vt / rho0;
      y = y / rho0;
      w = wt / xi1;
      z = z / xi1;

      delta1 = z' * y;   # If delta1 == 0, method fails.

      yt = M2m1x (y);
      zt = M1tm1x (z);

      if (iter == 1)
        p = yt;
        q = zt;
      else
        p = yt - (xi1*delta1/eps0) * p;
        q = zt - (rho0*delta1/eps0) * q;
      endif
      pt = Ax (p);

      eps0 = q' * pt;          # If eps0 == 0, method fails.
      beta1 = eps0 / delta1;   # If beta1 == 0, method fails.
      vt = pt - beta1 * v;

      y = M1m1x (vt);
      rho1 = norm(y);
      wt = Atx (q) - beta1 * w;
      z = M2tm1x (wt);

      xi1 = norm(z);
      theta1 = rho1 / (gamma0 * abs(beta1));
      gamma1 = 1 / sqrt(1 + theta1^2);   # If gamma1 == 0, method fails.
      eta1 = -eta0 * rho0 * gamma1^2 / (beta1 * gamma0^2);

      if (iter == 1)
        d = eta1 * p;
        s = eta1 * pt;
      else
        d = eta1 * p + (theta0*gamma1)^2 * d;
        s = eta1 * pt + (theta0 * gamma1)^2 * s;
      endif
      x += d;
      r -= s;

      res1 = norm (r) / bnorm;
      if (nargout > 4)
        resvec(iter + 1, 1) = norm (r);
      end

      if (res1 < tol)
        ## Convergence achieved.
        flag = 0;
        break;
      elseif (res0 <= res1)
        ## Stagnation encountered.
        flag = 3;
        break;
      endif
      theta0 = theta1;
      eta0 = eta1;
      gamma0 = gamma1;
      rho0 = rho1;
    endfor

    relres = res1;
    if (flag == 1)
      if (nargout < 2)
        printf ("qmr stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the maximum number of iterations was reached. ");
        printf ("The iterate returned (number %i) has ", maxit);
        printf ("relative residual %e\n", res1);
      endif
    elseif (flag == 3)
      if (nargout < 2)
        printf ("qmr stopped at iteration %i ", iter);
        printf (" without converging to the desired tolerance %e\n", tol);
        printf ("because the method stagnated.\n");
        printf ("The iterate returned (number %i) ", iter);
        printf ("has relative residual %e\n", res1);
      endif
    elseif (nargout < 2)
      printf ("qmr converged at iteration %i ", iter);
      printf ("to a solution with relative residual %e\n", res1);
    endif
  else
    print usage();
  endif
endfunction


%!demo
%! % Solve system of A*x=b
%! A = [5 -1 3;-1 2 -2;3 -2 3];
%! b = [7;-1;4];
%! [x, flag, relres, iter, resvec] = qmr (A, b)

%!test
%! n = 100;
%! A = spdiags ([-2*ones(n,1) 4*ones(n,1) -ones(n,1)], -1:1, n, n);
%! b = sum (A, 2);
%! tol = 1e-8;
%! maxit = 15;
%! M1 = spdiags ([ones(n,1)/(-2) ones(n,1)],-1:0, n, n);
%! M2 = spdiags ([4*ones(n,1) -ones(n,1)], 0:1, n, n);
%! [x, flag, relres, iter, resvec] = qmr (A, b, tol, maxit, M1, M2);
%! assert (x, ones (size (b)), 1e-7);

%!function y = afun (x, t, a)
%!  switch (t)
%!    case "notransp"
%!      y = a * x;
%!    case "transp"
%!      y = a' * x;
%!  endswitch
%!endfunction
%!
%!test
%! n = 100;
%! A = spdiags ([-2*ones(n,1) 4*ones(n,1) -ones(n,1)], -1:1, n, n);
%! b = sum (A, 2);
%! tol = 1e-8;
%! maxit = 15;
%! M1 = spdiags ([ones(n,1)/(-2) ones(n,1)],-1:0, n, n);
%! M2 = spdiags ([4*ones(n,1) -ones(n,1)], 0:1, n, n);
%!
%! [x, flag, relres, iter, resvec] = qmr (@(x, t) afun (x, t, A),
%!                                         b, tol, maxit, M1, M2);
%! assert (x, ones (size (b)), 1e-7);

%!test
%! n = 100;
%! tol = 1e-8;
%! a = sprand (n, n, .1);
%! A = a' * a + 100 * eye (n);
%! b = sum (A, 2);
%! [x, flag, relres, iter, resvec] = qmr (A, b, tol, [], diag (diag (A)));
%! assert (x, ones (size (b)), 1e-7);

%!test
%! A = [1 + 1i, 1 + 1i; 2 - 1i, 2 + 1i];
%! b = A * [1; 1];
%! [x, flag, relres, iter, resvec] = qmr (A, b);
%! assert (x, [1; 1], 1e-6);
