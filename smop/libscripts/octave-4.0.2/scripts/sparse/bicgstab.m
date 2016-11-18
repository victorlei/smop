## Copyright (C) 2008-2015 Radek Salac
## Copyright (C) 2012 Carlo de Falco
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
## @deftypefn  {Function File} {@var{x} =} bicgstab (@var{A}, @var{b}, @var{rtol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0})
## @deftypefnx {Function File} {@var{x} =} bicgstab (@var{A}, @var{b}, @var{rtol}, @var{maxit}, @var{P})
## @deftypefnx {Function File} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} bicgstab (@var{A}, @var{b}, @dots{})
## Solve @code{A x = b} using the stabilizied Bi-conjugate gradient iterative
## method.
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
## function @code{f} such that @code{f(x) = A*x}.
##
## The preconditioner @var{P} is given as @code{P = M1 * M2}.  Both @var{M1}
## and @var{M2} can be passed as a matrix or as a function handle or inline
## function @code{g} such that @code{g(x) = M1 \ x} or @code{g(x) = M2 \ x}.
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
## @item @var{resvec} is a vector containing the relative residual at each
## iteration.
## @end itemize
##
## @seealso{bicg, cgs, gmres, pcg, qmr}
##
## @end deftypefn

function [x, flag, relres, iter, resvec] = bicgstab (A, b, tol, maxit,
                                                     M1, M2, x0)

  if (nargin >= 2 && nargin <= 7 && isvector (full (b)))

    if (ischar (A))
      A = str2func (A);
    elseif (isnumeric(A) && issquare (A))
      Ax  = @(x) A  * x;
    elseif (isa (A, "function_handle"))
      Ax  = @(x) feval (A, x);
    else
      error ("bicgstab: A must be a function or square matrix");
    endif

    if (nargin < 3 || isempty (tol))
      tol = 1e-6;
    endif

    if (nargin < 4 || isempty (maxit))
      maxit = min (rows (b), 20);
    endif

    if (nargin < 5 || isempty (M1))
      M1m1x = @(x) x;
    elseif (ischar (M1))
      M1m1x = str2func (M1);
    elseif (isnumeric(M1) && ismatrix (M1))
      M1m1x = @(x) M1  \ x;
    elseif (isa (M1, "function_handle"))
      M1m1x = @(x) feval (M1, x);
    else
      error ("bicgstab: preconditioner M1 must be a function or matrix");
    endif

    if (nargin < 6 || isempty (M2))
      M2m1x = @(x) x;
    elseif (ischar (M2))
      M2m1x = str2func (M2);
    elseif (isnumeric(M2) && ismatrix (M2))
      M2m1x = @(x) M2  \ x;
    elseif (isa (M2, "function_handle"))
      M2m1x = @(x) feval (M2, x);
    else
      error ("bicgstab: preconditioner M2 must be a function or matrix");
    endif

    precon = @(x) M2m1x (M1m1x (x));

    if (nargin < 7 || isempty (x0))
      x0 = zeros (size (b));
    endif

    ## specifies initial estimate x0
    if (nargin < 7)
      x = zeros (rows (b), 1);
    else
      x = x0;
    endif

    norm_b = norm (b);

    res = b - Ax (x);
    rr = res;

    ## Vector of the residual norms for each iteration.
    resvec = norm (res) / norm_b;

    ## Default behaviour we don't reach tolerance tol within maxit iterations.
    flag = 1;

    for iter = 1:maxit
      rho_1 = rr' * res;

      if (iter == 1)
        p = res;
      else
        beta = (rho_1 / rho_2) * (alpha / omega);
        p = res + beta * (p - omega * v);
      endif

      phat = precon (p);

      v = Ax (phat);
      alpha = rho_1 / (rr' * v);
      s = res - alpha * v;

      shat = precon (s);

      t = Ax (shat);
      omega = (s' * t) / (t' * t);
      x = x + alpha * phat + omega * shat;
      res = s - omega * t;
      rho_2 = rho_1;

      relres = norm (res) / norm_b;
      resvec = [resvec; relres];

      if (relres <= tol)
        ## We reach tolerance tol within maxit iterations.
        flag = 0;
        break;
      elseif (resvec(end) == resvec(end - 1))
        ## The method stagnates.
        flag = 3;
        break;
      endif
    endfor

    if (nargout < 2)
      if (flag == 0)
        printf ("bicgstab converged at iteration %i ", iter);
        printf ("to a solution with relative residual %e\n", relres);
      elseif (flag == 3)
        printf ("bicgstab stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the method stagnated.\n");
        printf ("The iterate returned (number %i) ", iter);
        printf ("has relative residual %e\n", relres);
      else
        printf ("bicgstab stopped at iteration %i ", iter);
        printf ("without converging to the desired toleranc %e\n", tol);
        printf ("because the maximum number of iterations was reached.\n");
        printf ("The iterate returned (number %i) ", iter);
        printf ("has relative residual %e\n", relres);
      endif
    endif

  else
    print_usage ();
  endif

endfunction


%!demo
%! % Solve system of A*x=b
%! A = [5 -1 3;-1 2 -2;3 -2 3];
%! b = [7;-1;4];
%! [x, flag, relres, iter, resvec] = bicgstab (A, b)

%!shared A, b, n, M1, M2
%!
%!test
%! n = 100;
%! A = spdiags ([-2*ones(n,1) 4*ones(n,1) -ones(n,1)], -1:1, n, n);
%! b = sum (A, 2);
%! tol = 1e-8;
%! maxit = 15;
%! M1 = spdiags ([ones(n,1)/(-2) ones(n,1)],-1:0, n, n);
%! M2 = spdiags ([4*ones(n,1) -ones(n,1)], 0:1, n, n);
%! [x, flag, relres, iter, resvec] = bicgstab (A, b, tol, maxit, M1, M2);
%! assert (x, ones (size (b)), 1e-7);
%!
%!test
%!function y = afun (x, a)
%!  y = a * x;
%!endfunction
%!
%! tol = 1e-8;
%! maxit = 15;
%!
%! [x, flag, relres, iter, resvec] = bicgstab (@(x) afun (x, A), b,
%!                                             tol, maxit, M1, M2);
%! assert (x, ones (size (b)), 1e-7);

%!test
%! n = 100;
%! tol = 1e-8;
%! a = sprand (n, n, .1);
%! A = a'*a + 100 * eye (n);
%! b = sum (A, 2);
%! [x, flag, relres, iter, resvec] = bicgstab (A, b, tol, [], diag (diag (A)));
%! assert (x, ones (size (b)), 1e-7);

%!test
%! A = [1 + 1i, 1 + 1i; 2 - 1i, 2 + 1i];
%! b = A * [1; 1];
%! [x, flag, relres, iter, resvec] = bicgstab (A, b);
%! assert (x, [1; 1], 1e-6);
