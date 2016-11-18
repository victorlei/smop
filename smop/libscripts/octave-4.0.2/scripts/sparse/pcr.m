## Copyright (C) 2004-2015 Piotr Krzyzanowski
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
## @deftypefn  {Function File} {@var{x} =} pcr (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{m}, @var{x0}, @dots{})
## @deftypefnx {Function File} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} pcr (@dots{})
##
## Solve the linear system of equations @code{@var{A} * @var{x} = @var{b}} by
## means of the Preconditioned Conjugate Residuals iterative method.
##
## The input arguments are
##
## @itemize
## @item
## @var{A} can be either a square (preferably sparse) matrix or a function
## handle, inline function or string containing the name of a function which
## computes @code{@var{A} * @var{x}}.  In principle @var{A} should be
## symmetric and non-singular; if @code{pcr} finds @var{A} to be numerically
## singular, you will get a warning message and the @var{flag} output
## parameter will be set.
##
## @item
## @var{b} is the right hand side vector.
##
## @item
## @var{tol} is the required relative tolerance for the residual error,
## @code{@var{b} - @var{A} * @var{x}}.  The iteration stops if
## @code{norm (@var{b} - @var{A} * @var{x}) <=
##       @var{tol} * norm (@var{b} - @var{A} * @var{x0})}.
## If @var{tol} is empty or is omitted, the function sets
## @code{@var{tol} = 1e-6} by default.
##
## @item
## @var{maxit} is the maximum allowable number of iterations; if @code{[]} is
## supplied for @code{maxit}, or @code{pcr} has less arguments, a default
## value equal to 20 is used.
##
## @item
## @var{m} is the (left) preconditioning matrix, so that the iteration is
## (theoretically) equivalent to solving by
## @code{pcr} @code{@var{P} * @var{x} = @var{m} \ @var{b}}, with
## @code{@var{P} = @var{m} \ @var{A}}.  Note that a proper choice of the
## preconditioner may dramatically improve the overall performance of the
## method.  Instead of matrix @var{m}, the user may pass a function which
## returns the results of applying the inverse of @var{m} to a vector
## (usually this is the preferred way of using the preconditioner).  If
## @code{[]} is supplied for @var{m}, or @var{m} is omitted, no
## preconditioning is applied.
##
## @item
## @var{x0} is the initial guess.  If @var{x0} is empty or omitted, the
## function sets @var{x0} to a zero vector by default.
## @end itemize
##
## The arguments which follow @var{x0} are treated as parameters, and passed
## in a proper way to any of the functions (@var{A} or @var{m}) which are
## passed to @code{pcr}.  See the examples below for further details.
##
## The output arguments are
##
## @itemize
## @item
## @var{x} is the computed approximation to the solution of
## @code{@var{A} * @var{x} = @var{b}}.
##
## @item
## @var{flag} reports on the convergence.  @code{@var{flag} = 0} means the
## solution converged and the tolerance criterion given by @var{tol} is
## satisfied.  @code{@var{flag} = 1} means that the @var{maxit} limit for the
## iteration count was reached.  @code{@var{flag} = 3} reports a @code{pcr}
## breakdown, see [1] for details.
##
## @item
## @var{relres} is the ratio of the final residual to its initial value,
## measured in the Euclidean norm.
##
## @item
## @var{iter} is the actual number of iterations performed.
##
## @item
## @var{resvec} describes the convergence history of the method, so that
## @code{@var{resvec} (i)} contains the Euclidean norms of the residual after
## the (@var{i}-1)-th iteration, @code{@var{i} = 1,2, @dots{}, @var{iter}+1}.
## @end itemize
##
## Let us consider a trivial problem with a diagonal matrix (we exploit the
## sparsity of A)
##
## @example
## @group
## n = 10;
## A = sparse (diag (1:n));
## b = rand (N, 1);
## @end group
## @end example
##
## @sc{Example 1:} Simplest use of @code{pcr}
##
## @example
## x = pcr (A, b)
## @end example
##
## @sc{Example 2:} @code{pcr} with a function which computes
## @code{@var{A} * @var{x}}.
##
## @example
## @group
## function y = apply_a (x)
##   y = [1:10]' .* x;
## endfunction
##
## x = pcr ("apply_a", b)
## @end group
## @end example
##
## @sc{Example 3:}  Preconditioned iteration, with full diagnostics.  The
## preconditioner (quite strange, because even the original matrix
## @var{A} is trivial) is defined as a function
##
## @example
## @group
## function y = apply_m (x)
##   k = floor (length (x) - 2);
##   y = x;
##   y(1:k) = x(1:k) ./ [1:k]';
## endfunction
##
## [x, flag, relres, iter, resvec] = ...
##                    pcr (A, b, [], [], "apply_m")
## semilogy ([1:iter+1], resvec);
## @end group
## @end example
##
## @sc{Example 4:} Finally, a preconditioner which depends on a
## parameter @var{k}.
##
## @example
## @group
## function y = apply_m (x, varargin)
##   k = varargin@{1@};
##   y = x;
##   y(1:k) = x(1:k) ./ [1:k]';
## endfunction
##
## [x, flag, relres, iter, resvec] = ...
##                    pcr (A, b, [], [], "apply_m"', [], 3)
## @end group
## @end example
##
## References:
##
## [1] @nospell{W. Hackbusch}, @cite{Iterative Solution of Large Sparse
## Systems of Equations}, section 9.5.4; Springer, 1994
##
## @seealso{sparse, pcg}
## @end deftypefn

## Author: Piotr Krzyzanowski <piotr.krzyzanowski@mimuw.edu.pl>

function [x, flag, relres, iter, resvec] = pcr (A, b, tol, maxit, m, x0, varargin)

  breakdown = false;

  if (nargin < 6 || isempty (x0))
    x = zeros (size (b));
  else
    x = x0;
  endif

  if (nargin < 5)
    m = [];
  endif

  if (nargin < 4 || isempty (maxit))
    maxit = 20;
  endif

  maxit += 2;

  if (nargin < 3 || isempty (tol))
    tol = 1e-6;
  endif

  if (nargin < 2)
    print_usage ();
  endif

  ##  init
  if (isnumeric (A))            # is A a matrix?
    r = b - A*x;
  else                          # then A should be a function!
    r = b - feval (A, x, varargin{:});
  endif

  if (isnumeric (m))            # is M a matrix?
    if (isempty (m))            # if M is empty, use no precond
      p = r;
    else                        # otherwise, apply the precond
      p = m \ r;
    endif
  else                          # then M should be a function!
    p = feval (m, r, varargin{:});
  endif

  iter = 2;

  b_bot_old = 1;
  q_old = p_old = s_old = zeros (size (x));

  if (isnumeric (A))            # is A a matrix?
    q = A * p;
  else                          # then A should be a function!
    q = feval (A, p, varargin{:});
  endif

  resvec(1) = abs (norm (r));

  ## iteration
  while (resvec(iter-1) > tol*resvec(1) && iter < maxit)

    if (isnumeric (m))          # is M a matrix?
      if (isempty (m))          # if M is empty, use no precond
        s = q;
      else                      # otherwise, apply the precond
        s = m \ q;
      endif
    else                        # then M should be a function!
      s = feval (m, q, varargin{:});
    endif
    b_top = r' * s;
    b_bot = q' * s;

    if (b_bot == 0.0)
      breakdown = true;
      break;
    endif
    lambda = b_top / b_bot;

    x += lambda*p;
    r -= lambda*q;

    if (isnumeric (A))          # is A a matrix?
      t = A*s;
    else                        # then A should be a function!
      t = feval (A, s, varargin{:});
    endif

    alpha0 = (t'*s) / b_bot;
    alpha1 = (t'*s_old) / b_bot_old;

    p_temp = p;
    q_temp = q;

    p = s - alpha0*p - alpha1*p_old;
    q = t - alpha0*q - alpha1*q_old;

    s_old = s;
    p_old = p_temp;
    q_old = q_temp;
    b_bot_old = b_bot;

    resvec(iter) = abs (norm (r));
    iter++;
  endwhile

  flag = 0;
  relres = resvec(iter-1) ./ resvec(1);
  iter -= 2;
  if (iter >= maxit-2)
    flag = 1;
    if (nargout < 2)
      warning ("pcr: maximum number of iterations (%d) reached\n", iter);
      warning ("the initial residual norm was reduced %g times.\n", 1.0/relres);
    endif
  elseif (nargout < 2 && ! breakdown)
    fprintf (stderr, "pcr: converged in %d iterations. \n", iter);
    fprintf (stderr, "the initial residual norm was reduced %g times.\n",
             1.0 / relres);
  endif

  if (breakdown)
    flag = 3;
    if (nargout < 2)
      warning ("pcr: breakdown occurred:\n");
      warning ("system matrix singular or preconditioner indefinite?\n");
    endif
  endif

endfunction


%!demo
%! ## Simplest usage of PCR (see also 'help pcr')
%!
%! N = 20;
%! A = diag (linspace (-3.1,3,N)); b = rand (N,1);
%! y = A \ b;  # y is the true solution
%! x = pcr (A,b);
%! printf ("The solution relative error is %g\n", norm (x-y) / norm (y));
%!
%! ## You shouldn't be afraid if PCR issues some warning messages in this
%! ## example: watch out in the second example, why it takes N iterations
%! ## of PCR to converge to (a very accurate, by the way) solution.

%!demo
%! ## Full output from PCR
%! ## We use this output to plot the convergence history
%!
%! N = 20;
%! A = diag (linspace (-3.1,30,N)); b = rand (N,1);
%! X = A \ b;  # X is the true solution
%! [x, flag, relres, iter, resvec] = pcr (A,b);
%! printf ("The solution relative error is %g\n", norm (x-X) / norm (X));
%! clf;
%! title ("Convergence history");
%! xlabel ("Iteration"); ylabel ("log(||b-Ax||/||b||)");
%! semilogy ([0:iter], resvec/resvec(1), "o-g;relative residual;");

%!demo
%! ## Full output from PCR
%! ## We use indefinite matrix based on the Hilbert matrix, with one
%! ## strongly negative eigenvalue
%! ## Hilbert matrix is extremely ill conditioned, so is ours,
%! ## and that's why PCR WILL have problems
%!
%! N = 10;
%! A = hilb (N); A(1,1) = -A(1,1); b = rand (N,1);
%! X = A \ b;  # X is the true solution
%! printf ("Condition number of A is   %g\n", cond (A));
%! [x, flag, relres, iter, resvec] = pcr (A,b,[],200);
%! if (flag == 3)
%!   printf ("PCR breakdown.  System matrix is [close to] singular\n");
%! end
%! clf;
%! title ("Convergence history");
%! xlabel ("Iteration"); ylabel ("log(||b-Ax||)");
%! semilogy ([0:iter], resvec, "o-g;absolute residual;");

%!demo
%! ## Full output from PCR
%! ## We use an indefinite matrix based on the 1-D Laplacian matrix for A,
%! ## and here we have cond(A) = O(N^2)
%! ## That's the reason we need some preconditioner; here we take
%! ## a very simple and not powerful Jacobi preconditioner,
%! ## which is the diagonal of A.
%!
%! ## Note that we use here indefinite preconditioners!
%!
%! N = 100;
%! A = zeros (N,N);
%! for i=1:N-1 # form 1-D Laplacian matrix
%!   A(i:i+1,i:i+1) = [2 -1; -1 2];
%! endfor
%! A = [A, zeros(size(A)); zeros(size(A)), -A];
%! b = rand (2*N,1);
%! X = A \ b;  # X is the true solution
%! maxit = 80;
%! printf ("System condition number is %g\n", cond (A));
%! ## No preconditioner: the convergence is very slow!
%!
%! [x, flag, relres, iter, resvec] = pcr (A,b,[],maxit);
%! clf;
%! title ("Convergence history");
%! xlabel ("Iteration"); ylabel ("log(||b-Ax||)");
%! semilogy ([0:iter], resvec, "o-g;NO preconditioning: absolute residual;");
%!
%! pause (1);
%! ## Test Jacobi preconditioner: it will not help much!!!
%!
%! M = diag (diag (A)); # Jacobi preconditioner
%! [x, flag, relres, iter, resvec] = pcr (A,b,[],maxit,M);
%! hold on;
%! semilogy ([0:iter],resvec,"o-r;JACOBI preconditioner: absolute residual;");
%!
%! pause (1);
%! ## Test nonoverlapping block Jacobi preconditioner: this one should give
%! ## some convergence speedup!
%!
%! M = zeros (N,N); k = 4;
%! for i=1:k:N # get k x k diagonal blocks of A
%!   M(i:i+k-1,i:i+k-1) = A(i:i+k-1,i:i+k-1);
%! endfor
%! M = [M, zeros(size (M)); zeros(size(M)), -M];
%! [x, flag, relres, iter, resvec] = pcr (A,b,[],maxit,M);
%! semilogy ([0:iter], resvec, "o-b;BLOCK JACOBI preconditioner: absolute residual;");
%! hold off;

%!test
%! ## solve small indefinite diagonal system
%!
%! N = 10;
%! A = diag (linspace (-10.1,10,N)); b = ones (N,1);
%! X = A \ b;  # X is the true solution
%! [x, flag] = pcr (A,b,[],N+1);
%! assert (norm (x-X) / norm (X) < 1e-10);
%! assert (flag, 0);

%!test
%! ## solve tridiagonal system, do not converge in default 20 iterations
%! ## should perform max allowable default number of iterations
%!
%! N = 100;
%! A = zeros (N,N);
%! for i=1:N-1 # form 1-D Laplacian matrix
%!   A(i:i+1,i:i+1) = [2 -1; -1 2];
%! endfor
%! b = ones (N,1);
%! X = A \ b;  # X is the true solution
%! [x, flag, relres, iter, resvec] = pcr (A,b,1e-12);
%! assert (flag, 1);
%! assert (relres > 0.6);
%! assert (iter, 20);

%!test
%! ## solve tridiagonal system with "perfect" preconditioner
%! ## converges in one iteration
%!
%! N = 100;
%! A = zeros (N,N);
%! for i=1:N-1 # form 1-D Laplacian matrix
%!   A(i:i+1,i:i+1) = [2 -1; -1 2];
%! endfor
%! b = ones (N,1);
%! X = A \ b;  # X is the true solution
%! [x, flag, relres, iter] = pcr (A,b,[],[],A,b);
%! assert (norm (x-X) / norm(X) < 1e-6);
%! assert (relres < 1e-6);
%! assert (flag, 0);
%! assert (iter, 1); # should converge in one iteration

