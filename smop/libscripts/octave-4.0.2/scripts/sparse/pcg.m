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
## @deftypefn  {Function File} {@var{x} =} pcg (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{m1}, @var{m2}, @var{x0}, @dots{})
## @deftypefnx {Function File} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}, @var{eigest}] =} pcg (@dots{})
##
## Solve the linear system of equations @w{@code{@var{A} * @var{x} = @var{b}}}
## by means of the Preconditioned Conjugate Gradient iterative method.
##
## The input arguments are
##
## @itemize
## @item
## @var{A} can be either a square (preferably sparse) matrix or a function
## handle, inline function or string containing the name of a function which
## computes @w{@code{@var{A} * @var{x}}}.  In principle, @var{A} should be
## symmetric and positive definite; if @code{pcg} finds @var{A} not to be
## positive definite, a warning is printed and the @var{flag} output will be
## set.
##
## @item
## @var{b} is the right-hand side vector.
##
## @item
## @var{tol} is the required relative tolerance for the residual error,
## @w{@code{@var{b} - @var{A} * @var{x}}}.  The iteration stops if
## @w{@code{norm (@var{b} - @var{A} * @var{x})} @leq{}
## @w{@var{tol} * norm (@var{b})}}.
## If @var{tol} is omitted or empty then a tolerance of 1e-6 is used.
##
## @item
## @var{maxit} is the maximum allowable number of iterations; if @var{maxit}
## is omitted or empty then a value of 20 is used.
##
## @item
## @var{m} = @var{m1} * @var{m2} is the (left) preconditioning matrix, so that
## the iteration is (theoretically) equivalent to solving by @code{pcg}
## @w{@code{@var{P} * @var{x} = @var{m} \ @var{b}}}, with
## @w{@code{@var{P} = @var{m} \ @var{A}}}.
## Note that a proper choice of the preconditioner may dramatically improve
## the overall performance of the method.  Instead of matrices @var{m1} and
## @var{m2}, the user may pass two functions which return the results of
## applying the inverse of @var{m1} and @var{m2} to a vector (usually this is
## the preferred way of using the preconditioner).  If @var{m1} is omitted or
## empty @code{[]} then no preconditioning is applied.  If @var{m2} is
## omitted, @var{m} = @var{m1} will be used as a preconditioner.
##
## @item
## @var{x0} is the initial guess.  If @var{x0} is omitted or empty then the
## function sets @var{x0} to a zero vector by default.
## @end itemize
##
## The arguments which follow @var{x0} are treated as parameters, and passed in
## a proper way to any of the functions (@var{A} or @var{m}) which are passed
## to @code{pcg}.  See the examples below for further details.  The output
## arguments are
##
## @itemize
## @item
## @var{x} is the computed approximation to the solution of
## @w{@code{@var{A} * @var{x} = @var{b}}}.
##
## @item
## @var{flag} reports on the convergence.  A value of 0 means the solution
## converged and the tolerance criterion given by @var{tol} is satisfied.
## A value of 1 means that the @var{maxit} limit for the iteration count was
## reached.  A value of 3 indicates that the (preconditioned) matrix was found
## not to be positive definite.
##
## @item
## @var{relres} is the ratio of the final residual to its initial value,
## measured in the Euclidean norm.
##
## @item
## @var{iter} is the actual number of iterations performed.
##
## @item
## @var{resvec} describes the convergence history of the method.
## @code{@var{resvec}(i,1)} is the Euclidean norm of the residual, and
## @code{@var{resvec}(i,2)} is the preconditioned residual norm, after the
## (@var{i}-1)-th iteration, @code{@var{i} = 1, 2, @dots{}, @var{iter}+1}.
## The preconditioned residual norm is defined as
## @code{norm (@var{r}) ^ 2 = @var{r}' * (@var{m} \ @var{r})} where
## @code{@var{r} = @var{b} - @var{A} * @var{x}}, see also the
## description of @var{m}.  If @var{eigest} is not required, only
## @code{@var{resvec}(:,1)} is returned.
##
## @item
## @var{eigest} returns the estimate for the smallest @code{@var{eigest}(1)}
## and largest @code{@var{eigest}(2)} eigenvalues of the preconditioned matrix
## @w{@code{@var{P} = @var{m} \ @var{A}}}.  In particular, if no
## preconditioning is used, the estimates for the extreme eigenvalues of
## @var{A} are returned.  @code{@var{eigest}(1)} is an overestimate and
## @code{@var{eigest}(2)} is an underestimate, so that
## @code{@var{eigest}(2) / @var{eigest}(1)} is a lower bound for
## @code{cond (@var{P}, 2)}, which nevertheless in the limit should
## theoretically be equal to the actual value of the condition number.
## The method which computes @var{eigest} works only for symmetric positive
## definite @var{A} and @var{m}, and the user is responsible for verifying this
## assumption.
## @end itemize
##
## Let us consider a trivial problem with a diagonal matrix (we exploit the
## sparsity of A)
##
## @example
## @group
## n = 10;
## A = diag (sparse (1:n));
## b = rand (n, 1);
## [l, u, p] = ilu (A, struct ("droptol", 1.e-3));
## @end group
## @end example
##
## @sc{Example 1:} Simplest use of @code{pcg}
##
## @example
## x = pcg (A, b)
## @end example
##
## @sc{Example 2:} @code{pcg} with a function which computes
## @code{@var{A} * @var{x}}
##
## @example
## @group
## function y = apply_a (x)
##   y = [1:N]' .* x;
## endfunction
##
## x = pcg ("apply_a", b)
## @end group
## @end example
##
## @sc{Example 3:} @code{pcg} with a preconditioner: @var{l} * @var{u}
##
## @example
## x = pcg (A, b, 1.e-6, 500, l*u)
## @end example
##
## @sc{Example 4:} @code{pcg} with a preconditioner: @var{l} * @var{u}.
## Faster than @sc{Example 3} since lower and upper triangular matrices are
## easier to invert
##
## @example
## x = pcg (A, b, 1.e-6, 500, l, u)
## @end example
##
## @sc{Example 5:} Preconditioned iteration, with full diagnostics.  The
## preconditioner (quite strange, because even the original matrix @var{A} is
## trivial) is defined as a function
##
## @example
## @group
## function y = apply_m (x)
##   k = floor (length (x) - 2);
##   y = x;
##   y(1:k) = x(1:k) ./ [1:k]';
## endfunction
##
## [x, flag, relres, iter, resvec, eigest] = ...
##                    pcg (A, b, [], [], "apply_m");
## semilogy (1:iter+1, resvec);
## @end group
## @end example
##
## @sc{Example 6:} Finally, a preconditioner which depends on a parameter
## @var{k}.
##
## @example
## @group
## function y = apply_M (x, varargin)
##   K = varargin@{1@};
##   y = x;
##   y(1:K) = x(1:K) ./ [1:K]';
## endfunction
##
## [x, flag, relres, iter, resvec, eigest] = ...
##      pcg (A, b, [], [], "apply_m", [], [], 3)
## @end group
## @end example
##
## References:
##
## @enumerate
## @item
## C.T. Kelley, @cite{Iterative Methods for Linear and Nonlinear Equations},
## SIAM, 1995. (the base PCG algorithm)
##
## @item
## @nospell{Y. Saad}, @cite{Iterative Methods for Sparse Linear Systems},
## @nospell{PWS} 1996. (condition number estimate from PCG)
## Revised version of this book is available online at
## @url{http://www-users.cs.umn.edu/~saad/books.html}
## @end enumerate
##
## @seealso{sparse, pcr}
## @end deftypefn

## Author: Piotr Krzyzanowski <piotr.krzyzanowski@mimuw.edu.pl>
## Modified by: Vittoria Rezzonico <vittoria.rezzonico@epfl.ch>
##  - Add the ability to provide the pre-conditioner as two separate matrices

function [x, flag, relres, iter, resvec, eigest] = pcg (A, b, tol, maxit, m1, m2, x0, varargin)

  ## M = M1*M2

  if (nargin < 7 || isempty (x0))
    x = zeros (size (b));
  else
    x = x0;
  endif

  if (nargin < 5 || isempty (m1))
     exist_m1 = 0;
  else
     exist_m1 = 1;
  endif

  if (nargin < 6 || isempty (m2))
     exist_m2 = 0;
  else
     exist_m2 = 1;
  endif

  if (nargin < 4 || isempty (maxit))
    maxit = min (rows (b), 20);
  endif

  maxit += 2;

  if (nargin < 3 || isempty (tol))
    tol = 1e-6;
  endif

  preconditioned_residual_out = false;
  if (nargout > 5)
    T = zeros (maxit, maxit);
    preconditioned_residual_out = true;
  endif

  ## Assume A is positive definite.
  matrix_positive_definite = true;

  p = zeros (size (b));
  oldtau = 1;
  if (isnumeric (A))
    ## A is a matrix.
    r = b - A*x;
  else
    ## A should be a function.
    r = b - feval (A, x, varargin{:});
  endif

  b_norm = norm (b);
  resvec(1,1) = norm (r);
  alpha = 1;
  iter = 2;

  while (resvec(iter-1,1) > tol * b_norm && iter < maxit)
    if (exist_m1)
      if (isnumeric (m1))
        y = m1 \ r;
      else
        y = feval (m1, r, varargin{:});
      endif
    else
      y = r;
    endif
    if (exist_m2)
      if (isnumeric (m2))
        z = m2 \ y;
      else
        z = feval (m2, y, varargin{:});
      endif
    else
      z = y;
    endif
    tau = z' * r;
    resvec(iter-1,2) = sqrt (tau);
    beta = tau / oldtau;
    oldtau = tau;
    p = z + beta * p;
    if (isnumeric (A))
      ## A is a matrix.
      w = A * p;
    else
      ## A should be a function.
      w = feval (A, p, varargin{:});
    endif
    ## Needed only for eigest.
    oldalpha = alpha;
    alpha = tau / (p'*w);
    if (alpha <= 0.0)
      ## Negative matrix.
      matrix_positive_definite = false;
    endif
    x += alpha * p;
    r -= alpha * w;
    if (nargout > 5 && iter > 2)
      T(iter-1:iter, iter-1:iter) = T(iter-1:iter, iter-1:iter) + ...
          [1 sqrt(beta); sqrt(beta) beta]./oldalpha;
      ## EVS = eig (T(2:iter-1,2:iter-1));
      ## fprintf (stderr,"PCG condest: %g (iteration: %d)\n", max (EVS)/min (EVS),iter);
    endif
    resvec(iter,1) = norm (r);
    iter++;
  endwhile

  if (nargout > 5)
    if (matrix_positive_definite)
      if (iter > 3)
        T = T(2:iter-2,2:iter-2);
        l = eig (T);
        eigest = [min(l), max(l)];
        ## fprintf (stderr, "pcg condest: %g\n", eigest(2)/eigest(1));
      else
        eigest = [NaN, NaN];
        warning ("pcg: eigenvalue estimate failed: iteration converged too fast");
      endif
    else
      eigest = [NaN, NaN];
    endif

    ## Apply the preconditioner once more and finish with the precond
    ## residual.
    if (exist_m1)
      if (isnumeric (m1))
        y = m1 \ r;
      else
        y = feval (m1, r, varargin{:});
      endif
    else
      y = r;
    endif
    if (exist_m2)
      if (isnumeric (m2))
        z = m2 \ y;
      else
        z = feval (m2, y, varargin{:});
      endif
    else
      z = y;
    endif

    resvec(iter-1,2) = sqrt (r' * z);
  else
    resvec = resvec(:,1);
  endif

  flag = 0;
  relres = resvec(iter-1,1) ./ resvec(1,1);
  iter -= 2;
  if (iter >= maxit - 2)
    flag = 1;
    if (nargout < 2)
      warning ("pcg: maximum number of iterations (%d) reached\n", iter);
      warning ("the initial residual norm was reduced %g times.\n", ...
               1.0 / relres);
    endif
  elseif (nargout < 2)
    fprintf (stderr, "pcg: converged in %d iterations. ", iter);
    fprintf (stderr, "the initial residual norm was reduced %g times.\n",...
             1.0/relres);
  endif

  if (! matrix_positive_definite)
    flag = 3;
    if (nargout < 2)
      warning ("pcg: matrix not positive definite?\n");
    endif
  endif
endfunction


%!demo
%! ## Simplest usage of pcg (see also 'help pcg')
%!
%! N = 10;
%! A = diag ([1:N]); b = rand (N, 1);
%! y = A \ b;  # y is the true solution
%! x = pcg (A, b);
%! printf ("The solution relative error is %g\n", norm (x - y) / norm (y));
%!
%! ## You shouldn't be afraid if pcg issues some warning messages in this
%! ## example: watch out in the second example, why it takes N iterations
%! ## of pcg to converge to (a very accurate, by the way) solution

%!demo
%! ## Full output from pcg, except for the eigenvalue estimates
%! ## We use this output to plot the convergence history
%!
%! N = 10;
%! A = diag ([1:N]); b = rand (N, 1);
%! X = A \ b;  # X is the true solution
%! [x, flag, relres, iter, resvec] = pcg (A, b);
%! printf ("The solution relative error is %g\n", norm (x - X) / norm (X));
%! title ("Convergence history");
%! semilogy ([0:iter], resvec / resvec(1), "o-g");
%! xlabel ("Iteration"); ylabel ("log(||b-Ax||/||b||)");
%! legend ("relative residual");

%!demo
%! ## Full output from pcg, including the eigenvalue estimates
%! ## Hilbert matrix is extremely ill-conditioned, so pcg WILL have problems
%!
%! N = 10;
%! A = hilb (N); b = rand (N, 1);
%! X = A \ b;  # X is the true solution
%! [x, flag, relres, iter, resvec, eigest] = pcg (A, b, [], 200);
%! printf ("The solution relative error is %g\n", norm (x - X) / norm (X));
%! printf ("Condition number estimate is %g\n", eigest(2) / eigest(1));
%! printf ("Actual condition number is   %g\n", cond (A));
%! title ("Convergence history");
%! semilogy ([0:iter], resvec, ["o-g";"+-r"]);
%! xlabel ("Iteration"); ylabel ("log(||b-Ax||)");
%! legend ("absolute residual", "absolute preconditioned residual");

%!demo
%! ## Full output from pcg, including the eigenvalue estimates
%! ## We use the 1-D Laplacian matrix for A, and cond(A) = O(N^2)
%! ## and that's the reason we need some preconditioner; here we take
%! ## a very simple and not powerful Jacobi preconditioner,
%! ## which is the diagonal of A.
%!
%! N = 100;
%! A = zeros (N, N);
%! for i = 1 : N - 1 # form 1-D Laplacian matrix
%!   A(i:i+1, i:i+1) = [2 -1; -1 2];
%! endfor
%! b = rand (N, 1);
%! X = A \ b;  # X is the true solution
%! maxit = 80;
%! printf ("System condition number is %g\n", cond (A));
%! ## No preconditioner: the convergence is very slow!
%!
%! [x, flag, relres, iter, resvec, eigest] = pcg (A, b, [], maxit);
%! printf ("System condition number estimate is %g\n", eigest(2) / eigest(1));
%! title ("Convergence history");
%! semilogy ([0:iter], resvec(:,1), "o-g");
%! xlabel ("Iteration"); ylabel ("log(||b-Ax||)");
%! legend ("NO preconditioning: absolute residual");
%!
%! pause (1);
%! ## Test Jacobi preconditioner: it will not help much!!!
%!
%! M = diag (diag (A)); # Jacobi preconditioner
%! [x, flag, relres, iter, resvec, eigest] = pcg (A, b, [], maxit, M);
%! printf ("JACOBI preconditioned system condition number estimate is %g\n", eigest(2) / eigest(1));
%! hold on;
%! semilogy ([0:iter], resvec(:,1), "o-r");
%! legend ("NO preconditioning: absolute residual", ...
%!         "JACOBI preconditioner: absolute residual");
%!
%! pause (1);
%! ## Test nonoverlapping block Jacobi preconditioner: it will help much!
%!
%! M = zeros (N, N); k = 4;
%! for i = 1 : k : N # form 1-D Laplacian matrix
%!   M(i:i+k-1, i:i+k-1) = A(i:i+k-1, i:i+k-1);
%! endfor
%! [x, flag, relres, iter, resvec, eigest] = pcg (A, b, [], maxit, M);
%! printf ("BLOCK JACOBI preconditioned system condition number estimate is %g\n", eigest(2) / eigest(1));
%! semilogy ([0:iter], resvec(:,1), "o-b");
%! legend ("NO preconditioning: absolute residual", ...
%!         "JACOBI preconditioner: absolute residual", ...
%!         "BLOCK JACOBI preconditioner: absolute residual");
%! hold off;

%!test
%! ## solve small diagonal system
%!
%! N = 10;
%! A = diag ([1:N]); b = rand (N, 1);
%! X = A \ b;  # X is the true solution
%! [x, flag] = pcg (A, b, [], N+1);
%! assert (norm (x - X) / norm (X), 0, 1e-10);
%! assert (flag, 0);

%!test
%! ## solve small indefinite diagonal system
%! ## despite A is indefinite, the iteration continues and converges
%! ## indefiniteness of A is detected
%!
%! N = 10;
%! A = diag([1:N] .* (-ones(1, N) .^ 2)); b = rand (N, 1);
%! X = A \ b;  # X is the true solution
%! [x, flag] = pcg (A, b, [], N+1);
%! assert (norm (x - X) / norm (X), 0, 1e-10);
%! assert (flag, 3);

%!test
%! ## solve tridiagonal system, do not converge in default 20 iterations
%!
%! N = 100;
%! A = zeros (N, N);
%! for i = 1 : N - 1 # form 1-D Laplacian matrix
%!   A(i:i+1, i:i+1) = [2 -1; -1 2];
%! endfor
%! b = ones (N, 1);
%! X = A \ b;  # X is the true solution
%! [x, flag, relres, iter, resvec, eigest] = pcg (A, b, 1e-12);
%! assert (flag);
%! assert (relres > 1.0);
%! assert (iter, 20); # should perform max allowable default number of iterations

%!test
%! ## solve tridiagonal system with 'perfect' preconditioner
%! ## which converges in one iteration, so the eigest does not
%! ## work and issues a warning
%!
%! N = 100;
%! A = zeros (N, N);
%! for i = 1 : N - 1  # form 1-D Laplacian matrix
%!   A(i:i+1, i:i+1) = [2 -1; -1 2];
%! endfor
%! b = ones (N, 1);
%! X = A \ b;  # X is the true solution
%! [x, flag, relres, iter, resvec, eigest] = pcg (A, b, [], [], A, [], b);
%! assert (norm (x - X) / norm (X), 0, 1e-6);
%! assert (flag, 0);
%! assert (iter, 1); # should converge in one iteration
%! assert (isnan (eigest), isnan ([NaN, NaN]));

