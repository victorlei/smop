## Copyright (C) 2006 Sylvain Pelissier
## Copyright (C) 2012-2015 Carlo de Falco
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
## @deftypefn  {Function File} {@var{x} =} bicg (@var{A}, @var{b}, @var{rtol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0})
## @deftypefnx {Function File} {@var{x} =} bicg (@var{A}, @var{b}, @var{rtol}, @var{maxit}, @var{P})
## @deftypefnx {Function File} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} bicg (@var{A}, @var{b}, @dots{})
## Solve @code{A x = b} using the Bi-conjugate gradient iterative method.
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
## @var{A} can be passed as a matrix or as a function handle or inline function
## @code{f} such that @code{f(x, "notransp") = A*x} and
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
## @item @var{resvec} is a vector containing the relative residual at each
## iteration.
## @end itemize
##
## @seealso{bicgstab, cgs, gmres, pcg, qmr}
##
## @end deftypefn

## Author: Sylvain Pelissier <sylvain.pelissier@gmail.com>
## Author: Carlo de Falco

function [x, flag, res1, k, resvec] = bicg (A, b, tol, maxit, M1, M2, x0)

  if (nargin >= 2 && isvector (full (b)))

    if (ischar (A))
      fun = str2func (A);
      Ax  = @(x) feval (fun, x, "notransp");
      Atx = @(x) feval (fun, x, "transp");
    elseif (isnumeric (A) && issquare (A))
      Ax  = @(x) A  * x;
      Atx = @(x) A' * x;
    elseif (isa (A, "function_handle"))
      Ax  = @(x) feval (A, x, "notransp");
      Atx = @(x) feval (A, x, "transp");
    else
      error ("bicg: A must be a function or square matrix");
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
    elseif (isnumeric (M1) && ismatrix (M1))
      M1m1x  = @(x) M1  \ x;
      M1tm1x = @(x) M1' \ x;
    elseif (isa (M1, "function_handle"))
      M1m1x  = @(x) feval (M1, x, "notransp");
      M1tm1x = @(x) feval (M1, x, "transp");
    else
      error ("bicg: preconditioner M1 must be a function or matrix");
    endif

    if (nargin < 6 || isempty (M2))
      M2m1x = @(x, ignore) x;
      M2tm1x = M2m1x;
    elseif (ischar (M2))
      fun = str2func (M2);
      M2m1x  = @(x) feval (fun, x, "notransp");
      M2tm1x = @(x) feval (fun, x, "transp");
    elseif (isnumeric (M2) && ismatrix (M2))
      M2m1x  = @(x) M2  \ x;
      M2tm1x = @(x) M2' \ x;
    elseif (isa (M2, "function_handle"))
      M2m1x  = @(x) feval (M2, x, "notransp");
      M2tm1x = @(x) feval (M2, x, "transp");
    else
      error ("bicg: preconditioner M2 must be a function or matrix");
    endif

    Pm1x  = @(x) M2m1x  (M1m1x (x));
    Ptm1x = @(x) M1tm1x (M2tm1x (x));

    if (nargin < 7 || isempty (x0))
      x0 = zeros (size (b));
    endif

    y = x = x0;
    c = b;

    r0 = b - Ax (x);
    s0 = c - Atx (y);

    d = Pm1x (r0);
    f = Ptm1x (s0);

    bnorm = norm (b);
    res0  = Inf;

    if (any (r0 != 0))

      for k = 1:maxit

        a  = (s0' * Pm1x (r0)) ./ (f' * Ax (d));

        x += a * d;
        y += conj (a) * f;

        r1 = r0 - a * Ax (d);
        s1 = s0 - conj (a) * Atx (f);

        beta = (s1' * Pm1x (r1)) ./ (s0' * Pm1x (r0));

        d = Pm1x (r1) + beta * d;
        f = Ptm1x (s1) + conj (beta) * f;

        r0 = r1;
        s0 = s1;

        res1 = norm (b - Ax (x)) / bnorm;
        if (res1 < tol)
          flag = 0;
          if (nargout < 2)
            printf ("bicg converged at iteration %i ", k);
            printf ("to a solution with relative residual %e\n", res1);
          endif
          break;
        endif

        if (res0 <= res1)
          flag = 3;
          printf ("bicg stopped at iteration %i ", k);
          printf ("without converging to the desired tolerance %e\n", tol);
          printf ("because the method stagnated.\n");
          printf ("The iterate returned (number %i) ", k-1);
          printf ("has relative residual %e\n", res0);
          break
        endif
        res0 = res1;
        if (nargout > 4)
          resvec(k) = res0;
        endif
      endfor

      if (k == maxit)
        flag = 1;
        printf ("bicg stopped at iteration %i ", maxit);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the maximum number of iterations was reached. ");
        printf ("The iterate returned (number %i) has ", maxit);
        printf ("relative residual %e\n", res1);
      endif

    else
      flag = 0;
      if (nargout < 2)
        printf ("bicg converged after 0 interations\n");
      endif
    endif

  else
    print_usage ();
  endif

endfunction;


%!test
%! n = 100;
%! A = spdiags ([-2*ones(n,1) 4*ones(n,1) -ones(n,1)], -1:1, n, n);
%! b = sum (A, 2);
%! tol = 1e-8;
%! maxit = 15;
%! M1 = spdiags ([ones(n,1)/(-2) ones(n,1)],-1:0, n, n);
%! M2 = spdiags ([4*ones(n,1) -ones(n,1)], 0:1, n, n);
%! [x, flag, relres, iter, resvec] = bicg (A, b, tol, maxit, M1, M2);
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
%! [x, flag, relres, iter, resvec] = bicg (@(x, t) afun (x, t, A),
%!                                         b, tol, maxit, M1, M2);
%! assert (x, ones (size (b)), 1e-7);

%!test
%! n = 100;
%! tol = 1e-8;
%! a = sprand (n, n, .1);
%! A = a' * a + 100 * eye (n);
%! b = sum (A, 2);
%! [x, flag, relres, iter, resvec] = bicg (A, b, tol, [], diag (diag (A)));
%! assert (x, ones (size (b)), 1e-7);

