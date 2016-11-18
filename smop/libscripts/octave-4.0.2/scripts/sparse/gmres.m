## Copyright (C) 2009-2015 Carlo de Falco
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3 of the License, or (at your
## option) any later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{x} =} gmres (@var{A}, @var{b}, @var{m}, @var{rtol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0})
## @deftypefnx {Function File} {@var{x} =} gmres (@var{A}, @var{b}, @var{m}, @var{rtol}, @var{maxit}, @var{P})
## @deftypefnx {Function File} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} gmres (@dots{})
## Solve @code{A x = b} using the Preconditioned GMRES iterative method with
## restart, a.k.a. PGMRES(m).
##
## @itemize @minus
## @item @var{rtol} is the relative tolerance,
## if not given or set to [] the default value 1e-6 is used.
##
## @item @var{maxit} is the maximum number of outer iterations, if not given or
## set to [] the default value @code{min (10, numel (b) / restart)} is used.
##
## @item @var{x0} is the initial guess,
## if not given or set to [] the default value @code{zeros (size (b))} is used.
##
## @item @var{m} is the restart parameter,
## if not given or set to [] the default value @code{numel (b)} is used.
## @end itemize
##
## Argument @var{A} can be passed as a matrix, function handle, or inline
## function @code{f} such that @code{f(x) = A*x}.
##
## The preconditioner @var{P} is given as @code{P = M1 * M2}.  Both @var{M1}
## and @var{M2} can be passed as a matrix, function handle, or inline function
## @code{g} such that @code{g(x) = M1\x} or @code{g(x) = M2\x}.
##
## Besides the vector @var{x}, additional outputs are:
##
## @itemize @minus
## @item @var{flag} indicates the exit status:
##
## @table @asis
## @item 0 : iteration converged to within the specified tolerance
##
## @item 1 : maximum number of iterations exceeded
##
## @item 2 : unused, but skipped for compatibility
##
## @item 3 : algorithm reached stagnation (no change between iterations)
## @end table
##
## @item @var{relres} is the final value of the relative residual.
##
## @item @var{iter} is a vector containing the number of outer iterations and
## total iterations performed.
##
## @item @var{resvec} is a vector containing the relative residual at each
## iteration.
## @end itemize
##
## @seealso{bicg, bicgstab, cgs, pcg, pcr, qmr}
## @end deftypefn

function [x, flag, relres, it, resvec] = gmres (A, b, restart, rtol, maxit, M1, M2, x0)

  if (nargin < 2 || nargin > 8)
    print_usage ();
  endif

  if (ischar (A))
    Ax = str2func (A);
  elseif (isnumeric (A) && issquare (A))
    Ax = @(x) A*x;
  elseif (isa (A, "function_handle"))
    Ax = A;
  else
    error ("gmres: A must be a function or square matrix");
  endif

  if (nargin < 3 || isempty (restart))
    restart = rows (b);
  endif

  if (nargin < 4 || isempty (rtol))
    rtol = 1e-6;
  endif

  if (nargin < 5 || isempty (maxit))
    maxit = min (rows (b)/restart, 10);
  endif

  if (nargin < 6 || isempty (M1))
    M1m1x = @(x) x;
  elseif (ischar (M1))
    M1m1x = str2func (M1);
  elseif (isnumeric (M1) && ismatrix (M1))
    M1m1x = @(x) M1 \ x;
  elseif (isa (M1, "function_handle"))
    M1m1x = M1;
  else
    error ("gmres: preconditioner M1 must be a function or matrix");
  endif

  if (nargin < 7 || isempty (M2))
    M2m1x = @(x) x;
  elseif (ischar (M2))
    M2m1x = str2func (M2);
  elseif (isnumeric (M2) && ismatrix (M2))
    M2m1x = @(x) M2 \ x;
  elseif (isa (M2, "function_handle"))
    M2m1x = M2;
  else
    error ("gmres: preconditioner M2 must be a function or matrix");
  endif

  Pm1x = @(x) M2m1x (M1m1x (x));

  if (nargin < 8 || isempty (x0))
    x0 = zeros (size (b));
  endif

  x_old = x0;
  x = x_old;
  prec_res = Pm1x (b - Ax (x_old));
  presn = norm (prec_res, 2);

  B = zeros (restart + 1, 1);
  V = zeros (rows (x), restart);
  H = zeros (restart + 1, restart);

  ## begin loop
  iter = 1;
  restart_it  = restart + 1;
  resvec      = zeros (maxit, 1);
  resvec(1)   = presn;
  prec_b_norm = norm (Pm1x (b), 2);
  flag        = 1;  # Default flag is maximum # of iterations exceeded

  while (iter <= maxit * restart && presn > rtol * prec_b_norm)

    ## restart
    if (restart_it > restart)
      restart_it = 1;
      x_old = x;
      prec_res = Pm1x (b - Ax (x_old));
      presn = norm (prec_res, 2);
      B(1) = presn;
      H(:) = 0;
      V(:, 1) = prec_res / presn;
    endif

    ## basic iteration
    tmp = Pm1x (Ax (V(:, restart_it)));
    [V(:,restart_it+1), H(1:restart_it+1, restart_it)] = ...
        mgorth (tmp, V(:,1:restart_it));

    Y = (H(1:restart_it+1, 1:restart_it) \ B(1:restart_it+1));

    little_res = B(1:restart_it+1) - ...
        H(1:restart_it+1, 1:restart_it) * Y(1:restart_it);

    presn = norm (little_res, 2);

    x = x_old + V(:, 1:restart_it) * Y(1:restart_it);

    resvec(iter+1) = presn;
    if (norm (x - x_old, inf) <= eps)
      flag = 3;  # Stagnation: no change between iterations
      break;
    endif

    restart_it++ ;
    iter++;
  endwhile

  if (nargout > 1)
    ## Calculate extra outputs as requested
    relres = presn / prec_b_norm;
    if (relres <= rtol)
      flag = 0;  # Converged to solution within tolerance
    endif

    it = [floor(iter/restart), restart_it-1];
  endif

endfunction


%!demo
%! dim = 20;
%! A = spdiags ([-ones(dim,1) 2*ones(dim,1) ones(dim,1)], [-1:1], dim, dim);
%! b = ones (dim, 1);
%! [x, flag, relres, iter, resvec] = gmres (A, b, 10, 1e-10, dim, @(x) x ./ diag (A), [], b)

%!shared A, b, dim
%! dim = 100;
%!test
%! A = spdiags ([-ones(dim,1) 2*ones(dim,1) ones(dim,1)], [-1:1], dim, dim);
%! b = ones (dim, 1);
%! x = gmres (A, b, 10, 1e-10, dim, @(x) x ./ diag (A), [], b);
%! assert (x, A\b, 1e-9*norm (x, Inf));
%!
%!test
%! x = gmres (A, b, dim, 1e-10, 1e4, @(x) diag (diag (A)) \ x, [], b);
%! assert(x, A\b, 1e-7*norm (x, Inf));
%!
%!test
%! A = spdiags ([[1./(2:2:2*(dim-1)) 0]; 1./(1:2:2*dim-1); [0 1./(2:2:2*(dim-1))]]', -1:1, dim, dim);
%! A = A'*A;
%! b = rand (dim, 1);
%! [x, resvec] = gmres (@(x) A*x, b, dim, 1e-10, dim, @(x) x./diag (A), [], []);
%! assert (x, A\b, 1e-9*norm (x, Inf));
%! x = gmres (@(x) A*x, b, dim, 1e-10, 1e6, @(x) diag (diag (A)) \ x, [], []);
%! assert (x, A\b, 1e-9*norm (x, Inf));
%!test
%! x = gmres (@(x) A*x, b, dim, 1e-10, 1e6, @(x) x ./ diag (A), [], []);
%! assert (x, A\b, 1e-7*norm (x, Inf));


%!error gmres (1)
%!error gmres (1,2,3,4,5,6,7,8,9)
%!error <A must be> gmres ({1},2)
%!error <A must be a function or square matrix> gmres ({1},2)
%!error <M1 must be a function or matrix> gmres (1,2,3,4,5,{6})
%!error <M2 must be a function or matrix> gmres (1,2,3,4,5,6,{7})

