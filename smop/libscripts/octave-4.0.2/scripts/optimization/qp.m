## Copyright (C) 2013-2015 Julien Bect
## Copyright (C) 2000-2015 Gabriele Pannocchia.
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
## @deftypefn  {Function File} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H})
## @deftypefnx {Function File} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H}, @var{q})
## @deftypefnx {Function File} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H}, @var{q}, @var{A}, @var{b})
## @deftypefnx {Function File} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H}, @var{q}, @var{A}, @var{b}, @var{lb}, @var{ub})
## @deftypefnx {Function File} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@var{x0}, @var{H}, @var{q}, @var{A}, @var{b}, @var{lb}, @var{ub}, @var{A_lb}, @var{A_in}, @var{A_ub})
## @deftypefnx {Function File} {[@var{x}, @var{obj}, @var{info}, @var{lambda}] =} qp (@dots{}, @var{options})
## Solve a quadratic program (QP).
##
## Solve the quadratic program defined by
## @tex
## $$
##  \min_x {1 \over 2} x^T H x + x^T q
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## min 0.5 x'*H*x + x'*q
##  x
## @end group
## @end example
##
## @end ifnottex
## subject to
## @tex
## $$
##  Ax = b \qquad lb \leq x \leq ub \qquad A_{lb} \leq A_{in} \leq A_{ub}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## A*x = b
## lb <= x <= ub
## A_lb <= A_in*x <= A_ub
## @end group
## @end example
##
## @end ifnottex
## @noindent
## using a null-space active-set method.
##
## Any bound (@var{A}, @var{b}, @var{lb}, @var{ub}, @var{A_lb}, @var{A_ub})
## may be set to the empty matrix (@code{[]}) if not present.  If the initial
## guess is feasible the algorithm is faster.
##
## @table @var
## @item options
## An optional structure containing the following parameter(s) used to define
## the behavior of the solver.  Missing elements in the structure take on
## default values, so you only need to set the elements that you wish to
## change from the default.
##
## @table @code
## @item MaxIter (default: 200)
## Maximum number of iterations.
## @end table
## @end table
##
## @table @var
## @item info
## Structure containing run-time information about the algorithm.  The
## following fields are defined:
##
## @table @code
## @item solveiter
## The number of iterations required to find the solution.
##
## @item info
## An integer indicating the status of the solution.
##
## @table @asis
## @item 0
## The problem is feasible and convex.  Global solution found.
##
## @item 1
## The problem is not convex.  Local solution found.
##
## @item 2
## The problem is not convex and unbounded.
##
## @item 3
## Maximum number of iterations reached.
##
## @item 6
## The problem is infeasible.
## @end table
## @end table
## @end table
## @end deftypefn

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("qp");

function [x, obj, INFO, lambda] = qp (x0, H, varargin)

  nargs = nargin;

  if (nargin == 1 && ischar (x0) && strcmp (x0, 'defaults'))
    x = optimset ("MaxIter", 200);
    return;
  endif

  if (nargs > 2 && isstruct (varargin{end}))
    options = varargin{end};
    nargs--;
  else
    options = struct ();
  endif

  if (nargs >= 3)
    q = varargin{1};
  else
    q = [];
  endif

  if (nargs >= 5)
    A = varargin{2};
    b = varargin{3};
  else
    A = [];
    b = [];
  endif

  if (nargs >= 7)
    lb = varargin{4};
    ub = varargin{5};
  else
    lb = [];
    ub = [];
  endif

  if (nargs == 10)
    A_lb = varargin{6};
    A_in = varargin{7};
    A_ub = varargin{8};
  else
    A_lb = [];
    A_in = [];
    A_ub = [];
  endif

  if (nargs == 2 || nargs == 3 || nargs == 5 || nargs == 7 || nargs == 10)

    maxit = optimget (options, "MaxIter", 200);

    ## Checking the quadratic penalty
    if (! issquare (H))
      error ("qp: quadratic penalty matrix not square");
    elseif (! ishermitian (H))
      ## warning ("qp: quadratic penalty matrix not hermitian");
      H = (H + H')/2;
    endif
    n = rows (H);

    ## Checking the initial guess (if empty it is resized to the
    ## right dimension and filled with 0)
    if (isempty (x0))
      x0 = zeros (n, 1);
    elseif (numel (x0) != n)
      error ("qp: the initial guess has incorrect length");
    endif

    ## Linear penalty.
    if (isempty (q))
      q = zeros (n, 1);
    elseif (numel (q) != n)
      error ("qp: the linear term has incorrect length");
    endif

    ## Equality constraint matrices
    if (isempty (A) || isempty (b))
      A = zeros (0, n);
      b = zeros (0, 1);
      n_eq = 0;
    else
      [n_eq, n1] = size (A);
      if (n1 != n)
        error ("qp: equality constraint matrix has incorrect column dimension");
      endif
      if (numel (b) != n_eq)
        error ("qp: equality constraint matrix and vector have inconsistent dimension");
      endif
    endif

    ## Bound constraints
    Ain = zeros (0, n);
    bin = zeros (0, 1);
    n_in = 0;
    if (nargs > 5)
      if (! isempty (lb))
        if (numel (lb) != n)
          error ("qp: lower bound has incorrect length");
        elseif (isempty (ub))
          Ain = [Ain; eye(n)];
          bin = [bin; lb];
        endif
      endif

      if (! isempty (ub))
        if (numel (ub) != n)
          error ("qp: upper bound has incorrect length");
        elseif (isempty (lb))
          Ain = [Ain; -eye(n)];
          bin = [bin; -ub];
        endif
      endif

      if (! isempty (lb) && ! isempty (ub))
        rtol = sqrt (eps);
        for i = 1:n
          if (abs (lb (i) - ub(i)) < rtol*(1 + max (abs (lb(i) + ub(i)))))
            ## These are actually an equality constraint
            tmprow = zeros (1,n);
            tmprow(i) = 1;
            A = [A;tmprow];
            b = [b; 0.5*(lb(i) + ub(i))];
            n_eq = n_eq + 1;
          else
            tmprow = zeros (1,n);
            tmprow(i) = 1;
            Ain = [Ain; tmprow; -tmprow];
            bin = [bin; lb(i); -ub(i)];
            n_in = n_in + 2;
          endif
        endfor
      endif
    endif

    ## Inequality constraints
    if (nargs > 7)
      [dimA_in, n1] = size (A_in);
      if (n1 != n)
        error ("qp: inequality constraint matrix has incorrect column dimension");
      else
        if (! isempty (A_lb))
          if (numel (A_lb) != dimA_in)
            error ("qp: inequality constraint matrix and lower bound vector inconsistent");
          elseif (isempty (A_ub))
            Ain = [Ain; A_in];
            bin = [bin; A_lb];
          endif
        endif
        if (! isempty (A_ub))
          if (numel (A_ub) != dimA_in)
            error ("qp: inequality constraint matrix and upper bound vector inconsistent");
          elseif (isempty (A_lb))
            Ain = [Ain; -A_in];
            bin = [bin; -A_ub];
          endif
        endif

        if (! isempty (A_lb) && ! isempty (A_ub))
          rtol = sqrt (eps);
          for i = 1:dimA_in
            if (abs (A_lb(i) - A_ub(i))
                < rtol*(1 + max (abs (A_lb(i) + A_ub(i)))))
              ## These are actually an equality constraint
              tmprow = A_in(i,:);
              A = [A;tmprow];
              b = [b; 0.5*(A_lb(i) + A_ub(i))];
              n_eq = n_eq + 1;
            else
              tmprow = A_in(i,:);
              Ain = [Ain; tmprow; -tmprow];
              bin = [bin; A_lb(i); -A_ub(i)];
              n_in = n_in + 2;
            endif
          endfor
        endif
      endif
    endif

    ## Now we should have the following QP:
    ##
    ##   min_x  0.5*x'*H*x + x'*q
    ##   s.t.   A*x = b
    ##          Ain*x >= bin

    ## Discard inequality constraints that have -Inf bounds since those
    ## will never be active.
    idx = isinf (bin) & bin < 0;

    bin(idx) = [];
    Ain(idx,:) = [];

    n_in = numel (bin);

    ## Check if the initial guess is feasible.
    if (isa (x0, "single") || isa (H, "single") || isa (q, "single")
        || isa (A, "single") || isa (b, "single"))
      rtol = sqrt (eps ("single"));
    else
      rtol = sqrt (eps);
    endif

    eq_infeasible = (n_eq > 0 && norm (A*x0-b) > rtol*(1+abs (b)));
    in_infeasible = (n_in > 0 && any (Ain*x0-bin < -rtol*(1+abs (bin))));

    info = 0;
    if (eq_infeasible || in_infeasible)
      ## The initial guess is not feasible.
      ## First define xbar that is feasible with respect to the equality
      ## constraints.
      if (eq_infeasible)
        if (rank (A) < n_eq)
          error ("qp: equality constraint matrix must be full row rank");
        endif
        xbar = pinv (A) * b;
      else
        xbar = x0;
      endif

      ## Check if xbar is feasible with respect to the inequality
      ## constraints also.
      if (n_in > 0)
        res = Ain * xbar - bin;
        if (any (res < -rtol * (1 + abs (bin))))
          ## xbar is not feasible with respect to the inequality
          ## constraints.  Compute a step in the null space of the
          ## equality constraints, by solving a QP.  If the slack is
          ## small, we have a feasible initial guess.  Otherwise, the
          ## problem is infeasible.
          if (n_eq > 0)
            Z = null (A);
            if (isempty (Z))
              ## The problem is infeasible because A is square and full
              ## rank, but xbar is not feasible.
              info = 6;
            endif
          endif

          if (info != 6)
            ## Solve an LP with additional slack variables to find
            ## a feasible starting point.
            gamma = eye (n_in);
            if (n_eq > 0)
              Atmp = [Ain*Z, gamma];
              btmp = -res;
            else
              Atmp = [Ain, gamma];
              btmp = bin;
            endif
            ctmp = [zeros(n-n_eq, 1); ones(n_in, 1)];
            lb = [-Inf(n-n_eq,1); zeros(n_in,1)];
            ub = [];
            ctype = repmat ("L", n_in, 1);
            [P, dummy, status] = glpk (ctmp, Atmp, btmp, lb, ub, ctype);
            if ((status == 0)
                && all (abs (P(n-n_eq+1:end)) < rtol * (1 + norm (btmp))))
              ## We found a feasible starting point
              if (n_eq > 0)
                x0 = xbar + Z*P(1:n-n_eq);
              else
                x0 = P(1:n);
              endif
            else
              ## The problem is infeasible
              info = 6;
            endif
          endif
        else
          ## xbar is feasible.  We use it a starting point.
          x0 = xbar;
        endif
      else
        ## xbar is feasible.  We use it a starting point.
        x0 = xbar;
      endif
    endif

    if (info == 0)
      ## The initial (or computed) guess is feasible.
      ## We call the solver.
      [x, lambda, info, iter] = __qp__ (x0, H, q, A, b, Ain, bin, maxit);
    else
      iter = 0;
      x = x0;
      lambda = [];
    endif
    obj = 0.5 * x' * H * x + q' * x;
    INFO.solveiter = iter;
    INFO.info = info;

  else
    print_usage ();
  endif

endfunction


## Test infeasible initial guess (bug #40536)
%!testif HAVE_GLPK
%!
%! H = 1;  q = 0;                # objective: x -> 0.5 x^2
%! A = 1;  lb = 1;  ub = +inf;   # constraint: x >= 1
%! x0 = 0;                       # infeasible initial guess
%!
%! [x, obj_qp, INFO, lambda] = qp (x0, H, q, [], [], [], [], lb, A, ub);
%!
%! assert (isstruct (INFO) && isfield (INFO, "info") && (INFO.info == 0));
%! assert ([x obj_qp], [1.0 0.5], eps);

