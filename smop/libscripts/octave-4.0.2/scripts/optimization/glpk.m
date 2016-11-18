## Copyright (C) 2005-2015 Nicolo' Giorgetti
## Copyright (C) 2013-2015 Sébastien Villemot <sebastien@debian.org>
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
## @deftypefn {Function File} {[@var{xopt}, @var{fmin}, @var{errnum}, @var{extra}] =} glpk (@var{c}, @var{A}, @var{b}, @var{lb}, @var{ub}, @var{ctype}, @var{vartype}, @var{sense}, @var{param})
## Solve a linear program using the GNU @sc{glpk} library.
##
## Given three arguments, @code{glpk} solves the following standard LP:
## @tex
## $$
##   \min_x C^T x
## $$
## @end tex
## @ifnottex
##
## @example
## min C'*x
## @end example
##
## @end ifnottex
## subject to
## @tex
## $$
##   Ax = b \qquad x \geq 0
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## A*x  = b
##   x >= 0
## @end group
## @end example
##
## @end ifnottex
## but may also solve problems of the form
## @tex
## $$
##   [ \min_x | \max_x ] C^T x
## $$
## @end tex
## @ifnottex
##
## @example
## [ min | max ] C'*x
## @end example
##
## @end ifnottex
## subject to
## @tex
## $$
##  Ax [ = | \leq | \geq ] b \qquad LB \leq x \leq UB
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## A*x [ "=" | "<=" | ">=" ] b
##   x >= LB
##   x <= UB
## @end group
## @end example
##
## @end ifnottex
##
## Input arguments:
##
## @table @var
## @item c
## A column array containing the objective function coefficients.
##
## @item A
## A matrix containing the constraints coefficients.
##
## @item b
## A column array containing the right-hand side value for each constraint in
## the constraint matrix.
##
## @item lb
## An array containing the lower bound on each of the variables.  If @var{lb}
## is not supplied, the default lower bound for the variables is zero.
##
## @item ub
## An array containing the upper bound on each of the variables.  If @var{ub}
## is not supplied, the default upper bound is assumed to be infinite.
##
## @item ctype
## An array of characters containing the sense of each constraint in the
## constraint matrix.  Each element of the array may be one of the following
## values
##
## @table @asis
## @item @qcode{"F"}
## A free (unbounded) constraint (the constraint is ignored).
##
## @item @qcode{"U"}
## An inequality constraint with an upper bound (@code{A(i,:)*x <= b(i)}).
##
## @item @qcode{"S"}
## An equality constraint (@code{A(i,:)*x = b(i)}).
##
## @item @qcode{"L"}
## An inequality with a lower bound (@code{A(i,:)*x >= b(i)}).
##
## @item @qcode{"D"}
## An inequality constraint with both upper and lower bounds
## (@code{A(i,:)*x >= -b(i)}) @emph{and} (@code{A(i,:)*x <= b(i)}).
## @end table
##
## @item vartype
## A column array containing the types of the variables.
##
## @table @asis
## @item @qcode{"C"}
## A continuous variable.
##
## @item @qcode{"I"}
## An integer variable.
## @end table
##
## @item sense
## If @var{sense} is 1, the problem is a minimization.  If @var{sense} is -1,
## the problem is a maximization.  The default value is 1.
##
## @item param
## A structure containing the following parameters used to define the
## behavior of solver.  Missing elements in the structure take on default
## values, so you only need to set the elements that you wish to change from
## the default.
##
## Integer parameters:
##
## @table @code
## @item msglev (default: 1)
## Level of messages output by solver routines:
##
## @table @asis
## @item 0 (@w{@code{GLP_MSG_OFF}})
## No output.
##
## @item 1 (@w{@code{GLP_MSG_ERR}})
## Error and warning messages only.
##
## @item 2 (@w{@code{GLP_MSG_ON}})
## Normal output.
##
## @item 3 (@w{@code{GLP_MSG_ALL}})
## Full output (includes informational messages).
## @end table
##
## @item scale (default: 16)
## Scaling option.  The values can be combined with the bitwise OR operator and
## may be the following:
##
## @table @asis
## @item 1 (@w{@code{GLP_SF_GM}})
## Geometric mean scaling.
##
## @item 16 (@w{@code{GLP_SF_EQ}})
## Equilibration scaling.
##
## @item 32 (@w{@code{GLP_SF_2N}})
## Round scale factors to power of two.
##
## @item 64 (@w{@code{GLP_SF_SKIP}})
## Skip if problem is well scaled.
## @end table
##
## Alternatively, a value of 128 (@w{@env{GLP_SF_AUTO}}) may be also
## specified, in which case the routine chooses the scaling options
## automatically.
##
## @item dual (default: 1)
## Simplex method option:
##
## @table @asis
## @item 1 (@w{@code{GLP_PRIMAL}})
## Use two-phase primal simplex.
##
## @item 2 (@w{@code{GLP_DUALP}})
## Use two-phase dual simplex, and if it fails, switch to the primal simplex.
##
## @item 3 (@w{@code{GLP_DUAL}})
## Use two-phase dual simplex.
## @end table
##
## @item price (default: 34)
## Pricing option (for both primal and dual simplex):
##
## @table @asis
## @item 17 (@w{@code{GLP_PT_STD}})
## Textbook pricing.
##
## @item 34 (@w{@code{GLP_PT_PSE}})
## Steepest edge pricing.
## @end table
##
## @item itlim (default: intmax)
## Simplex iterations limit.  It is decreased by one each time when one simplex
## iteration has been performed, and reaching zero value signals the solver to
## stop the search.
##
## @item outfrq (default: 200)
## Output frequency, in iterations.  This parameter specifies how frequently
## the solver sends information about the solution to the standard output.
##
## @item branch (default: 4)
## Branching technique option (for MIP only):
##
## @table @asis
## @item 1 (@w{@code{GLP_BR_FFV}})
## First fractional variable.
##
## @item 2 (@w{@code{GLP_BR_LFV}})
## Last fractional variable.
##
## @item 3 (@w{@code{GLP_BR_MFV}})
## Most fractional variable.
##
## @item 4 (@w{@code{GLP_BR_DTH}})
## Heuristic by @nospell{Driebeck and Tomlin}.
##
## @item 5 (@w{@code{GLP_BR_PCH}})
## Hybrid @nospell{pseudocost} heuristic.
## @end table
##
## @item btrack (default: 4)
## Backtracking technique option (for MIP only):
##
## @table @asis
## @item 1 (@w{@code{GLP_BT_DFS}})
## Depth first search.
##
## @item 2 (@w{@code{GLP_BT_BFS}})
## Breadth first search.
##
## @item 3 (@w{@code{GLP_BT_BLB}})
## Best local bound.
##
## @item 4 (@w{@code{GLP_BT_BPH}})
## Best projection heuristic.
## @end table
##
## @item presol (default: 1)
## If this flag is set, the simplex solver uses the built-in LP presolver.
## Otherwise the LP presolver is not used.
##
## @item lpsolver (default: 1)
## Select which solver to use.  If the problem is a MIP problem this flag
## will be ignored.
##
## @table @asis
## @item 1
## Revised simplex method.
##
## @item 2
## Interior point method.
## @end table
##
## @item rtest (default: 34)
## Ratio test technique:
##
## @table @asis
## @item 17 (@w{@code{GLP_RT_STD}})
## Standard ("textbook").
##
## @item 34 (@w{@code{GLP_RT_HAR}})
## Harris' two-pass ratio test.
## @end table
##
## @item tmlim (default: intmax)
## Searching time limit, in milliseconds.
##
## @item outdly (default: 0)
## Output delay, in seconds.  This parameter specifies how long the solver
## should delay sending information about the solution to the standard output.
##
## @item save (default: 0)
## If this parameter is nonzero, save a copy of the problem in CPLEX LP
## format to the file @file{"outpb.lp"}.  There is currently no way to change
## the name of the output file.
## @end table
##
## Real parameters:
##
## @table @code
## @item tolbnd (default: 1e-7)
## Relative tolerance used to check if the current basic solution is primal
## feasible.  It is not recommended that you change this parameter unless you
## have a detailed understanding of its purpose.
##
## @item toldj (default: 1e-7)
## Absolute tolerance used to check if the current basic solution is dual
## feasible.  It is not recommended that you change this parameter unless you
## have a detailed understanding of its purpose.
##
## @item tolpiv (default: 1e-10)
## Relative tolerance used to choose eligible pivotal elements of the simplex
## table.  It is not recommended that you change this parameter unless you have
## a detailed understanding of its purpose.
##
## @item objll (default: -DBL_MAX)
## Lower limit of the objective function.  If the objective function reaches
## this limit and continues decreasing, the solver stops the search.  This
## parameter is used in the dual simplex method only.
##
## @item objul (default: +DBL_MAX)
## Upper limit of the objective function.  If the objective function reaches
## this limit and continues increasing, the solver stops the search.  This
## parameter is used in the dual simplex only.
##
## @item tolint (default: 1e-5)
## Relative tolerance used to check if the current basic solution is integer
## feasible.  It is not recommended that you change this parameter unless you
## have a detailed understanding of its purpose.
##
## @item tolobj (default: 1e-7)
## Relative tolerance used to check if the value of the objective function is
## not better than in the best known integer feasible solution.  It is not
## recommended that you change this parameter unless you have a detailed
## understanding of its purpose.
## @end table
## @end table
##
## Output values:
##
## @table @var
## @item xopt
## The optimizer (the value of the decision variables at the optimum).
##
## @item fopt
## The optimum value of the objective function.
##
## @item errnum
## Error code.
##
## @table @asis
## @item 0
## No error.
##
## @item 1 (@w{@code{GLP_EBADB}})
## Invalid basis.
##
## @item 2 (@w{@code{GLP_ESING}})
## Singular matrix.
##
## @item 3 (@w{@code{GLP_ECOND}})
## Ill-conditioned matrix.
##
## @item 4 (@w{@code{GLP_EBOUND}})
## Invalid bounds.
##
## @item 5 (@w{@code{GLP_EFAIL}})
## Solver failed.
##
## @item 6 (@w{@code{GLP_EOBJLL}})
## Objective function lower limit reached.
##
## @item 7 (@w{@code{GLP_EOBJUL}})
## Objective function upper limit reached.
##
## @item 8 (@w{@code{GLP_EITLIM}})
## Iterations limit exhausted.
##
## @item 9 (@w{@code{GLP_ETMLIM}})
## Time limit exhausted.
##
## @item 10 (@w{@code{GLP_ENOPFS}})
## No primal feasible solution.
##
## @item 11 (@w{@code{GLP_ENODFS}})
## No dual feasible solution.
##
## @item 12 (@w{@code{GLP_EROOT}})
## Root LP optimum not provided.
##
## @item 13 (@w{@code{GLP_ESTOP}})
## Search terminated by application.
##
## @item 14 (@w{@code{GLP_EMIPGAP}})
## Relative MIP gap tolerance reached.
##
## @item 15 (@w{@code{GLP_ENOFEAS}})
## No primal/dual feasible solution.
##
## @item 16 (@w{@code{GLP_ENOCVG}})
## No convergence.
##
## @item 17 (@w{@code{GLP_EINSTAB}})
## Numerical instability.
##
## @item 18 (@w{@code{GLP_EDATA}})
## Invalid data.
##
## @item 19 (@w{@code{GLP_ERANGE}})
## Result out of range.
## @end table
##
## @item extra
## A data structure containing the following fields:
##
## @table @code
## @item lambda
## Dual variables.
##
## @item redcosts
## Reduced Costs.
##
## @item time
## Time (in seconds) used for solving LP/MIP problem.
##
## @item status
## Status of the optimization.
##
## @table @asis
## @item 1 (@w{@code{GLP_UNDEF}})
## Solution status is undefined.
##
## @item 2 (@w{@code{GLP_FEAS}})
## Solution is feasible.
##
## @item 3 (@w{@code{GLP_INFEAS}})
## Solution is infeasible.
##
## @item 4 (@w{@code{GLP_NOFEAS}})
## Problem has no feasible solution.
##
## @item 5 (@w{@code{GLP_OPT}})
## Solution is optimal.
##
## @item 6 (@w{@code{GLP_UNBND}})
## Problem has no unbounded solution.
## @end table
## @end table
## @end table
##
## Example:
##
## @example
## @group
## c = [10, 6, 4]';
## A = [ 1, 1, 1;
##      10, 4, 5;
##       2, 2, 6];
## b = [100, 600, 300]';
## lb = [0, 0, 0]';
## ub = [];
## ctype = "UUU";
## vartype = "CCC";
## s = -1;
##
## param.msglev = 1;
## param.itlim = 100;
##
## [xmin, fmin, status, extra] = ...
##    glpk (c, A, b, lb, ub, ctype, vartype, s, param);
## @end group
## @end example
## @end deftypefn

## Author: Nicolo' Giorgetti <giorgetti@dii.unisi.it>
## Adapted-by: jwe

function [xopt, fmin, errnum, extra] = glpk (c, A, b, lb, ub, ctype, vartype, sense, param)

  ## If there is no input output the version and syntax
  if (nargin < 3 || nargin > 9)
    print_usage ();
    return;
  endif

  if (all (size (c) > 1) || iscomplex (c) || ischar (c))
    error ("glpk:C must be a real vector");
    return;
  endif
  nx = length (c);
  ## Force column vector.
  c = c(:);

  ## 2) Matrix constraint

  if (isempty (A))
    error ("glpk: A cannot be an empty matrix");
    return;
  endif
  [nc, nxa] = size (A);
  if (! isreal (A) || nxa != nx)
    error ("glpk: A must be a real valued %d by %d matrix", nc, nx);
    return;
  endif

  ## 3) RHS

  if (isempty (b))
    error ("glpk: B cannot be an empty vector");
    return;
  endif
  if (! isreal (b) || length (b) != nc)
    error ("glpk: B must be a real valued %d by 1 vector", nc);
    return;
  endif

  ## 4) Vector with the lower bound of each variable

  if (nargin > 3)
    if (isempty (lb))
      lb = zeros (nx, 1);
    elseif (! isreal (lb) || all (size (lb) > 1) || length (lb) != nx)
      error ("glpk: LB must be a real valued %d by 1 column vector", nx);
      return;
    endif
  else
    lb = zeros (nx, 1);
  endif

  ## 5) Vector with the upper bound of each variable

  if (nargin > 4)
    if (isempty (ub))
      ub = Inf (nx, 1);
    elseif (! isreal (ub) || all (size (ub) > 1) || length (ub) != nx)
      error ("glpk: UB must be a real valued %d by 1 column vector", nx);
      return;
    endif
  else
    ub = Inf (nx, 1);
  endif

  ## 6) Sense of each constraint

  if (nargin > 5)
    if (isempty (ctype))
      ctype = repmat ("S", nc, 1);
    elseif (! ischar (ctype) || all (size (ctype) > 1) || length (ctype) != nc)
      error ("glpk: CTYPE must be a char valued vector of length %d", nc);
      return;
    elseif (! all (ctype == "F" | ctype == "U" | ctype == "S"
                   | ctype == "L" | ctype == "D"))
      error ("glpk: CTYPE must contain only F, U, S, L, or D");
      return;
    endif
  else
    ctype = repmat ("S", nc, 1);
  endif

  ## 7) Vector with the type of variables

  if (nargin > 6)
    if (isempty (vartype))
      vartype = repmat ("C", nx, 1);
    elseif (! ischar (vartype) || all (size (vartype) > 1)
            || length (vartype) != nx)
      error ("glpk: VARTYPE must be a char valued vector of length %d", nx);
      return;
    elseif (! all (vartype == "C" | vartype == "I"))
      error ("glpk: VARTYPE must contain only C or I");
      return;
    endif
  else
    ## As default we consider continuous vars
    vartype = repmat ("C", nx, 1);
  endif

  ## 8) Sense of optimization

  if (nargin > 7)
    if (isempty (sense))
      sense = 1;
    elseif (ischar (sense) || all (size (sense) > 1) || ! isreal (sense))
      error ("glpk: SENSE must be an integer value");
    elseif (sense >= 0)
      sense = 1;
    else
      sense = -1;
    endif
  else
    sense = 1;
  endif

  ## 9) Parameters vector

  if (nargin > 8)
    if (! isstruct (param))
      error ("glpk: PARAM must be a structure");
      return;
    endif
  else
    param = struct ();
  endif

  [xopt, fmin, errnum, extra] = ...
    __glpk__ (c, A, b, lb, ub, ctype, vartype, sense, param);

endfunction

