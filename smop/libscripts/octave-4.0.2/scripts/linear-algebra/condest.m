## Copyright (C) 2007-2015 Regents of the University of California
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
## @deftypefn  {Function File} {} condest (@var{A})
## @deftypefnx {Function File} {} condest (@var{A}, @var{t})
## @deftypefnx {Function File} {[@var{est}, @var{v}] =} condest (@dots{})
## @deftypefnx {Function File} {[@var{est}, @var{v}] =} condest (@var{A}, @var{solve}, @var{solve_t}, @var{t})
## @deftypefnx {Function File} {[@var{est}, @var{v}] =} condest (@var{apply}, @var{apply_t}, @var{solve}, @var{solve_t}, @var{n}, @var{t})
##
## Estimate the 1-norm condition number of a matrix @var{A} using @var{t} test
## vectors using a randomized 1-norm estimator.
##
## If @var{t} exceeds 5, then only 5 test vectors are used.
##
## If the matrix is not explicit, e.g., when estimating the condition
## number of @var{A} given an LU@tie{}factorization, @code{condest} uses the
## following functions:
##
## @table @var
## @item apply
## @code{A*x} for a matrix @code{x} of size @var{n} by @var{t}.
##
## @item apply_t
## @code{A'*x} for a matrix @code{x} of size @var{n} by @var{t}.
##
## @item solve
## @code{A \ b} for a matrix @code{b} of size @var{n} by @var{t}.
##
## @item solve_t
## @code{A' \ b} for a matrix @code{b} of size @var{n} by @var{t}.
## @end table
##
## The implicit version requires an explicit dimension @var{n}.
##
## @code{condest} uses a randomized algorithm to approximate the 1-norms.
##
## @code{condest} returns the 1-norm condition estimate @var{est} and a vector
## @var{v} satisfying @code{norm (A*v, 1) == norm (A, 1) * norm
## (@var{v}, 1) / @var{est}}.  When @var{est} is large, @var{v} is an
## approximate null vector.
##
## References:
##
## @itemize
## @item
## @nospell{N.J. Higham and F. Tisseur}, @cite{A Block Algorithm
## for Matrix 1-Norm Estimation, with an Application to 1-Norm
## Pseudospectra}. SIMAX vol 21, no 4, pp 1185-1201.
## @url{http://dx.doi.org/10.1137/S0895479899356080}
##
## @item
## @nospell{N.J. Higham and F. Tisseur}, @cite{A Block Algorithm
## for Matrix 1-Norm Estimation, with an Application to 1-Norm
## Pseudospectra}. @url{http://citeseer.ist.psu.edu/223007.html}
## @end itemize
##
## @seealso{cond, norm, onenormest}
## @end deftypefn

## Code originally licensed under:
##
## Copyright (c) 2007, Regents of the University of California
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
##
##    * Redistributions of source code must retain the above copyright
##      notice, this list of conditions and the following disclaimer.
##
##    * Redistributions in binary form must reproduce the above
##      copyright notice, this list of conditions and the following
##      disclaimer in the documentation and/or other materials provided
##      with the distribution.
##
##    * Neither the name of the University of California, Berkeley nor
##      the names of its contributors may be used to endorse or promote
##      products derived from this software without specific prior
##      written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS''
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
## TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
## PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
## USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
## ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
## OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
## SUCH DAMAGE.

## Author: Jason Riedy <ejr@cs.berkeley.edu>
## Keywords: linear-algebra norm estimation
## Version: 0.2

function [est, v] = condest (varargin)

  if (nargin < 1 || nargin > 6)
    print_usage ();
  endif

  default_t = 5;

  have_A = false;
  have_t = false;
  have_solve = false;

  if (isnumeric (varargin{1}))
    A = varargin{1};
    if (! issquare (A))
      error ("condest: matrix must be square");
    endif
    n = rows (A);
    have_A = true;

    if (nargin > 1)
      if (! is_function_handle (varargin{2}))
        t = varargin{2};
        have_t = true;
      elseif (nargin > 2)
        solve = varargin{2};
        solve_t = varargin{3};
        have_solve = true;
        if (nargin > 3)
          t = varargin{4};
          have_t = true;
        endif
      else
        error ("condest: must supply both SOLVE and SOLVE_T");
      endif
    endif
  elseif (nargin > 4)
    apply = varargin{1};
    apply_t = varargin{2};
    solve = varargin{3};
    solve_t = varargin{4};
    have_solve = true;
    n = varargin{5};
    if (! isscalar (n))
      error ("condest: dimension argument of implicit form must be scalar");
    endif
    if (nargin > 5)
      t = varargin{6};
      have_t = true;
    endif
  else
    error ("condest: implicit form of condest requires at least 5 arguments");
  endif

  if (! have_t)
    t = min (n, default_t);
  endif

  if (! have_solve)
    if (issparse (A))
      [L, U, P, Pc] = lu (A);
      solve = @(x) Pc' * (U \ (L \ (P * x)));
      solve_t = @(x) P' * (L' \ (U' \ (Pc * x)));
    else
      [L, U, P] = lu (A);
      solve = @(x) U \ (L \ (P*x));
      solve_t = @(x) P' * (L' \ (U' \ x));
    endif
  endif

  if (have_A)
    Anorm = norm (A, 1);
  else
    Anorm = onenormest (apply, apply_t, n, t);
  endif

  [Ainv_norm, v, w] = onenormest (solve, solve_t, n, t);

  est = Anorm * Ainv_norm;
  v = w / norm (w, 1);

endfunction


%!demo
%! N = 100;
%! A = randn (N) + eye (N);
%! condest (A)
%! [L,U,P] = lu (A);
%! condest (A, @(x) U \ (L \ (P*x)), @(x) P'*(L' \ (U'\x)))
%! condest (@(x) A*x, @(x) A'*x, @(x) U \ (L \ (P*x)), @(x) P'*(L' \ (U'\x)), N)
%! norm (inv (A), 1) * norm (A, 1)

## Yes, these test bounds are really loose.  There's
## enough randomization to trigger odd cases with hilb().

%!test
%! N = 6;
%! A = hilb (N);
%! cA = condest (A);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-8);

%!test
%! N = 6;
%! A = hilb (N);
%! solve = @(x) A\x; solve_t = @(x) A'\x;
%! cA = condest (A, solve, solve_t);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-8);

%!test
%! N = 6;
%! A = hilb (N);
%! apply = @(x) A*x; apply_t = @(x) A'*x;
%! solve = @(x) A\x; solve_t = @(x) A'\x;
%! cA = condest (apply, apply_t, solve, solve_t, N);
%! cA_test = norm (inv (A), 1) * norm (A, 1);
%! assert (cA, cA_test, -2^-6);

%!test
%! warning ("off", "Octave:nearly-singular-matrix", "local");
%! N = 12;
%! A = hilb (N);
%! [rcondA, v] = condest (A);
%! x = A*v;
%! assert (norm (x, inf), 0, eps);

