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
## @deftypefn  {Function File} {[@var{est}, @var{v}, @var{w}, @var{iter}] =} onenormest (@var{A}, @var{t})
## @deftypefnx {Function File} {[@var{est}, @var{v}, @var{w}, @var{iter}] =} onenormest (@var{apply}, @var{apply_t}, @var{n}, @var{t})
##
## Apply @nospell{Higham and Tisseur's} randomized block 1-norm estimator to
## matrix @var{A} using @var{t} test vectors.
##
## If @var{t} exceeds 5, then only 5 test vectors are used.
##
## If the matrix is not explicit, e.g., when estimating the norm of
## @code{inv (@var{A})} given an LU@tie{}factorization, @code{onenormest}
## applies @var{A} and its conjugate transpose through a pair of functions
## @var{apply} and @var{apply_t}, respectively, to a dense matrix of size
## @var{n} by @var{t}.  The implicit version requires an explicit dimension
## @var{n}.
##
## Returns the norm estimate @var{est}, two vectors @var{v} and @var{w} related
## by norm @code{(@var{w}, 1) = @var{est} * norm (@var{v}, 1)}, and the number
## of iterations @var{iter}.  The number of iterations is limited to 10 and is
## at least 2.
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
## @seealso{condest, norm, cond}
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

function [est, v, w, iter] = onenormest (varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  default_t = 5;
  itmax = 10;

  if (isnumeric (varargin{1}))
    [n, nc] = size (varargin{1});
    if (n != nc)
      error ("onenormest: matrix must be square");
    endif
    apply = @(x) varargin{1} * x;
    apply_t = @(x) varargin{1}' * x;
    if (nargin > 1)
      t = varargin{2};
    else
      t = min (n, default_t);
    endif
    issing = isa (varargin{1}, "single");
  else
    if (nargin < 3)
      print_usage ();
    endif
    apply = varargin{1};
    apply_t = varargin{2};
    n = varargin{3};
    if (nargin > 3)
      t = varargin{4};
    else
      t = default_t;
    endif
    issing = isa (n, "single");
  endif

  ## Initial test vectors X.
  X = rand (n, t);
  X = X ./ (ones (n,1) * sum (abs (X), 1));

  ## Track if a vertex has been visited.
  been_there = zeros (n, 1);

  ## To check if the estimate has increased.
  est_old = 0;

  ## Normalized vector of signs.
  S = zeros (n, t);

  if (issing)
    myeps = eps ("single");
    X = single (X);
  else
    myeps = eps;
  endif

  for iter = 1 : itmax + 1
    Y = feval (apply, X);

    ## Find the initial estimate as the largest A*x.
    [est, ind_best] = max (sum (abs (Y), 1));
    if (est > est_old || iter == 2)
      w = Y(:,ind_best);
    endif
    if (iter >= 2 && est < est_old)
      ## No improvement, so stop.
      est = est_old;
      break;
    endif

    est_old = est;
    S_old = S;
    if (iter > itmax),
      ## Gone too far.  Stop.
      break;
    endif

    S = sign (Y);

    ## Test if any of S are approximately parallel to previous S
    ## vectors or current S vectors.  If everything is parallel,
    ## stop.  Otherwise, replace any parallel vectors with
    ## rand{-1,+1}.
    partest = any (abs (S_old' * S - n) < 4*eps*n);
    if (all (partest))
      ## All the current vectors are parallel to old vectors.
      ## We've hit a cycle, so stop.
      break;
    endif
    if (any (partest))
      ## Some vectors are parallel to old ones and are cycling,
      ## but not all of them.  Replace the parallel vectors with
      ## rand{-1,+1}.
      numpar = sum (partest);
      replacements = 2*(rand (n,numpar) < 0.5) - 1;
      S(:,partest) = replacements;
    endif
    ## Now test for parallel vectors within S.
    partest = any ((S' * S - eye (t)) == n);
    if (any (partest))
      numpar = sum (partest);
      replacements = 2*(rand (n,numpar) < 0.5) - 1;
      S(:,partest) = replacements;
    endif

    Z = feval (apply_t, S);

    ## Now find the largest non-previously-visted index per vector.
    h = max (abs (Z),2);
    [mh, mhi] = max (h);
    if (iter >= 2 && mhi == ind_best)
      ## Hit a cycle, stop.
      break;
    endif
    [h, ind] = sort (h, 'descend');
    if (t > 1)
      firstind = ind(1:t);
      if (all (been_there(firstind)))
        ## Visited all these before, so stop.
        break;
      endif
      ind = ind(! been_there(ind));
      if (length (ind) < t)
        ## There aren't enough new vectors, so we're practically
        ## in a cycle.  Stop.
        break;
      endif
    endif

    ## Visit the new indices.
    X = zeros (n, t);
    for zz = 1 : t
      X(ind(zz),zz) = 1;
    endfor
    been_there(ind(1 : t)) = 1;
  endfor

  ## The estimate est and vector w are set in the loop above.
  ## The vector v selects the ind_best column of A.
  v = zeros (n, 1);
  v(ind_best) = 1;

endfunction


%!demo
%! N = 100;
%! A = randn (N) + eye (N);
%! [L,U,P] = lu (A);
%! nm1inv = onenormest (@(x) U\(L\(P*x)), @(x) P'*(L'\(U'\x)), N, 30)
%! norm (inv (A), 1)

%!test
%! N = 10;
%! A = ones (N);
%! [nm1, v1, w1] = onenormest (A);
%! [nminf, vinf, winf] = onenormest (A', 6);
%! assert (nm1, N, -2*eps);
%! assert (nminf, N, -2*eps);
%! assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps);
%! assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps);

%!test
%! N = 10;
%! A = ones (N);
%! [nm1, v1, w1] = onenormest (@(x) A*x, @(x) A'*x, N, 3);
%! [nminf, vinf, winf] = onenormest (@(x) A'*x, @(x) A*x, N, 3);
%! assert (nm1, N, -2*eps);
%! assert (nminf, N, -2*eps);
%! assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps);
%! assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps);

%!test
%! N = 5;
%! A = hilb (N);
%! [nm1, v1, w1] = onenormest (A);
%! [nminf, vinf, winf] = onenormest (A', 6);
%! assert (nm1, norm (A, 1), -2*eps);
%! assert (nminf, norm (A, inf), -2*eps);
%! assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps);
%! assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps);

## Only likely to be within a factor of 10.
%!test
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ('state', 42);  % Initialize to guarantee reproducible results
%! N = 100;
%! A = rand (N);
%! [nm1, v1, w1] = onenormest (A);
%! [nminf, vinf, winf] = onenormest (A', 6);
%! assert (nm1, norm (A, 1), -.1);
%! assert (nminf, norm (A, inf), -.1);
%! assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps);
%! assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps);

