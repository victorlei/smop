## Copyright (C) 1993-2015 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{u}, @var{h}, @var{nu}] =} krylov (@var{A}, @var{V}, @var{k}, @var{eps1}, @var{pflg})
## Construct an orthogonal basis @var{u} of block Krylov subspace
##
## @example
## [v a*v a^2*v @dots{} a^(k+1)*v]
## @end example
##
## @noindent
## using Householder reflections to guard against loss of orthogonality.
##
## If @var{V} is a vector, then @var{h} contains the Hessenberg matrix
## such that @nospell{@tcode{a*u == u*h+rk*ek'}}, in which
## @code{rk = a*u(:,k)-u*h(:,k)}, and @nospell{@tcode{ek'}} is the vector
## @code{[0, 0, @dots{}, 1]} of length @code{k}.  Otherwise, @var{h} is
## meaningless.
##
## If @var{V} is a vector and @var{k} is greater than @code{length (A) - 1},
## then @var{h} contains the Hessenberg matrix such that @code{a*u == u*h}.
##
## The value of @var{nu} is the dimension of the span of the Krylov subspace
## (based on @var{eps1}).
##
## If @var{b} is a vector and @var{k} is greater than @var{m-1}, then @var{h}
## contains the Hessenberg decomposition of @var{A}.
##
## The optional parameter @var{eps1} is the threshold for zero.  The default
## value is 1e-12.
##
## If the optional parameter @var{pflg} is nonzero, row pivoting is used to
## improve numerical behavior.  The default value is 0.
##
## Reference: @nospell{A. Hodel, P. Misra}, @cite{Partial Pivoting in the
## Computation of Krylov Subspaces of Large Sparse Systems}, Proceedings of
## the 42nd IEEE Conference on Decision and Control, December 2003.
## @end deftypefn

## Author: A. Scottedward Hodel <a.s.hodel@eng.auburn.edu>

function [Uret, H, nu] = krylov (A, V, k, eps1, pflg);

  if (isa (A, "single") || isa (V, "single"))
    defeps = 1e-6;
  else
    defeps = 1e-12;
  endif

  if (nargin < 3 || nargin > 5)
    print_usage ();
  elseif (nargin < 5)
    ## Default permutation flag.
    pflg = 0;
  endif

  if (nargin < 4)
    ## Default tolerance parameter.
    eps1 = defeps;
  endif

  if (isempty (eps1))
    eps1 = defeps;
  endif

  if (! issquare (A) || isempty (A))
    error ("krylov: A(%d x %d) must be a non-empty square matrix", rows (A), columns (A));
  endif
  na = rows (A);

  [m, kb] = size (V);
  if (m != na)
    error ("krylov: A(%d x %d), V(%d x %d): argument dimensions do not match",
          na, na, m, kb);
  endif

  if (! isscalar (k))
    error ("krylov: K must be a scalar integer");
  endif

  Vnrm = norm (V, Inf);

  ## check for trivial solution.
  if (Vnrm == 0)
    Uret = [];
    H = [];
    nu = 0;
    return;
  endif

  ## Identify trivial null space.
  abm = max (abs ([A, V]'));
  zidx = find (abm == 0);

  ## Set up vector of pivot points.
  pivot_vec = 1:na;

  iter = 0;
  alpha = [];
  nh = 0;
  while (length (alpha) < na) && (columns (V) > 0) && (iter < k)
    iter++;

    ## Get orthogonal basis of V.
    jj = 1;
    while (jj <= columns (V) && length (alpha) < na)
      ## Index of next Householder reflection.
      nu = length (alpha)+1;

      short_pv = pivot_vec(nu:na);
      q = V(:,jj);
      short_q = q(short_pv);

      if (norm (short_q) < eps1)
        ## Insignificant column; delete.
        nv = columns (V);
        if (jj != nv)
          [V(:,jj), V(:,nv)] = swap (V(:,jj), V(:,nv));
          ## FIXME: H columns should be swapped too.
          ##        Not done since Block Hessenberg structure is lost anyway.
        endif
        V = V(:,1:(nv-1));
        ## One less reflection.
        nu--;
      else
        ## New householder reflection.
        if (pflg)
          ## Locate max magnitude element in short_q.
          asq = abs (short_q);
          maxv = max (asq);
          maxidx = find (asq == maxv, 1);
          pivot_idx = short_pv(maxidx);

          ## See if need to change the pivot list.
          if (pivot_idx != pivot_vec(nu))
            swapidx = maxidx + (nu-1);
            [pivot_vec(nu), pivot_vec(swapidx)] = ...
                swap (pivot_vec(nu), pivot_vec(swapidx));
          endif
        endif

        ## Isolate portion of vector for reflection.
        idx = pivot_vec(nu:na);
        jdx = pivot_vec(1:nu);

        [hv, av, z] = housh (q(idx), 1, 0);
        alpha(nu) = av;
        U(idx,nu) = hv;

        ## Reduce V per the reflection.
        V(idx,:) = V(idx,:) - av*hv*(hv' * V(idx,:));
        if(iter > 1)
          ## FIXME: not done correctly for block case.
          H(nu,nu-1) = V(pivot_vec(nu),jj);
        endif

        ## Advance to next column of V.
        jj++;
      endif
    endwhile

    ## Check for oversize V (due to full rank).
    if ((columns (V) > na) && (length (alpha) == na))
      ## Trim to size.
      V = V(:,1:na);
    elseif (columns(V) > na)
      krylov_V = V;
      krylov_na = na;
      krylov_length_alpha = length (alpha);
      error ("krylov: this case should never happen; submit a bug report");
    endif

    if (columns (V) > 0)
      ## Construct next Q and multiply.
      Q = zeros (size (V));
      for kk = 1:columns (Q)
        Q(pivot_vec(nu-columns(Q)+kk),kk) = 1;
      endfor

      ## Apply Householder reflections.
      for ii = nu:-1:1
        idx = pivot_vec(ii:na);
        hv = U(idx,ii);
        av = alpha(ii);
        Q(idx,:) = Q(idx,:) - av*hv*(hv'*Q(idx,:));
      endfor
    endif

    ## Multiply to get new vector.
    V = A*Q;
    ## Project off of previous vectors.
    nu = length (alpha);
    for i = 1:nu
      hv = U(:,i);
      av = alpha(i);
      V = V - av*hv*(hv'*V);
      H(i,nu-columns(V)+(1:columns(V))) = V(pivot_vec(i),:);
    endfor

  endwhile

  ## Back out complete U matrix.
  ## back out U matrix.
  j1 = columns (U);
  for i = j1:-1:1;
    idx = pivot_vec(i:na);
    hv = U(idx,i);
    av = alpha(i);
    U(:,i) = zeros (na, 1);
    U(idx(1),i) = 1;
    U(idx,i:j1) = U(idx,i:j1)-av*hv*(hv'*U(idx,i:j1));
  endfor

  nu = length (alpha);
  Uret = U;
  if (max (max (abs (Uret(zidx,:)))) > 0)
    warning ("krylov: trivial null space corrupted; set pflg = 1 or eps1 > %e",
             eps1);
  endif

endfunction


function [a1, b1] = swap (a, b)

  a1 = b;
  b1 = a;

endfunction

