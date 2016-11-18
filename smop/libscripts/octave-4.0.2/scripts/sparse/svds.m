## Copyright (C) 2006-2015 David Bateman
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
## @deftypefn  {Function File} {@var{s} =} svds (@var{A})
## @deftypefnx {Function File} {@var{s} =} svds (@var{A}, @var{k})
## @deftypefnx {Function File} {@var{s} =} svds (@var{A}, @var{k}, @var{sigma})
## @deftypefnx {Function File} {@var{s} =} svds (@var{A}, @var{k}, @var{sigma}, @var{opts})
## @deftypefnx {Function File} {[@var{u}, @var{s}, @var{v}] =} svds (@dots{})
## @deftypefnx {Function File} {[@var{u}, @var{s}, @var{v}, @var{flag}] =} svds (@dots{})
##
## Find a few singular values of the matrix @var{A}.
##
## The singular values are calculated using
##
## @example
## @group
## [@var{m}, @var{n}] = size (@var{A});
## @var{s} = eigs ([sparse(@var{m}, @var{m}), @var{A};
##                      @var{A}', sparse(@var{n}, @var{n})])
## @end group
## @end example
##
## The eigenvalues returned by @code{eigs} correspond to the singular values
## of @var{A}.  The number of singular values to calculate is given by @var{k}
## and defaults to 6.
##
## The argument @var{sigma} specifies which singular values to find.  When
## @var{sigma} is the string @qcode{'L'}, the default, the largest singular
## values of @var{A} are found.  Otherwise, @var{sigma} must be a real scalar
## and the singular values closest to @var{sigma} are found.  As a corollary,
## @code{@var{sigma} = 0} finds the smallest singular values.  Note that for
## relatively small values of @var{sigma}, there is a chance that the
## requested number of singular values will not be found.  In that case
## @var{sigma} should be increased.
##
## @var{opts} is a structure defining options that @code{svds} will pass
## to @code{eigs}.  The possible fields of this structure are documented in
## @code{eigs}.  By default, @code{svds} sets the following three fields:
##
## @table @code
## @item tol
## The required convergence tolerance for the singular values.  The default
## value is 1e-10.  @code{eigs} is passed @code{@var{tol} / sqrt(2)}.
##
## @item maxit
## The maximum number of iterations.  The default is 300.
##
## @item disp
## The level of diagnostic printout (0|1|2).  If @code{disp} is 0 then
## diagnostics are disabled.  The default value is 0.
## @end table
##
## If more than one output is requested then @code{svds} will return an
## approximation of the singular value decomposition of @var{A}
##
## @example
## @var{A}_approx = @var{u}*@var{s}*@var{v}'
## @end example
##
## @noindent
## where @var{A}_approx is a matrix of size @var{A} but only rank @var{k}.
##
## @var{flag} returns 0 if the algorithm has succesfully converged, and 1
## otherwise.  The test for convergence is
##
## @example
## @group
## norm (@var{A}*@var{v} - @var{u}*@var{s}, 1) <= @var{tol} * norm (@var{A}, 1)
## @end group
## @end example
##
## @code{svds} is best for finding only a few singular values from a large
## sparse matrix.  Otherwise, @code{svd (full (@var{A}))} will likely be more
## efficient.
## @end deftypefn
## @seealso{svd, eigs}

function [u, s, v, flag] = svds (A, k, sigma, opts)

  persistent root2 = sqrt (2);

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  if (ndims (A) > 2)
    error ("svds: A must be a 2-D matrix");
  endif

  if (nargin < 4)
    opts.tol = 0;    # use ARPACK default
    opts.disp = 0;
    opts.maxit = 300;
  else
    if (! isstruct (opts))
      error ("svds: OPTS must be a structure");
    endif
    if (! isfield (opts, "tol"))
      opts.tol = 0;  # use ARPACK default
    else
      opts.tol = opts.tol / root2;
    endif
    if (isfield (opts, "v0"))
      if (! isvector (opts.v0) || (length (opts.v0) != sum (size (A))))
        error ("svds: OPTS.v0 must be a vector with rows(A)+columns(A) entries");
      endif
    endif
  endif

  if (nargin < 3 || strcmp (sigma, "L"))
    if (isreal (A))
      sigma = "LA";
    else
      sigma = "LR";
    endif
  elseif (isscalar (sigma) && isnumeric (sigma) && isreal (sigma))
    if (sigma < 0)
      error ("svds: SIGMA must be a positive real value");
    endif
  else
    error ("svds: SIGMA must be a positive real value or the string 'L'");
  endif

  [m, n] = size (A);
  max_a = max (abs (nonzeros (A)));
  if (isempty (max_a))
    max_a = 0;
  endif
  ## Must initialize variable value, otherwise it may appear to interpreter
  ## that code is trying to call flag() colormap function.
  flag = 0;

  if (max_a == 0)
    s = zeros (k, 1);  # special case of zero matrix
  else
    if (nargin < 2)
      k = min ([6, m, n]);
    else
      k = min ([k, m, n]);
    endif

    ## Scale everything by the 1-norm to make things more stable.
    b = A / max_a;
    b_opts = opts;
    ## Call to eigs is always a symmetric matrix by construction
    b_opts.issym = true;
    b_sigma = sigma;
    if (! ischar (b_sigma))
      b_sigma = b_sigma / max_a;
    endif

    if (b_sigma == 0)
      ## Find the smallest eigenvalues
      ## The eigenvalues returns by eigs for sigma=0 are symmetric about 0.
      ## As we are only interested in the positive eigenvalues, we have to
      ## double k and then throw out the k negative eigenvalues.
      ## Separately, if sigma is nonzero, but smaller than the smallest
      ## singular value, ARPACK may not return k eigenvalues. However, as
      ## computation scales with k we'd like to avoid doubling k for all
      ## scalar values of sigma.
      b_k = 2 * k;
    else
      b_k = k;  # Normal case, find just the k largest eigenvalues
    endif

    if (nargout > 1)
      [V, s, flag] = eigs ([sparse(m,m), b; b', sparse(n,n)],
                           b_k, b_sigma, b_opts);
      s = diag (s);
    else
      s = eigs ([sparse(m,m), b; b', sparse(n,n)], b_k, b_sigma, b_opts);
    endif

    if (ischar (sigma))
      norma = max (s);
    else
      norma = normest (A);
    endif
    ## We wish to exclude all eigenvalues that are less than zero as these
    ## are artifacts of the way the matrix passed to eigs is formed. There
    ## is also the possibility that the value of sigma chosen is exactly
    ## a singular value, and in that case we're dead!! So have to rely on
    ## the warning from eigs. We exclude the singular values which are
    ## less than or equal to zero to within some tolerance scaled by the
    ## norm since if we don't we might end up with too many singular
    ## values.
    if (b_sigma == 0)
      if (sum (s>0) < k)
        ## It may happen that the number of positive s is less than k.
        ## In this case, take -s (if s in an eigenvalue, so is -s),
        ## flipped upside-down.
        s = flipud (-s);
      endif
    endif
    tol = norma * opts.tol;
    ind = find (s > tol);
    if (length (ind) < k)
      ## Too few eigenvalues returned.  Add in any zero eigenvalues of B,
      ## including the nominally negative ones.
      zind = find (abs (s) <= tol);
      p = min (length (zind), k - length (ind));
      ind = [ind; zind(1:p)];
    elseif (length (ind) > k)
      ## Too many eigenvalues returned.  Select according to criterium.
      if (b_sigma == 0)
        ind = ind(end+1-k:end); # smallest eigenvalues
      else
        ind = ind(1:k);         # largest eigenvalues
      endif
    endif
    s = s(ind);

    if (length (s) < k)
      warning ("returning fewer singular values than requested");
      if (! ischar (sigma))
        warning ("try increasing the value of sigma");
      endif
    endif

    s = s * max_a;
  endif

  if (nargout < 2)
    u = s;
  else
    if (max_a == 0)
      u = eye (m, k);
      s = diag (s);
      v = eye (n, k);
    else
      u = root2 * V(1:m,ind);
      s = diag (s);
      v = root2 * V(m+1:end,ind);
    endif

    if (nargout > 3)
      flag = (flag != 0);
    endif
  endif

endfunction


%!shared n, k, A, u, s, v, opts, rand_state, randn_state, tol
%! n = 100;
%! k = 7;
%! A = sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),0.4*n*ones(1,n),ones(1,n-2)]);
%! [u,s,v] = svd (full (A));
%! s = diag (s);
%! [~, idx] = sort (abs (s));
%! s = s(idx);
%! u = u(:, idx);
%! v = v(:, idx);
%! randn_state = randn ("state");
%! rand_state = rand ("state");
%! randn ("state", 42);      % Initialize to make normest function reproducible
%! rand ("state", 42);
%! opts.v0 = rand (2*n,1); % Initialize eigs ARPACK starting vector
%!                         % to guarantee reproducible results
%!
%!testif HAVE_ARPACK
%! [u2,s2,v2,flag] = svds (A,k);
%! s2 = diag (s2);
%! assert (flag, !1);
%! tol = 10 * eps() * norm(s2, 1);
%! assert (s2, s(end:-1:end-k+1), tol);
%!
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! [u2,s2,v2,flag] = svds (A,k,0,opts);
%! s2 = diag (s2);
%! assert (flag, !1);
%! tol = 100 * eps() * norm(s2, 1);
%! assert (s2, s(length(s2):-1:1), tol);
%!
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! idx = floor (n/2);
%! % Don't put sigma right on a singular value or there are convergence issues
%! sigma = 0.99*s(idx) + 0.01*s(idx+1);
%! [u2,s2,v2,flag] = svds (A,k,sigma,opts);
%! s2 = diag (s2);
%! assert (flag, !1);
%! tol = 10 * eps() * norm(s2, 1);
%! assert (s2, s((idx+floor(k/2)):-1:(idx-floor(k/2))), tol);
%!
%!testif HAVE_ARPACK
%! [u2,s2,v2,flag] = svds (zeros (10), k);
%! assert (u2, eye (10, k));
%! assert (s2, zeros (k));
%! assert (v2, eye (10, 7));
%!
%!testif HAVE_ARPACK
%! s = svds (speye (10));
%! assert (s, ones (6, 1), 8*eps);

%!test
%! ## Restore random number generator seeds at end of tests
%! rand ("state", rand_state);
%! randn ("state", randn_state);

