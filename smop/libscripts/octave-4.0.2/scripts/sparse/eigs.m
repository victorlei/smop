## Copyright (C) 2005-2015 David Bateman
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
## @deftypefn  {Function File} {@var{d} =} eigs (@var{A})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{A}, @var{k})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{A}, @var{k}, @var{sigma})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{A}, @var{k}, @var{sigma}, @var{opts})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{A}, @var{B})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{A}, @var{B}, @var{k})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{A}, @var{B}, @var{k}, @var{sigma})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{A}, @var{B}, @var{k}, @var{sigma}, @var{opts})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{af}, @var{n})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{af}, @var{n}, @var{B})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{af}, @var{n}, @var{k})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{af}, @var{n}, @var{B}, @var{k})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{af}, @var{n}, @var{k}, @var{sigma})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{af}, @var{n}, @var{B}, @var{k}, @var{sigma})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{af}, @var{n}, @var{k}, @var{sigma}, @var{opts})
## @deftypefnx {Function File} {@var{d} =} eigs (@var{af}, @var{n}, @var{B}, @var{k}, @var{sigma}, @var{opts})
## @deftypefnx {Function File} {[@var{V}, @var{d}] =} eigs (@var{A}, @dots{})
## @deftypefnx {Function File} {[@var{V}, @var{d}] =} eigs (@var{af}, @var{n}, @dots{})
## @deftypefnx {Function File} {[@var{V}, @var{d}, @var{flag}] =} eigs (@var{A}, @dots{})
## @deftypefnx {Function File} {[@var{V}, @var{d}, @var{flag}] =} eigs (@var{af}, @var{n}, @dots{})
## Calculate a limited number of eigenvalues and eigenvectors of @var{A},
## based on a selection criteria.
##
## The number of eigenvalues and eigenvectors to calculate is given by
## @var{k} and defaults to 6.
##
## By default, @code{eigs} solve the equation
## @tex
## $A \nu = \lambda \nu$,
## @end tex
## @ifinfo
## @code{A * v = lambda * v},
## @end ifinfo
## where
## @tex
## $\lambda$ is a scalar representing one of the eigenvalues, and $\nu$
## @end tex
## @ifinfo
## @code{lambda} is a scalar representing one of the eigenvalues, and @code{v}
## @end ifinfo
## is the corresponding eigenvector.  If given the positive definite matrix
## @var{B} then @code{eigs} solves the general eigenvalue equation
## @tex
## $A \nu = \lambda B \nu$.
## @end tex
## @ifinfo
## @code{A * v = lambda * B * v}.
## @end ifinfo
##
## The argument @var{sigma} determines which eigenvalues are returned.
## @var{sigma} can be either a scalar or a string.  When @var{sigma} is a
## scalar, the @var{k} eigenvalues closest to @var{sigma} are returned.  If
## @var{sigma} is a string, it must have one of the following values.
##
## @table @asis
## @item @qcode{"lm"}
## Largest Magnitude (default).
##
## @item @qcode{"sm"}
## Smallest Magnitude.
##
## @item @qcode{"la"}
## Largest Algebraic (valid only for real symmetric problems).
##
## @item @qcode{"sa"}
## Smallest Algebraic (valid only for real symmetric problems).
##
## @item @qcode{"be"}
## Both Ends, with one more from the high-end if @var{k} is odd (valid only for
## real symmetric problems).
##
## @item @qcode{"lr"}
## Largest Real part (valid only for complex or unsymmetric problems).
##
## @item @qcode{"sr"}
## Smallest Real part (valid only for complex or unsymmetric problems).
##
## @item @qcode{"li"}
## Largest Imaginary part (valid only for complex or unsymmetric problems).
##
## @item @qcode{"si"}
## Smallest Imaginary part (valid only for complex or unsymmetric problems).
## @end table
##
## If @var{opts} is given, it is a structure defining possible options that
## @code{eigs} should use.  The fields of the @var{opts} structure are:
##
## @table @code
## @item issym
## If @var{af} is given, then flags whether the function @var{af} defines a
## symmetric problem.  It is ignored if @var{A} is given.  The default is false.
##
## @item isreal
## If @var{af} is given, then flags whether the function @var{af} defines a
## real problem.  It is ignored if @var{A} is given.  The default is true.
##
## @item tol
## Defines the required convergence tolerance, calculated as
## @code{tol * norm (A)}.  The default is @code{eps}.
##
## @item maxit
## The maximum number of iterations.  The default is 300.
##
## @item p
## The number of Lanzcos basis vectors to use.  More vectors will result in
## faster convergence, but a greater use of memory.  The optimal value of
## @code{p} is problem dependent and should be in the range @var{k} to @var{n}.
## The default value is @code{2 * @var{k}}.
##
## @item v0
## The starting vector for the algorithm.  An initial vector close to the
## final vector will speed up convergence.  The default is for @sc{arpack}
## to randomly generate a starting vector.  If specified, @code{v0} must be
## an @var{n}-by-1 vector where @code{@var{n} = rows (@var{A})}
##
## @item disp
## The level of diagnostic printout (0|1|2).  If @code{disp} is 0 then
## diagnostics are disabled.  The default value is 0.
##
## @item cholB
## Flag if @code{chol (@var{B})} is passed rather than @var{B}.  The default is
## false.
##
## @item permB
## The permutation vector of the Cholesky@tie{}factorization of @var{B} if
## @code{cholB} is true.  That is @code{chol (@var{B}(permB, permB))}.  The
## default is @code{1:@var{n}}.
##
## @end table
##
## It is also possible to represent @var{A} by a function denoted @var{af}.
## @var{af} must be followed by a scalar argument @var{n} defining the length
## of the vector argument accepted by @var{af}.  @var{af} can be a function
## handle, an inline function, or a string.  When @var{af} is a string it
## holds the name of the function to use.
##
## @var{af} is a function of the form @code{y = af (x)} where the required
## return value of @var{af} is determined by the value of @var{sigma}.  The
## four possible forms are
##
## @table @code
## @item A * x
## if @var{sigma} is not given or is a string other than "sm".
##
## @item A \ x
## if @var{sigma} is 0 or "sm".
##
## @item (A - sigma * I) \ x
## for the standard eigenvalue problem, where @code{I} is the identity matrix
## of the same size as @var{A}.
##
## @item (A - sigma * B) \ x
## for the general eigenvalue problem.
## @end table
##
## The return arguments of @code{eigs} depend on the number of return arguments
## requested.  With a single return argument, a vector @var{d} of length @var{k}
## is returned containing the @var{k} eigenvalues that have been found.  With
## two return arguments, @var{V} is a @var{n}-by-@var{k} matrix whose columns
## are the @var{k} eigenvectors corresponding to the returned eigenvalues.  The
## eigenvalues themselves are returned in @var{d} in the form of a
## @var{n}-by-@var{k} matrix, where the elements on the diagonal are the
## eigenvalues.
##
## Given a third return argument @var{flag}, @code{eigs} returns the status
## of the convergence.  If @var{flag} is 0 then all eigenvalues have converged.
## Any other value indicates a failure to converge.
##
## This function is based on the @sc{arpack} package, written by
## @nospell{R. Lehoucq, K. Maschhoff, D. Sorensen, and C. Yang}.  For more
## information see @url{http://www.caam.rice.edu/software/ARPACK/}.
##
## @seealso{eig, svds}
## @end deftypefn

function varargout = eigs (varargin)

  ## For compatibility with Matlab, handle small matrix cases here
  ## that ARPACK does not.

  if (nargin == 0)
    print_usage ();
  endif

  call_eig = false;
  offset = 0;
  k = 6;
  sigma = "lm";

  if (isnumeric (varargin{1}) && issquare (varargin{1}))
    a = varargin{1};
    if (nargin > 1 && isnumeric (varargin{2})
        && issquare (varargin{2}) && size_equal (a, varargin{2}))
      b = varargin{2};
      offset = 1;
    endif

    if (rows (a) < 9)
      call_eig = true;
    endif

    if (nargin > 1 + offset)
      tmp = varargin{2+offset};
      if (isnumeric (tmp) && isscalar (tmp) && isreal (tmp)
          && round (tmp) == tmp)
        k = tmp;

        if (rows (a) - k < 3)
          call_eig = true;
        endif
      else
        call_eig = false;
      endif

      if (nargin > 2 + offset)
        tmp = varargin{3+offset};
        if (ischar (tmp))
          sigma = tolower (tmp);
        elseif (isnumeric (tmp) && isscalar (tmp))
          sigma = tmp;
        else
          call_eig = false;
        endif
      endif
    endif
  endif

  if (call_eig)
    varargout = cell (1, min (2, max (1, nargout)));
    if (offset)
      real_valued = isreal (a) && isreal (b);
      symmetric = issymmetric (a) && issymmetric (b);
      [varargout{:}] = eig (a, b);
    else
      real_valued = isreal (a);
      symmetric = issymmetric (a);
      [varargout{:}] = eig (a);
    endif
    varargout = select (varargout, k, sigma, real_valued, symmetric);
    if (nargout == 3)
      varargout{3} = 0;
    endif
  else
    varargout = cell (1, max (1, nargout));
    [varargout{:}] = __eigs__ (varargin{:});
  endif

endfunction

function out = select (args, k, sigma, real_valued, symmetric)

  if (numel (args) == 1)
    d = args{1};
  else
    d = diag (args{2});
  endif

  if (ischar (sigma))
    switch (sigma)
      case "lm"
        [~, idx] = sort (abs (d), "descend");

      case "sm"
        [~, idx] = sort (abs (d), "ascend");

      case "la"
        if (real_valued && symmetric)
          [~, idx] = sort (real (d), "descend");
        else
          error ('sigma = "la" requires real symmetric problem');
        endif

      case "sa"
        if (real_valued && symmetric)
          [~, idx] = sort (real (d), "ascend");
        else
          error ('sigma = "sa" requires real symmetric problem');
        endif

      case "be"
        if (real_valued && symmetric)
          [~, idx] = sort (real (d), "ascend");
        else
          error ('sigma = "be" requires real symmetric problem');
        endif

      case "lr"
        if (! (real_valued || symmetric))
          [~, idx] = sort (real (d), "descend");
        else
          error ('sigma = "lr" requires complex or unsymmetric problem');
        endif

      case "sr"
        if (! (real_valued || symmetric))
          [~, idx] = sort (real (d), "ascend");
        else
          error ('sigma = "sr" requires complex or unsymmetric problem');
        endif

      case "li"
        if (! (real_valued || symmetric))
          [~, idx] = sort (imag (d), "descend");
        else
          error ('sigma = "li" requires complex or unsymmetric problem');
        endif

      case "si"
        if (! (real_valued || symmetric))
          [~, idx] = sort (imag (d), "ascend");
        else
          error ('sigma = "si" requires complex or unsymmetric problem');
        endif

      otherwise
        error ("unrecognized value for sigma: %s", sigma);
    endswitch
  else
    ## numeric sigma, find k closest values
    [~, idx] = sort (abs (d - sigma));
  endif

  d = d(idx);

  n = numel (d);

  k = min (k, n);

  if (strcmp (sigma, "be"))
    tmp = k / 2;
    n1 = floor (tmp);
    n2 = n - ceil (tmp) + 1;
    selection = [1:floor(k/2), n2:n];
  else
    selection = 1:k;
  endif

  d = d(selection);

  if (numel (args) == 1)
    out{1} = d;
  else
    out{2} = diag (d);

    v = args{1};
    v = v(:,idx);
    out{1} = v(:,selection);
  endif

endfunction


#### SPARSE MATRIX VERSIONS ####

## Real positive definite tests, n must be even
%!shared n, k, A, d0, d2
%! n = 20;
%! k = 4;
%! A = sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),4*ones(1,n),ones(1,n-2)]);
%! d0 = eig (A);
%! d2 = sort (d0);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! rand ("state", 42); # initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (d1, d0(end:-1:(end-k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, "sm");
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "la");
%! assert (d1, d2(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sa");
%! assert (d1, d2(1:k), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "be");
%! assert (d1, d2([1:floor(k/2), (end - ceil(k/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1, "be");
%! assert (d1, d2([1:floor((k+1)/2), (end - ceil((k+1)/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (d1(idx1), d0(idx0(1:k)), 1e-11);
%!testif HAVE_ARPACK, HAVE_CHOLMOD
%! d1 = eigs (A, speye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (eigs (A, k, 4.1), eigs (A, speye (n), k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (eigs (A, k, 4.1), eigs (A, speye (n), k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, "lm", opts);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, "sm", opts);
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! fn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (d1, eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! AA = speye (10);
%! fn = @(x) AA * x;
%! opts.issym = 1;  opts.isreal = 1;
%! assert (eigs (fn, 10, AA, 3, "lm", opts), [1; 1; 1], 10*eps);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "la");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sa");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "be");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor

## Real unsymmetric tests
%!shared n, k, A, d0
%! n = 20;
%! k = 4;
%! A = sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),1:n,-ones(1,n-2)]);
%! d0 = eig (A);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! rand ("state", 42); % initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (abs (d1), abs (d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, "sm");
%! assert (abs (d1), abs (d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lr");
%! [~, idx] = sort (real (d0));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "li");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "si");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(1:k))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (abs (d1(idx1)), abs (d0(idx0(1:k))), 1e-11);
%! assert (sort (imag (d1(idx1))), sort (imag (d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK, HAVE_CHOLMOD
%! d1 = eigs (A, speye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (abs (eigs (A, k, 4.1)), abs (eigs (A, speye (n), k, 4.1)), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (sort (imag (eigs (A, k, 4.1))), sort (imag (eigs (A, speye (n), k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, "sm", opts);
%! assert (abs (d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! fn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "li");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "si");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor


## Complex hermitian tests
%!shared n, k, A, d0
%! n = 20;
%! k = 4;
%! A = sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[1i*ones(1,n-2),4*ones(1,n),-1i*ones(1,n-2)]);
%! d0 = eig (A);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! rand ("state", 42); % initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (abs (d1), abs (d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, "sm");
%! assert (abs (d1), abs (d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "li");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "si");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(1:k))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (abs (d1(idx1)), abs (d0(idx0(1:k))), 1e-11);
%! assert (sort (imag (d1(idx1))), sort (imag (d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK, HAVE_CHOLMOD
%! d1 = eigs (A, speye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, 4.1, opts);
%! assert (abs (abs (d1)), abs (eigs (A, k, 4.1)), 1e-11);
%! assert (sort (imag (abs (d1))), sort (imag (eigs (A, k, 4.1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, 4.1, opts);
%! assert (abs (abs (d1)), abs (eigs (A, k, 4.1)), 1e-11);
%! assert (sort (imag (abs (d1))), sort (imag (eigs (A, k, 4.1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (abs (eigs (A, k, 4.1)), abs (eigs (A, speye (n), k, 4.1)), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (sort (imag (eigs (A, k, 4.1))), sort (imag (eigs (A, speye (n), k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fn, n, k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fn, n, k, "sm", opts);
%! assert (abs (d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! fn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "li");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "si");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor

#### FULL MATRIX VERSIONS ####

## Real positive definite tests, n must be even
%!shared n, k, A, d0, d2
%! n = 20;
%! k = 4;
%! A = full (sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),4*ones(1,n),ones(1,n-2)]));
%! d0 = eig (A);
%! d2 = sort (d0);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! rand ("state", 42); % initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (d1, d0(end:-1:(end-k)),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sm");
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "la");
%! assert (d1, d2(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sa");
%! assert (d1, d2(1:k), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "be");
%! assert (d1, d2([1:floor(k/2), (end - ceil(k/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1, "be");
%! assert (d1, d2([1:floor((k+1)/2), (end - ceil((k+1)/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (d1(idx1), d0(idx0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, eye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! assert (eigs (A, k, 4.1), eigs (A, eye (n), k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! assert (eigs (A, k, 4.1), eigs (A, eye (n), k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, "lm", opts);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, "sm", opts);
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (d1, eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "la");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sa");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "be");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor

## Real unsymmetric tests
%!shared n, k, A, d0
%! n = 20;
%! k = 4;
%! A = full (sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),1:n,-ones(1,n-2)]));
%! d0 = eig (A);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! rand ("state", 42); % initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (abs (d1), abs (d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sm");
%! assert (abs (d1), abs (d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lr");
%! [~, idx] = sort (real (d0));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "li");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "si");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(1:k))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (abs (d1(idx1)), abs (d0(idx0(1:k))), 1e-11);
%! assert (sort (imag (d1(idx1))), sort (imag (d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, eye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! assert (abs (eigs (A, k, 4.1)), abs (eigs (A, eye (n), k, 4.1)), 1e-11);
%!testif HAVE_ARPACK
%! assert (sort (imag (eigs (A, k, 4.1))), sort (imag (eigs (A, eye (n), k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, "sm", opts);
%! assert (abs (d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "li");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "si");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor

## Complex hermitian tests
%!shared n, k, A, d0
%! n = 20;
%! k = 4;
%! A = full (sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[1i*ones(1,n-2),4*ones(1,n),-1i*ones(1,n-2)]));
%! d0 = eig (A);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! rand ("state", 42); % initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (abs (d1), abs (d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sm");
%! assert (abs (d1), abs (d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "li");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "si");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(1:k))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (abs (d1(idx1)), abs (d0(idx0(1:k))), 1e-11);
%! assert (sort (imag (d1(idx1))), sort (imag (d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, eye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, 4.1, opts);
%! assert (abs (abs (d1)), abs (eigs (A, k, 4.1)), 1e-11);
%! assert (sort (imag (abs (d1))), sort (imag (eigs (A, k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, 4.1, opts);
%! assert (abs (abs (d1)), abs (eigs (A, k, 4.1)), 1e-11);
%! assert (sort (imag (abs (d1))), sort (imag (eigs (A, k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! assert (abs (eigs (A, k, 4.1)), abs (eigs (A, eye (n), k, 4.1)), 1e-11);
%!testif HAVE_ARPACK
%! assert (sort (imag (eigs (A, k, 4.1))), sort (imag (eigs (A, eye (n), k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fn, n, k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fn, n, k, "sm", opts);
%! assert (abs (d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "li");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "si");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor

%!test
%! A = 2 * diag (ones (10, 1)) - diag (ones (9, 1), 1) - diag (ones (9, 1), -1);
%! B = diag (ones (10, 1));
%! reseig = eig (A, B);
%! [~, idx] = sort (abs (reseig), "ascend");
%! assert (eigs (A, B, 10, 0), reseig (idx))

%!test
%! X = [70 47 42 39 50 73 79 23;
%!      19 52 61 80 36 76 63 68;
%!      14 34 66 65 29  4 72  9;
%!      24  8 78 49 58 54 43 33;
%!      62 69 32 31 40 46 22 28;
%!      48 12 45 59 10 17 15 25;
%!      64 67 77 56 13 55 41 74;
%!      37 38 18 21 11  3 71  7;
%!       5 35 16  1 51 27 26 44;
%!      30 57 60 75  2 53 20  6];
%! Z = X * X';
%! r = rank (Z);
%! assert (r, 8);
%! [V, D] = eigs (Z, r, "lm");
%! ZZ = V * D * V';
%! tmp = abs (Z - ZZ);
%! assert (max (tmp(:)) < 5e-11);

%!assert (eigs (diag (1:5), 5, "sa"), [1;2;3;4;5]);
%!assert (eigs (diag (1:5), 5, "la"), [5;4;3;2;1]);
%!assert (eigs (diag (1:5), 3, "be"), [1;4;5]);
