## Copyright (C) 2014-2015 Eduardo Ramos Fernández <eduradical951@gmail.com>
## Copyright (C) 2013-2015 Kai T. Ohlhus <k.ohlhus@gmail.com>
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
## @deftypefn  {Function File} {@var{L} =} ichol (@var{A})
## @deftypefnx {Function File} {@var{L} =} ichol (@var{A}, @var{opts})
##
## Compute the incomplete Cholesky factorization of the sparse square matrix
## @var{A}.
##
## By default, @code{ichol} uses only the lower triangle of @var{A} and
## produces a lower triangular factor @var{L} such that @tcode{L*L'}
## approximates @var{A}.
##
## The factor given by this routine may be useful as a preconditioner for a
## system of linear equations being solved by iterative methods such as
## PCG (Preconditioned Conjugate Gradient).
##
## The factorization may be modified by passing options in a structure
## @var{opts}.  The option name is a field of the structure and the setting
## is the value of field.  Names and specifiers are case sensitive.
##
## @table @asis
## @item type
## Type of factorization.
##
## @table @asis
## @item @qcode{"nofill"} (default)
## Incomplete Cholesky factorization with no fill-in (@nospell{IC(0)}).
##
## @item @qcode{"ict"}
## Incomplete Cholesky factorization with threshold dropping (@nospell{ICT}).
## @end table
##
## @item diagcomp
## A non-negative scalar @var{alpha} for incomplete Cholesky factorization of
## @code{@var{A} + @var{alpha} * diag (diag (@var{A}))} instead of @var{A}.
## This can be useful when @var{A} is not positive definite.  The default value
## is 0.
##
## @item droptol
## A non-negative scalar specifying the drop tolerance for factorization if
## performing @nospell{ICT}@.  The default value is 0 which produces the
## complete Cholesky factorization.
##
## Non-diagonal entries of @var{L} are set to 0 unless
##
## @code{abs (@var{L}(i,j)) >= droptol * norm (@var{A}(j:end, j), 1)}.
##
## @item michol
## Modified incomplete Cholesky factorization:
##
## @table @asis
## @item @qcode{"off"} (default)
## Row and column sums are not necessarily preserved.
##
## @item @qcode{"on"}
## The diagonal of @var{L} is modified so that row (and column) sums are
## preserved even when elements have been dropped during the factorization.
## The relationship preserved is: @code{@var{A} * e = @var{L} * @var{L}' * e},
## where e is a vector of ones.
## @end table
##
## @item shape
##
## @table @asis
## @item @qcode{"lower"} (default)
## Use only the lower triangle of @var{A} and return a lower triangular factor
## @var{L} such that @tcode{L*L'} approximates @var{A}.
##
## @item @qcode{"upper"}
## Use only the upper triangle of @var{A} and return an upper triangular factor
## @var{U} such that @code{U'*U} approximates @var{A}.
## @end table
## @end table
##
## EXAMPLES
##
## The following problem demonstrates how to factorize a sample symmetric
## positive definite matrix with the full Cholesky decomposition and with the
## incomplete one.
##
## @example
## @group
## A = [ 0.37, -0.05,  -0.05,  -0.07;
##      -0.05,  0.116,  0.0,   -0.05;
##      -0.05,  0.0,    0.116, -0.05;
##      -0.07, -0.05,  -0.05,   0.202];
## A = sparse (A);
## nnz (tril (A))
## ans =  9
## L = chol (A, "lower");
## nnz (L)
## ans =  10
## norm (A - L * L', "fro") / norm (A, "fro")
## ans =  1.1993e-16
## opts.type = "nofill";
## L = ichol (A, opts);
## nnz (L)
## ans =  9
## norm (A - L * L', "fro") / norm (A, "fro")
## ans =  0.019736
## @end group
## @end example
##
## Another example for decomposition is a finite difference matrix used to
## solve a boundary value problem on the unit square.
##
## @example
## @group
## nx = 400; ny = 200;
## hx = 1 / (nx + 1); hy = 1 / (ny + 1);
## Dxx = spdiags ([ones(nx, 1), -2*ones(nx, 1), ones(nx, 1)],
##                [-1 0 1 ], nx, nx) / (hx ^ 2);
## Dyy = spdiags ([ones(ny, 1), -2*ones(ny, 1), ones(ny, 1)],
##                [-1 0 1 ], ny, ny) / (hy ^ 2);
## A = -kron (Dxx, speye (ny)) - kron (speye (nx), Dyy);
## nnz (tril (A))
## ans =  239400
## opts.type = "nofill";
## L = ichol (A, opts);
## nnz (tril (A))
## ans =  239400
## norm (A - L * L', "fro") / norm (A, "fro")
## ans =  0.062327
## @end group
## @end example
##
## References for implemented algorithms:
##
## [1] @nospell{Y. Saad}. "Preconditioning Techniques." @cite{Iterative
## Methods for Sparse Linear Systems}, @nospell{PWS} Publishing Company, 1996.
##
## [2] @nospell{M. Jones, P. Plassmann}: @cite{An Improved Incomplete
## Cholesky Factorization}, 1992.
## @seealso{chol, ilu, pcg}
## @end deftypefn

function L = ichol (A, opts = struct ())

  if (nargin < 1 || nargin > 2 || nargout > 1)
    print_usage ();
  endif

  if (! (issparse (A) && issquare (A)))
    error ("ichol: A must be a sparse square matrix");
  endif

  if (! isstruct (opts))
    error ("ichol: OPTS must be a structure.");
  endif

  ## If A is empty then return empty L for Matlab compatibility
  if (isempty (A))
    L = A;
    return;
  endif

  ## Parse input options
  if (! isfield (opts, "type"))
    opts.type = "nofill";  # set default
  else
    type = tolower (getfield (opts, "type"));
    if (! strcmp (type, "nofill") && ! strcmp (type, "ict"))
      error ('ichol: TYPE must be "nofill" or "ict"');
    endif
    opts.type = type;
  endif

  if (! isfield (opts, "droptol"))
    opts.droptol = 0;      # set default
  else
    if (! (isreal (opts.droptol) && isscalar (opts.droptol)
           && opts.droptol >= 0))
      error ("ichol: DROPTOL must be a non-negative real scalar");
    endif
  endif

  michol = "";
  if (! isfield (opts, "michol"))
    opts.michol = "off";   # set default
  else
    michol = tolower (getfield (opts, "michol"));
    if (! strcmp (michol, "off") && ! strcmp (michol, "on"))
      error ('ichol: MICHOL must be "on" or "off"');
    endif
    opts.michol = michol;
  endif

  if (! isfield (opts, "diagcomp"))
    opts.diagcomp = 0;     # set default
  else
    if (! (isreal (opts.diagcomp) && isscalar (opts.diagcomp)
           && opts.diagcomp >= 0))
      error ("ichol: DIAGCOMP must be a non-negative real scalar");
    endif
  endif

  if (! isfield (opts, "shape"))
    opts.shape = "lower";  # set default
  else
    shape = tolower (getfield (opts, "shape"));
    if (! strcmp (shape, "lower") && ! strcmp (shape, "upper"))
      error ('ichol: SHAPE must be "lower" or "upper"');
    endif
    opts.shape = shape;
  endif

  ## Prepare input for specialized ICHOL
  A_in = [];
  if (opts.diagcomp > 0)
    A += opts.diagcomp * diag (diag (A));
  endif
  if (strcmp (opts.shape, "upper"))
    A_in = triu (A);
    A_in = A_in';
  else
    A_in = tril (A);
  endif

  ## Delegate to specialized ICHOL
  switch (opts.type)
    case "nofill"
      L  = __ichol0__ (A_in, opts.michol);
    case "ict"
      L = __icholt__ (A_in, opts.droptol, opts.michol);
  endswitch

  if (strcmp (opts.shape, "upper"))
    L = L';
  endif

endfunction


%!shared A1, A2, A3, A4, A5, A6, A7
%! A1 = [ 0.37, -0.05,  -0.05,  -0.07;
%!       -0.05,  0.116,  0.0,   -0.05;
%!       -0.05,  0.0,    0.116, -0.05;
%!       -0.07, -0.05,  -0.05,   0.202];
%! A1 = sparse (A1);
%! A2 = gallery ("poisson", 30);
%! A3 = gallery ("tridiag", 50);
%! nx = 400; ny = 200;
%! hx = 1 / (nx + 1); hy = 1 / (ny + 1);
%! Dxx = spdiags ([ones(nx, 1), -2*ones(nx, 1), ones(nx, 1)],
%!                [-1 0 1 ], nx, nx) / (hx ^ 2);
%! Dyy = spdiags ([ones(ny, 1), -2*ones(ny, 1), ones(ny, 1)],
%!                [-1 0 1 ], ny, ny) / (hy ^ 2);
%! A4 = -kron (Dxx, speye (ny)) - kron (speye (nx), Dyy);
%! A5 = [ 0.37, -0.05,         -0.05,  -0.07;
%!       -0.05,  0.116,         0.0,   -0.05 + 0.05i;
%!       -0.05,  0.0,           0.116, -0.05;
%!       -0.07, -0.05 - 0.05i, -0.05,   0.202];
%! A5 = sparse (A5);
%! A6 = [ 0.37,     -0.05 - i, -0.05,  -0.07;
%!       -0.05 + i,  0.116,     0.0,   -0.05;
%!       -0.05,      0.0,       0.116, -0.05;
%!       -0.07,     -0.05,     -0.05,   0.202];
%! A6 = sparse (A6);
%! A7 = A5;
%! A7(1) = 2i;

## ICHOL0 tests

%!test
%! opts.type = "nofill";
%! opts.michol = "off";
%! assert (nnz (tril (A1)), nnz (ichol (A1, opts)));
%! assert (nnz (tril (A2)), nnz (ichol (A2, opts)));
%! assert (nnz (tril (A3)), nnz (ichol (A3, opts)));
%! assert (nnz (tril (A4)), nnz (ichol (A4, opts)));
%! assert (nnz (tril (A5)), nnz (ichol (A5, opts)));
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "off";
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', "fro") / norm (A1, "fro"), 0.0197, 1e-4);
%! opts.shape = "upper";
%! U = ichol (A1, opts);
%! assert (norm (A1 - U' * U, "fro") / norm (A1, "fro"), 0.0197, 1e-4);
%! opts.shape = "lower";
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', "fro") / norm (A1, "fro"), 0.0197, 1e-4);
%!
%!test
%! opts.michol = "on";
%! opts.shape = "lower";
%! opts.type = "nofill";
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', "fro") / norm (A1, "fro"), 0.0279, 1e-4);
%! opts.shape = "upper";
%! U = ichol (A1, opts);
%! assert (norm (A1 - U' * U, "fro") / norm (A1, "fro"), 0.0279, 1e-4);
%! opts.shape = "lower";
%! opts.diagcomp = 3e-3;
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', "fro") / norm (A1, "fro"), 0.0272, 1e-4);
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "off";
%! L = ichol (A2, opts);
%! assert (norm (A2 - L*L', "fro") / norm (A2, "fro"), 0.0893, 1e-4)
%! opts.michol = "on";
%! L = ichol (A2, opts);
%! assert (norm (A2 - L*L', "fro") / norm (A2, "fro"), 0.2377, 1e-4)
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "off";
%! L = ichol (A3, opts);
%! assert (norm (A3 - L*L', "fro") / norm (A3, "fro"), eps, eps);
%! opts.michol = "on";
%! L = ichol (A3, opts);
%! assert (norm (A3 - L*L', "fro") / norm (A3, "fro"), eps, eps);
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "off";
%! L = ichol (A4, opts);
%! assert (norm (A4 - L*L', "fro") / norm (A4, "fro"), 0.0623, 1e-4);
%! opts.michol = "on";
%! L = ichol (A4, opts);
%! assert (norm (A4 - L*L', "fro") / norm (A4, "fro"), 0.1664, 1e-4);
%!
%!test
%! opts.type = "nofill";
%! opts.michol = "off";
%! L = ichol (A5, opts);
%! assert (norm (A5 - L*L', "fro") / norm (A5, "fro"), 0.0195, 1e-4);
%! opts.michol = "on";
%! L = ichol (A5, opts);
%! assert (norm (A5 - L*L', "fro") / norm (A5, "fro"), 0.0276, 1e-4);

## Negative pivot
%!error <negative pivot> ichol (A6)
%!error ichol (A6)
## Complex entry in the diagonal
%!error <non-real pivot> ichol (A7)

## ICHOLT tests

#%!test
%! opts.type = "ict";
%! opts.droptol = 1e-1;
%! opts.michol = "off";
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', "fro") / norm (A1, "fro"), 0.2065, 1e-4);
%! opts.shape = "upper";
%! U = ichol (A1, opts);
%! assert (norm (A1 - U' * U, "fro") / norm (A1, "fro"), 0.2065, 1e-4);
%! opts.shape = "lower";
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', "fro") / norm (A1, "fro"), 0.2065, 1e-4);
%!
#%!test
%! opts.type = "ict";
%! opts.droptol = 1e-1;
%! opts.michol = "on";
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', "fro") / norm (A1, "fro"), 0.3266, 1e-4);
%! opts.shape = "upper";
%! U = ichol (A1, opts);
%! assert (norm (A1 - U' * U, "fro") / norm (A1, "fro"), 0.3266, 1e-4);
%! opts.shape = "lower";
%! opts.diagcomp = 3e-3;
%! L = ichol (A1, opts);
%! assert (norm (A1 - L * L', "fro") / norm (A1, "fro"), 0.3266, 1e-4);
%!
%!test
%! opts.type = "ict";
%! opts.droptol = 1e-1;
%! opts.michol = "off";
%! L = ichol (A2, opts);
%! assert (norm (A2 - L*L', "fro") / norm (A2, "fro"),  0.0893, 1e-4)
%! opts.michol = "on";
%! L = ichol (A2, opts);
%! assert (norm (A2 - L*L', "fro") / norm (A2, "fro"), 0.2377, 1e-4)
%!
%!test
%! opts.type = "ict";
%! opts.droptol = 1e-1;
%! opts.michol = "off";
%! L = ichol (A3, opts);
%! assert (norm (A3 - L*L', "fro") / norm (A3, "fro"), eps, eps);
%! opts.michol = "on";
%! L = ichol (A3, opts);
%! assert (norm (A3 - L*L', "fro") / norm (A3, "fro"), eps, eps);
%!
%!test
%! opts.type = "ict";
%! opts.droptol = 1e-1;
%! opts.michol = "off";
%! L = ichol (A4, opts);
%! assert (norm (A4 - L*L', "fro") / norm (A4, "fro"), 0.1224, 1e-4);
%! opts.michol = "on";
%! L = ichol (A4, opts);
%! assert (norm (A4 - L*L', "fro") / norm (A4, "fro"), 0.2118, 1e-4);
%!
%!test
%! opts.type = "ict";
%! opts.droptol = 1e-1;
%! opts.michol = "off";
%! L = ichol (A5, opts);
%! assert (norm (A5 - L*L', "fro") / norm (A5, "fro"), 0.2044, 1e-4);
%! opts.michol = "on";
%! L = ichol (A5, opts);
%! assert (norm (A5 - L*L', "fro") / norm (A5, "fro"), 0.3231, 1e-4);

## Test input validation
%!error <A must be a sparse square matrix> ichol ([])
%!error <A must be a sparse square matrix> ichol (0)
%!error <pivot equal to 0> ichol (sparse (0))
%!error <pivot equal to 0> ichol (sparse (-0))
%!error <negative pivot> ichol (sparse (-1))
%!test
%! opts.type = "foo";
%! fail ("ichol (A1, opts)", 'TYPE must be "nofill"');
%! opts.type = 1;
%! fail ("ichol (A1, opts)", 'TYPE must be "nofill"');
%! opts.type = [];
%! fail ("ichol (A1, opts)", 'TYPE must be "nofill"');
%!test
%! opts.droptol = -1;
%! fail ("ichol (A1, opts)", "DROPTOL must be a non-negative real scalar");
%! opts.droptol = 0.5i;
%! fail ("ichol (A1, opts)", "DROPTOL must be a non-negative real scalar");
%! opts.droptol = [];
%! fail ("ichol (A1, opts)", "DROPTOL must be a non-negative real scalar");
%!test
%! opts.michol = "foo";
%! fail ("ichol (A1, opts)", 'MICHOL must be "on"');
%! opts.michol = 1;
%! fail ("ichol (A1, opts)", 'MICHOL must be "on"');
%! opts.michol = [];
%! fail ("ichol (A1, opts)", 'MICHOL must be "on"');
%!test
%! opts.diagcomp = -1;
%! fail ("ichol (A1, opts)", "DIAGCOMP must be a non-negative real scalar");
%! opts.diagcomp = 0.5i;
%! fail ("ichol (A1, opts)", "DIAGCOMP must be a non-negative real scalar");
%! opts.diagcomp = [];
%! fail ("ichol (A1, opts)", "DIAGCOMP must be a non-negative real scalar");
%!test
%! opts.shape = "foo";
%! fail ("ichol (A1, opts)", 'SHAPE must be "lower"');
%! opts.shape = 1;
%! fail ("ichol (A1, opts)", 'SHAPE must be "lower"');
%! opts.shape = [];
%! fail ("ichol (A1, opts)", 'SHAPE must be "lower"');

