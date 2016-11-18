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
## @deftypefn  {Function File} {} ilu (@var{A})
## @deftypefnx {Function File} {} ilu (@var{A}, @var{opts})
## @deftypefnx {Function File} {[@var{L}, @var{U}] =} ilu (@dots{})
## @deftypefnx {Function File} {[@var{L}, @var{U}, @var{P}] =} ilu (@dots{})
##
## Compute the incomplete LU factorization of the sparse square matrix @var{A}.
##
## @code{ilu} returns a unit lower triangular matrix @var{L}, an upper
## triangular matrix @var{U}, and optionally a permutation matrix @var{P}, such
## that @code{@var{L}*@var{U}} approximates @code{@var{P}*@var{A}}.
##
## The factors given by this routine may be useful as preconditioners for a
## system of linear equations being solved by iterative methods such as BICG
## (BiConjugate Gradients) or GMRES (Generalized Minimum Residual Method).
##
## The factorization may be modified by passing options in a structure
## @var{opts}.  The option name is a field of the structure and the setting
## is the value of field.  Names and specifiers are case sensitive.
##
## @table @code
## @item type
## Type of factorization.
##
## @table @asis
## @item @qcode{"nofill"}
## ILU factorization with no fill-in (ILU(0)).
##
## Additional supported options: @code{milu}.
##
## @item @qcode{"crout"}
## Crout version of ILU factorization (@nospell{ILUC}).
##
## Additional supported options: @code{milu}, @code{droptol}.
##
## @item @qcode{"ilutp"} (default)
## ILU factorization with threshold and pivoting.
##
## Additional supported options: @code{milu}, @code{droptol}, @code{udiag},
## @code{thresh}.
## @end table
##
## @item droptol
## A non-negative scalar specifying the drop tolerance for factorization.  The
## default value is 0 which produces the complete LU factorization.
##
## Non-diagonal entries of @var{U} are set to 0 unless
##
## @code{abs (@var{U}(i,j)) >= droptol * norm (@var{A}(:,j))}.
##
## Non-diagonal entries of @var{L} are set to 0 unless
##
## @code{abs (@var{L}(i,j)) >= droptol * norm (@var{A}(:,j))/@var{U}(j,j)}.
##
## @item milu
## Modified incomplete LU factorization:
##
## @table @asis
## @item @qcode{"row"}
## Row-sum modified incomplete LU factorization.
## The factorization preserves row sums:
## @code{@var{A} * e = @var{L} * @var{U} * e}, where e is a vector of ones.
##
## @item @qcode{"col"}
## Column-sum modified incomplete LU factorization.
## The factorization preserves column sums:
## @code{e' * @var{A} = e' * @var{L} * @var{U}}.
##
## @item @qcode{"off"} (default)
## Row and column sums are not necessarily preserved.
## @end table
##
## @item udiag
## If true, any zeros on the diagonal of the upper triangular factor are
## replaced by the local drop tolerance
## @code{droptol * norm (@var{A}(:,j))/@var{U}(j,j)}.  The default is false.
##
## @item thresh
## Pivot threshold for factorization.  It can range between 0 (diagonal
## pivoting) and 1 (default), where the maximum magnitude entry in the column
## is chosen to be the pivot.
## @end table
##
## If @code{ilu} is called with just one output, the returned matrix is
## @code{@var{L} + @var{U} - speye (size (@var{A}))}, where @var{L} is unit
## lower triangular and @var{U} is upper triangular.
##
## With two outputs, @code{ilu} returns a unit lower triangular matrix @var{L}
## and an upper triangular matrix @var{U}.  For @var{opts}.type ==
## @qcode{"ilutp"}, one of the factors is permuted based on the value of
## @var{opts}.milu.  When @var{opts}.milu == @qcode{"row"}, @var{U} is a
## column permuted upper triangular factor.  Otherwise, @var{L} is a
## row-permuted unit lower triangular factor.
##
## If there are three named outputs and @var{opts}.milu != @qcode{"row"},
## @var{P} is returned such that @var{L} and @var{U} are incomplete factors
## of @code{@var{P}*@var{A}}.  When @var{opts}.milu == @qcode{"row"}, @var{P}
## is returned such that @var{L} and @var{U} are incomplete factors of
## @code{@var{A}*@var{P}}.
##
## EXAMPLES
##
## @example
## @group
## A = gallery ("neumann", 1600) + speye (1600);
## opts.type = "nofill";
## nnz (A)
## ans = 7840
##
## nnz (lu (A))
## ans = 126478
##
## nnz (ilu (A, opts))
## ans = 7840
## @end group
## @end example
##
## This shows that @var{A} has 7,840 nonzeros, the complete LU factorization
## has 126,478 nonzeros, and the incomplete LU factorization, with 0 level of
## fill-in, has 7,840 nonzeros, the same amount as @var{A}.  Taken from:
## http://www.mathworks.com/help/matlab/ref/ilu.html
##
## @example
## @group
## A = gallery ("wathen", 10, 10);
## b = sum (A, 2);
## tol = 1e-8;
## maxit = 50;
## opts.type = "crout";
## opts.droptol = 1e-4;
## [L, U] = ilu (A, opts);
## x = bicg (A, b, tol, maxit, L, U);
## norm (A * x - b, inf)
## @end group
## @end example
##
## This example uses ILU as preconditioner for a random FEM-Matrix, which has a
## large condition number.  Without @var{L} and @var{U} BICG would not converge.
##
## @seealso{lu, ichol, bicg, gmres}
## @end deftypefn

function [L, U, P] = ilu (A, opts = struct ())

  if (nargin < 1 || nargin > 2 || (nargout > 3))
    print_usage ();
  endif

  if (! (issparse (A) && issquare (A)))
    error ("ichol: A must be a sparse square matrix");
  endif

  if (! isstruct (opts))
    error ("ichol: OPTS must be a structure.");
  endif

  ## If A is empty then return empty L, U and P for Matlab compatibility
  if (isempty (A))
    L = U = P = A;
    return;
  endif

  ## Parse input options
  if (! isfield (opts, "type"))
    opts.type = "nofill";  # set default
  else
    type = tolower (getfield (opts, "type"));
    if (! any (strcmp (type, {"nofill", "crout", "ilutp"})))
      error ("ilu: invalid TYPE specified");
    endif
    opts.type = type;
  endif

  if (! isfield (opts, "droptol"))
    opts.droptol = 0;      # set default
  else
    if (! (isreal (opts.droptol) && isscalar (opts.droptol)
           && opts.droptol >= 0))
      error ("ilu: DROPTOL must be a non-negative real scalar");
    endif
  endif

  if (! isfield (opts, "milu"))
    opts.milu = "off";     # set default
  else
    milu = tolower (getfield (opts, "milu"));
    if (! any (strcmp (milu, {"off", "col", "row"})))
      error ('ilu: MILU must be one of "off", "col", or "row"');
    endif
    opts.milu = milu;
  endif

  if (! isfield (opts, "udiag"))
    opts.udiag = 0;        # set default
  else
    if (! isscalar (opts.udiag) || (opts.udiag != 0 && opts.udiag != 1))
      error ("ilu: UDIAG must be 0 or 1");
    endif
  endif

  if (! isfield (opts, "thresh"))
    opts.thresh = 1;       # set default
  else
    if (! (isreal (opts.thresh) && isscalar (opts.thresh))
        || opts.thresh < 0 || opts.thresh > 1)
      error ("ilu: THRESH must be a scalar in the range [0, 1]");
    endif
  endif

  n = length (A);

  ## Delegate to specialized ILU
  switch (opts.type)
    case "nofill"
        [L, U] = __ilu0__ (A, opts.milu);
        if (nargout == 3)
          P = speye (length (A));
        endif
    case "crout"
        [L, U] = __iluc__ (A, opts.droptol, opts.milu);
        if (nargout == 3)
          P = speye (length (A));
        endif
    case "ilutp"
        if (nargout == 2)
          [L, U]  = __ilutp__ (A, opts.droptol, opts.thresh,
                                  opts.milu, opts.udiag);
        elseif (nargout == 3)
          [L, U, P]  = __ilutp__ (A, opts.droptol, opts.thresh,
                                     opts.milu, opts.udiag);
        endif
  endswitch

  if (nargout == 1)
    L = L + U - speye (n);
  endif

endfunction


%!shared n, dtol, A
%! n = 1600;
%! dtol = 0.1;
%! A = gallery ("neumann", n) + speye (n);
%!test
%! opts.type = "nofill";
%! assert (nnz (ilu (A, opts)), 7840);
## This test has been verified in both Matlab and Octave.
%!test
%! opts.type = "crout";
%! opts.milu = "row";
%! opts.droptol = dtol;
%! [L, U] = ilu (A, opts);
%! e = ones (size (A, 2),1);
%! assert (norm (A*e - L*U*e), 1e-14, 1e-14);
%!test
%! opts.type = "crout";
%! opts.droptol = dtol;
%! [L, U] = ilu (A, opts);
%! assert (norm (A - L * U, "fro") / norm (A, "fro"), 0.05, 1e-2);

## Check if the elements in U satisfy the non-dropping condition.
%!test
%! opts.type = "crout";
%! opts.droptol = dtol;
%! [L, U] = ilu (A, opts);
%! for j = 1:n
%!   cmp_value = dtol * norm (A(:, j));
%!   non_zeros = nonzeros (U(:, j));
%!   assert (abs (non_zeros) >= cmp_value);
%! endfor
%!test
%! opts.type = "ilutp";
%! opts.droptol = dtol;
%! [L, U] = ilu (A, opts);
%! for j = 1:n
%!   cmp_value = dtol * norm (A(:, j));
%!   non_zeros = nonzeros (U(:, j));
%!   assert (abs (non_zeros) >= cmp_value);
%! endfor

## Check that the complete LU factorisation with crout and ilutp algorithms
## produce the same result.
%!test
%! opts.type = "crout";
%! opts.droptol = 0;
%! [L1, U1] = ilu (A, opts);
%! opts.type = "ilutp";
%! opts.thresh = 0;
%! [L2, U2] = ilu (A, opts);
%! assert (norm (L1 - L2, "fro") / norm (L1, "fro"), 0, eps);
%! assert (norm (U1 - U2, "fro") / norm (U1, "fro"), 0, eps);

## Tests for real matrices of different sizes for ilu0, iluc and ilutp.
## The difference A - L*U should be not greater than eps because with droptol
## equaling 0, the LU complete factorization is performed.
%!shared n_tiny, n_small, n_medium, n_large, A_tiny, A_small, A_medium, A_large
%! n_tiny = 5;
%! n_small = 40;
%! n_medium = 600;
%! n_large = 10000;
%! A_tiny = spconvert ([1 4 2 3 3 4 2 5; 1 1 2 3 4 4 5 5; 1 2 3 4 5 6 7 8]');
%! A_small = sprand (n_small, n_small, 1/n_small) + speye (n_small);
%! A_medium = sprand (n_medium, n_medium, 1/n_medium) + speye (n_medium);
%! A_large = sprand (n_large, n_large, 1/n_large/10) + speye (n_large);
%!
%!test
%! opts.type = "nofill";
%! [L, U] = ilu (A_tiny);
%! assert (norm (A_tiny - L*U, "fro") / norm (A_tiny, "fro"), 0, n_tiny * eps);
%!test
%! opts.type = "nofill";
%! [L, U] = ilu (A_small);
%! assert (norm (A_small - L*U, "fro") / norm (A_small, "fro"), 0, 1);
%!test
%! opts.type = "nofill";
%! [L, U] = ilu (A_medium);
%! assert (norm (A_medium - L*U, "fro") / norm (A_medium, "fro"), 0, 1);
%!test
%! opts.type = "nofill";
%! [L, U] = ilu (A_large);
%! assert (norm (A_large - L*U, "fro") / norm (A_large, "fro"), 0, 1);
%!
%!test
%! opts.type = "crout";
%! [L, U] = ilu (A_tiny, opts);
%! assert (norm (A_tiny - L*U, "fro") / norm (A_tiny, "fro"), eps, eps);
%!test
%! opts.type = "crout";
%! [L, U] = ilu (A_small, opts);
%! assert (norm (A_small - L*U, "fro") / norm (A_small, "fro"), eps, eps);
%!test
%! opts.type = "crout";
%! [L, U] = ilu (A_medium, opts);
%! assert (norm (A_medium - L*U, "fro") / norm (A_medium, "fro"), eps, eps);
%!test
%! opts.type = "crout";
%! [L, U] = ilu (A_large, opts);
%! assert (norm (A_large - L*U, "fro") / norm (A_large, "fro"), eps, eps);
%!
%!test
%! opts.type = "ilutp";
%! opts.droptol = 0;
%! opts.thresh = 0;
%! [L, U] = ilu (A_tiny, opts);
%! assert (norm (A_tiny - L*U, "fro") / norm (A_tiny, "fro"), eps, eps);
%!test
%! opts.type = "ilutp";
%! opts.droptol = 0;
%! opts.thresh = 0;
%! [L, U] = ilu (A_small, opts);
%! assert (norm (A_small - L*U, "fro") / norm (A_small, "fro"), eps, eps);
%!test
%! opts.type = "ilutp";
%! opts.droptol = 0;
%! opts.thresh = 0;
%! [L, U] = ilu (A_medium, opts);
%! assert (norm (A_medium - L*U, "fro") / norm (A_medium, "fro"), eps, eps);
%!test
%! opts.type = "ilutp";
%! opts.droptol = 0;
%! opts.thresh = 0;
%! [L, U] = ilu (A_large, opts);
%! assert (norm (A_large - L*U, "fro") / norm (A_large, "fro"), eps, eps);

## Tests for complex matrices of different sizes for ilu0, iluc and ilutp.
%!shared n_tiny, n_small, n_medium, n_large, A_tiny, A_small, A_medium, A_large
%! n_tiny = 5;
%! n_small = 40;
%! n_medium = 600;
%! n_large = 10000;
%! A_tiny = spconvert ([1 4 2 3 3 4 2 5; 1 1 2 3 4 4 5 5; 1 2 3 4 5 6 7 8]');
%! A_tiny(1,1) += 1i;
%! A_small = sprand (n_small, n_small, 1/n_small) + ...
%!   i * sprand (n_small, n_small, 1/n_small) + speye (n_small);
%! A_medium = sprand (n_medium, n_medium, 1/n_medium) + ...
%!   i * sprand (n_medium, n_medium, 1/n_medium) + speye (n_medium);
%! A_large = sprand (n_large, n_large, 1/n_large/10) + ...
%!   i * sprand (n_large, n_large, 1/n_large/10) + speye (n_large);
%!
%!test
%! opts.type = "nofill";
%! [L, U] = ilu (A_tiny);
%! assert (norm (A_tiny - L*U, "fro") / norm (A_tiny, "fro"), 0, n_tiny * eps);
%!test
%! opts.type = "nofill";
%! [L, U] = ilu (A_small);
%! assert (norm (A_small - L*U, "fro") / norm (A_small, "fro"), 0, 1);
%!test
%! opts.type = "nofill";
%! [L, U] = ilu (A_medium);
%! assert (norm (A_medium - L*U, "fro") / norm (A_medium, "fro"), 0, 1);
%!test
%! opts.type = "nofill";
%! [L, U] = ilu (A_large);
%! assert (norm (A_large - L*U, "fro") / norm (A_large, "fro"), 0, 1);
%!
%!test
%! opts.type = "crout";
%! [L, U] = ilu (A_tiny, opts);
%! assert (norm (A_tiny - L*U, "fro") / norm (A_tiny, "fro"), eps, eps);
%!test
%! opts.type = "crout";
%! [L, U] = ilu (A_small, opts);
%! assert (norm (A_small - L*U, "fro") / norm (A_small, "fro"), eps, eps);
%!test
%! opts.type = "crout";
%! [L, U] = ilu (A_medium, opts);
%! assert (norm (A_medium - L*U, "fro") / norm (A_medium, "fro"), eps, eps);
%!test
%! opts.type = "crout";
%! [L, U] = ilu (A_large, opts);
%! assert (norm (A_large - L*U, "fro") / norm (A_large, "fro"), eps, eps);
%!
%!test
%! opts.type = "ilutp";
%! opts.droptol = 0;
%! opts.thresh = 0;
%! [L, U] = ilu (A_tiny, opts);
%! assert (norm (A_tiny - L*U, "fro") / norm (A_tiny, "fro"), eps, eps);
%!test
%! opts.type = "ilutp";
%! opts.droptol = 0;
%! opts.thresh = 0;
%! [L, U] = ilu (A_small, opts);
%! assert (norm (A_small - L*U, "fro") / norm (A_small, "fro"), eps, eps);
%!test
%! opts.type = "ilutp";
%! opts.droptol = 0;
%! opts.thresh = 0;
%! [L, U] = ilu (A_medium, opts);
%! assert (norm (A_medium - L*U, "fro") / norm (A_medium, "fro"), eps, eps);
%!test
%! opts.type = "ilutp";
%! opts.droptol = 0;
%! opts.thresh = 0;
%! [L, U] = ilu (A_large, opts);
%! assert (norm (A_large - L*U, "fro") / norm (A_large, "fro"), eps, eps);

## Specific tests for ilutp

%!shared a1, a2
%! a1 = sparse ([0 0 4 3 1; 5 1 2.3 2 4.5; 0 0 0 2 1;0 0 8 0 2.2; 0 0 9 9 1 ]);
%! a2 = sparse ([3 1 0 0 4; 3 1 0 0 -2;0 0 8 0 0; 0 4 0 4 -4.5; 0 -1 0 0 1]);
%!test
%! opts.udiag = 1;
%! opts.type = "ilutp";
%! opts.droptol = 0.2;
%! [L, U, P] = ilu (a1, opts);
%! assert (norm (U, "fro"), 17.4577, 1e-4);
%! assert (norm (L, "fro"), 2.4192, 1e-4);
%! opts.udiag = 0;
%! #fail ("ilu (a1, opts)");
%!
%!test
%! opts.type = "ilutp";
%! opts.droptol = 0;
%! opts.thresh = 0;
%! opts.milu = "row";
%! #fail ("ilu (a2, opts)");

## Tests for input validation
%!shared A_tiny
%! A_tiny = spconvert ([1 4 2 3 3 4 2 5; 1 1 2 3 4 4 5 5; 1 2 3 4 5 6 7 8]');

%!test
%! [L, U] = ilu (sparse ([]));
%! assert (isempty (L));
%! assert (isempty (U));
%! opts.type = "crout";
%! [L, U] = ilu (sparse ([]), opts);
%! assert (isempty (L));
%! assert (isempty (U));
%! opts.type = "ilutp";
%! [L, U] = ilu (sparse ([]), opts);
%! assert (isempty (L));
%! assert (isempty (U));
%!error <A must be a sparse square matrix> ilu (0)
%!error <A must be a sparse square matrix> ilu ([])
%!error <zero on the diagonal> ilu (sparse (0))

%!test
%! opts.type = "foo";
%! fail ("ilu (A_tiny, opts)", "invalid TYPE specified");
%! opts.type = 1;
%! fail ("ilu (A_tiny, opts)", "invalid TYPE specified");
%! opts.type = [];
%! fail ("ilu (A_tiny, opts)", "invalid TYPE specified");
%!test
%! opts.droptol = -1;
%! fail ("ilu (A_tiny, opts)", "DROPTOL must be a non-negative real scalar");
%! opts.droptol = 0.5i;
%! fail ("ilu (A_tiny, opts)", "DROPTOL must be a non-negative real scalar");
%! opts.droptol = [];
%! fail ("ilu (A_tiny, opts)", "DROPTOL must be a non-negative real scalar");
%!test
%! opts.milu = "foo";
%! fail ("ilu (A_tiny, opts)", 'MILU must be one of "off"');
%! opts.milu = 1;
%! fail ("ilu (A_tiny, opts)", 'MILU must be one of "off"');
%! opts.milu = [];
%! fail ("ilu (A_tiny, opts)", 'MILU must be one of "off"');
%!test
%! opts.udiag = -1;
%! fail ("ilu (A_tiny, opts)", "UDIAG must be 0 or 1");
%! opts.udiag = 0.5i;
%! fail ("ilu (A_tiny, opts)", "UDIAG must be 0 or 1");
%! opts.udiag = [];
%! fail ("ilu (A_tiny, opts)", "UDIAG must be 0 or 1");
%!test
%! opts.thresh = -1;
%! fail ("ilu (A_tiny, opts)", "THRESH must be a scalar");
%! opts.thresh = 0.5i;
%! fail ("ilu (A_tiny, opts)", "THRESH must be a scalar");
%! opts.thresh = [];
%! fail ("ilu (A_tiny, opts)", "THRESH must be a scalar");

