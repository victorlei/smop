## Copyright (C) 2009-2015 E. Jason Riedy
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

########################################
## Permutation matrices

## row permutation
%!test
%! n = 5;
%! A = rand (n);
%! perm = randperm (n);
%! Prow = eye (n) (perm, :);
%! assert (A(perm, :), Prow * A);
%! invperm(perm) = 1:n;
%! assert (Prow \ A, A(invperm, :));
%! assert (Prow' * A, A(invperm, :));

## column permutation
%!test
%! n = 7;
%! A = rand (n);
%! perm = randperm (n);
%! Pcol = eye (n) (:, perm);
%! assert (A(:, perm), A * Pcol);
%! invperm(perm) = 1:n;
%! assert (A / Pcol, A(:, invperm));
%! assert (A * Pcol.', A(:, invperm));

## fall back to a matrix in addition
%!test
%! n = 4;
%! P1 = eye (n) (:, randperm (n));
%! A = zeros (n) + P1;
%! assert (sum (A), full (ones (1, n)));
%! assert (sum (A, 2), full (ones (n, 1)));

## preserve dense matrix structure
%!test
%! n = 7;
%! Pc = eye (n) (:, randperm (n));
%! Pr = eye (n) (randperm (n), :);
%! assert (typeinfo (rand (n) * Pc), "matrix");
%! assert (typeinfo (Pr * rand (n)), "matrix");

## preserve sparse matrix structure
%!test
%! n = 7;
%! Pc = eye (n) (:, randperm (n));
%! Ac = sprand (n-3, n, .5) + I () * sprand (n-3, n, .5);
%! Pr = eye (n) (randperm (n), :);
%! Ar = sprand (n, n+2, .5);
%! assert (typeinfo (Ac * Pc), "sparse complex matrix");
%! assert (full (Ac * Pc), full (Ac) * Pc);
%! assert (full (Ac / Pc), full (Ac) / Pc);
%! assert (typeinfo (Pr * Ar), "sparse matrix");
%! assert (full (Pr * Ar), Pr * full (Ar));
%! assert (full (Pr \ Ar), Pr \ full (Ar));

## structure rules for 1x1 dense / scalar and 1x1 perm
%!test
%! n = 7;
%! P1 = eye (1) (:, [1]);
%! A1 = 1;
%! P = eye (n) (:, randperm (n));
%! A = rand (n-3, n, .5);
%! assert (typeinfo (A * P1), "matrix");
%! assert (full (A * P1), full (A) * P1);
%! assert (typeinfo (P1 * A), "matrix");
%! assert (full (P1 * A), P1 * full (A));
%! assert (typeinfo (A1 * P), "matrix");
%! assert (full (A1 * P), full (A1) * P);
%! assert (typeinfo (P * A1), "matrix");
%! assert (full (P * A1), P * full (A1));

## structure rules for 1x1 sparse and 1x1 perm
%!test
%! n = 7;
%! P1 = eye (1) (:, [1]);
%! A1 = sparse (1, 1, 2);
%! P = eye (n) (:, randperm (n));
%! A = sprand (n-3, n, .5);
%! assert (typeinfo (A * P1), "sparse matrix");
%! assert (full (A * P1), full (A) * P1);
%! assert (typeinfo (P1 * A), "sparse matrix");
%! assert (full (P1 * A), P1 * full (A));
%! assert (typeinfo (A1 * P), "sparse matrix");
%! assert (full (A1 * P), full (A1) * P);
%! assert (typeinfo (P * A1), "sparse matrix");
%! assert (full (P * A1), P * full (A1));

## permuting a matrix with exceptional values does not introduce new ones.
%!test
%! n = 5;
%! pc = randperm (n);
%! Pc = eye (n) (:, pc);
%! pr = randperm (n);
%! Pr = eye (n) (pr, :);
%! A = rand (n);
%! A(n, n-2) = NaN;
%! A(3, 1) = Inf;
%! assert (Pr * A * Pc, A(pr, pc));

## conversion to sparse form
%!test
%! n = 7;
%! P = eye (n) (:, randperm (n));
%! sP = sparse (P);
%! assert (full (sP), full (P));
%! assert (size (find (sP), 1), n);
%! [I, J, V] = find (sP);
%! assert (all (V == 1));

########################################
## Diagonal matrices

## square row scaling
%!test
%! m = 7;
%! n = 11;
%! A = rand (m, n);
%! scalefact = rand (m, 1);
%! Dr = diag (scalefact);
%! assert (Dr * A, repmat (scalefact, 1, n) .* A);
%! assert (Dr \ A, A ./ repmat (scalefact, 1, n));
%! scalefact(m-1) = Inf;
%! Dr(m-1, m-1) = 0;
%! assert (Dr \ A, A ./ repmat (scalefact, 1, n));

## square column scaling
%!test
%! m = 13;
%! n = 11;
%! A = rand (m, n);
%! scalefact = rand (1, n);
%! Dc = diag (scalefact);
%! assert (A * Dc, repmat (scalefact, m, 1) .* A);
%! assert (A / Dc, A ./ repmat (scalefact, m, 1));
%! scalefact(n-1) = Inf;
%! Dc(n-1, n-1) = 0;
%! assert (A / Dc, A ./ repmat (scalefact, m, 1));

## arithmetic
%!test
%! m = 9;
%! n = 7;
%! mn = min (m, n);
%! d1 = rand (mn, 1) + I () * rand (mn, 1);
%! D1 = diag (d1, m, n);
%! d2 = rand (mn, 1);
%! D2 = diag (d2, m, n);
%! D1D2 = D1 + D2;
%! assert (typeinfo (D1D2), "complex diagonal matrix");
%! assert (diag (D1D2), d1 + d2);
%! D1D2 = D2.' * D1;
%! assert (typeinfo (D1D2), "complex diagonal matrix");
%! assert (diag (D1D2), d1 .* d2);

## slicing
%!test
%! m = 13;
%! n = 6;
%! mn = min (m, n);
%! d = rand (mn, 1);
%! D = diag (d, m, n);
%! Dslice = D (1:(m-3), 1:(n-2));
%! assert (typeinfo (Dslice), "diagonal matrix");

## preserve dense matrix structure when scaling
%!assert (typeinfo (rand (8) * (3 * eye (8))), "matrix");
%!assert (typeinfo ((3 * eye (8)) * rand (8)), "matrix");

## preserve sparse matrix structure when scaling
%!assert (typeinfo (sprand (8, 8, .5) * (3 * eye (8))), "sparse matrix");
%!assert (typeinfo (sprand (8, 8, .5) * (3 * eye (8))'), "sparse matrix");
%!assert (typeinfo (((3 + 2 * I ()) * eye (8)) * sprand (8, 8, .5)), "sparse complex matrix");
%!assert (typeinfo (((3 + 2 * I ()) * eye (8))' * sprand (8, 8, .5)), "sparse complex matrix");
%!assert (typeinfo (sprand (8, 8, .5) * ((3 + 2 * I ()) * eye (8)).'), "sparse complex matrix");

## scaling a matrix with exceptional values does not introduce new ones.
%!test
%! n = 6;
%! dr = rand (n, 1);
%! Dr = diag (dr);
%! dc = rand (1, n);
%! Dc = diag (dc);
%! A = rand (n);
%! A(n, n-2) = NaN;
%! A(4, 1) = Inf;
%! assert (Dr * A * Dc, A .* kron (dr, dc), eps);

## sparse inverse row scaling with a zero factor
%!test
%! n = 8;
%! A = sprand (n, n, .5);
%! scalefact = rand (n, 1);
%! Dr = diag (scalefact);
%! scalefact(n-1) = Inf;
%! Dr(n-1, n-1) = 0;
%! assert (full (Dr \ A), full (A) ./ repmat (scalefact, 1, n));

## narrow sparse inverse row scaling
%!test
%! n = 8;
%! A = sprand (n, n, .5);
%! scalefact = rand (n-2, 1);
%! Dr = diag (scalefact, n, n-2);
%! assert (full (Dr \ A), Dr \ full(A));

## sparse inverse column scaling with a zero factor
%!test
%! n = 11;
%! A = sprand (n, n, .5);
%! scalefact = rand (1, n);
%! Dc = diag (scalefact);
%! scalefact(n-1) = Inf;
%! Dc(n-1, n-1) = 0;
%! assert (full (A / Dc), full(A) / Dc);

## short sparse inverse column scaling
%!test
%! n = 7;
%! A = sprand (n, n, .5);
%! scalefact = rand (1, n-2) + I () * rand(1, n-2);
%! Dc = diag (scalefact, n-2, n);
%! assert (full (A / Dc), full(A) / Dc);

## adding sparse and diagonal stays sparse
%!test
%! n = 9;
%! A = sprand (n, n, .5);
%! D = 2 * eye (n);
%! assert (typeinfo (A + D), "sparse matrix");
%! assert (typeinfo (A - D), "sparse matrix");
%! D = D * I () + D;
%! assert (typeinfo (A - D), "sparse complex matrix");
%! A = A * I () + A;
%! assert (typeinfo (D - A), "sparse complex matrix");

## adding sparse and diagonal stays sparse
%!test
%! n = 9;
%! A = sprand (n, n, .5);
%! D = 2 * eye (n);
%! assert (full (A + D), full (A) + D);
%! assert (full (A - D), full (A) - D);
%! D = D * I () + D;
%! assert (full (D + A), D + full (A));
%! A = A * I () + A;
%! A(6, 4) = nan ();
%! assert (full (D - A), D - full (A));
