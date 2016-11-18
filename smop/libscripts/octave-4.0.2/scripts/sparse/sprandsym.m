## Copyright (C) 2004-2015 David Bateman and Andy Adler
## Copyright (C) 2012 Jordi Gutiérrez Hermoso
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
## @deftypefn  {Function File} {} sprandsym (@var{n}, @var{d})
## @deftypefnx {Function File} {} sprandsym (@var{s})
## Generate a symmetric random sparse matrix.
##
## The size of the matrix will be @var{n}x@var{n}, with a density of values
## given by @var{d}.  @var{d} must be between 0 and 1 inclusive.  Values will
## be normally distributed with a mean of zero and a variance of 1.
##
## If called with a single matrix argument, a random sparse matrix is generated
## wherever the matrix @var{s} is nonzero in its lower triangular part.
## @seealso{sprand, sprandn, spones, sparse}
## @end deftypefn

function S = sprandsym (n, d)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (nargin == 1)
    [i, j] = find (tril (n));
    [nr, nc] = size (n);
    S = sparse (i, j, randn (size (i)), nr, nc);
    S = S + tril (S, -1)';
    return;
  endif

  if (!(isscalar (n) && n == fix (n) && n > 0))
    error ("sprandsym: N must be an integer greater than 0");
  endif

  if (d < 0 || d > 1)
    error ("sprandsym: density D must be between 0 and 1");
  endif

  ## Actual number of nonzero entries
  k = round (n^2*d);

  ## Diagonal nonzero entries, same parity as k
  r = pick_rand_diag (n, k);

  ## Off diagonal nonzero entries
  m = (k - r)/2;

  ondiag = randperm (n, r);
  offdiag = randperm (n*(n - 1)/2, m);

  ## Row index
  i = lookup (cumsum (0:n), offdiag - 1) + 1;

  ## Column index
  j = offdiag - (i - 1).*(i - 2)/2;

  diagvals = randn (1, r);
  offdiagvals = randn (1, m);

  S = sparse ([ondiag, i, j], [ondiag, j, i],
              [diagvals, offdiagvals, offdiagvals], n, n);

endfunction

function r = pick_rand_diag (n, k)
  ## Pick a random number R of entries for the diagonal of a sparse NxN
  ## symmetric square matrix with exactly K nonzero entries, ensuring
  ## that this R is chosen uniformly over all such matrices.
  ##
  ## Let D be the number of diagonal entries and M the number of
  ## off-diagonal entries. Then K = D + 2*M. Let A = N*(N-1)/2 be the
  ## number of available entries in the upper triangle of the matrix.
  ## Then, by a simple counting argument, there is a total of
  ##
  ##     T = nchoosek (N, D) * nchoosek (A, M)
  ##
  ## symmetric NxN matrices with a total of K nonzero entries and D on
  ## the diagonal. Letting D range from mod (K,2) through min (N,K), and
  ## dividing by this sum, we obtain the probability P for D to be each
  ## of those values.
  ##
  ## However, we cannot use this form for computation, as the binomial
  ## coefficients become unmanageably large. Instead, we use the
  ## successive quotients Q(i) = T(i+1)/T(i), which we easily compute to
  ## be
  ##
  ##               (N - D)*(N - D - 1)*M
  ##     Q =  -------------------------------
  ##            (D + 2)*(D + 1)*(A - M + 1)
  ##
  ## Then, after prepending 1, the cumprod of these quotients is
  ##
  ##      C = [ T(1)/T(1), T(2)/T(1), T(3)/T(1), ..., T(N)/T(1) ]
  ##
  ## Their sum is thus S = sum (T)/T(1), and then C(i)/S is the desired
  ## probability P(i) for i=1:N. The cumsum will finally give the
  ## distribution function for computing the random number of entries on
  ## the diagonal R.
  ##
  ## Thanks to Zsbán Ambrus <ambrus@math.bme.hu> for most of the ideas
  ## of the implementation here, especially how to do the computation
  ## numerically to avoid overflow.

  ## Degenerate case
  if (k == 1)
    r = 1;
    return;
  endif

  ## Compute the stuff described above
  a = n*(n - 1)/2;
  d = [mod(k,2):2:min(n,k)-2];
  m = (k - d)/2;
  q = (n - d).*(n - d - 1).*m ./ (d + 2)./(d + 1)./(a - m + 1);

  ## Slight modification from discussion above: pivot around the max in
  ## order to avoid overflow (underflow is fine, just means effectively
  ## zero probabilities).
  [~, midx] = max (cumsum (log (q))) ;
  midx++;
  lc = fliplr (cumprod (1./q(midx-1:-1:1)));
  rc = cumprod (q(midx:end));

  ## Now c = t(i)/t(midx), so c > 1 == [].
  c = [lc, 1, rc];
  s = sum (c);
  p = c/s;

  ## Add final d
  d(end+1) = d(end) + 2;

  ## Pick a random r using this distribution
  r = d(sum (cumsum (p) < rand) + 1);

endfunction


%!test
%! s = sprandsym (10, 0.1);
%! assert (issparse (s));
%! assert (issymmetric (s));
%! assert (size (s), [10, 10]);
%! assert (nnz (s) / numel (s), 0.1, .01);

## Test 1-input calling form
%!test
%! s = sprandsym (sparse ([1 2 3], [3 2 3], [2 2 2]));
%! [i, j] = find (s);
%! assert (sort (i), [2 3]');
%! assert (sort (j), [2 3]');

## Test input validation
%!error sprandsym ()
%!error sprandsym (1, 2, 3)
%!error sprandsym (ones (3), 0.5)
%!error sprandsym (3.5, 0.5)
%!error sprandsym (0, 0.5)
%!error sprandsym (3, -1)
%!error sprandsym (3, 2)

