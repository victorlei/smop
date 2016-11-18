## Copyright (C) 2008-2015 Ben Abbott and Jaroslav Hajek
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
## @deftypefn  {Function File} {@var{q} =} quantile (@var{x})
## @deftypefnx {Function File} {@var{q} =} quantile (@var{x}, @var{p})
## @deftypefnx {Function File} {@var{q} =} quantile (@var{x}, @var{p}, @var{dim})
## @deftypefnx {Function File} {@var{q} =} quantile (@var{x}, @var{p}, @var{dim}, @var{method})
## For a sample, @var{x}, calculate the quantiles, @var{q}, corresponding to
## the cumulative probability values in @var{p}.  All non-numeric values (NaNs)
## of @var{x} are ignored.
##
## If @var{x} is a matrix, compute the quantiles for each column and
## return them in a matrix, such that the i-th row of @var{q} contains
## the @var{p}(i)th quantiles of each column of @var{x}.
##
## If @var{p} is unspecified, return the quantiles for
## @code{[0.00 0.25 0.50 0.75 1.00]}.
## The optional argument @var{dim} determines the dimension along which
## the quantiles are calculated.  If @var{dim} is omitted it defaults to
## the first non-singleton dimension.
##
## The methods available to calculate sample quantiles are the nine methods
## used by R (@url{http://www.r-project.org/}).  The default value is
## @w{METHOD = 5}.
##
## Discontinuous sample quantile methods 1, 2, and 3
##
## @enumerate 1
## @item Method 1: Inverse of empirical distribution function.
##
## @item Method 2: Similar to method 1 but with averaging at discontinuities.
##
## @item Method 3: SAS definition: nearest even order statistic.
## @end enumerate
##
## Continuous sample quantile methods 4 through 9, where p(k) is the linear
## interpolation function respecting each methods' representative cdf.
##
## @enumerate 4
## @item Method 4: p(k) = k / n.  That is, linear interpolation of the
## empirical cdf.
##
## @item Method 5: p(k) = (k - 0.5) / n.  That is a piecewise linear function
## where the knots are the values midway through the steps of the empirical
## cdf.
##
## @item Method 6: p(k) = k / (n + 1).
##
## @item Method 7: p(k) = (k - 1) / (n - 1).
##
## @item Method 8: p(k) = (k - 1/3) / (n + 1/3).  The resulting quantile
## estimates are approximately median-unbiased regardless of the distribution
## of @var{x}.
##
## @item Method 9: p(k) = (k - 3/8) / (n + 1/4).  The resulting quantile
## estimates are approximately unbiased for the expected order statistics if
## @var{x} is normally distributed.
## @end enumerate
##
## @nospell{Hyndman and Fan} (1996) recommend method 8.  Maxima, S, and R
## (versions prior to 2.0.0) use 7 as their default.  Minitab and SPSS
## use method 6.  @sc{matlab} uses method 5.
##
## References:
##
## @itemize @bullet
## @item @nospell{Becker, R. A., Chambers, J. M. and Wilks, A. R.} (1988)
## The New S Language.  Wadsworth & Brooks/Cole.
##
## @item @nospell{Hyndman, R. J. and Fan, Y.} (1996) Sample quantiles in
## statistical packages, American Statistician, 50, 361--365.
##
## @item R: A Language and Environment for Statistical Computing;
## @url{http://cran.r-project.org/doc/manuals/fullrefman.pdf}.
## @end itemize
##
## Examples:
## @c Set example in small font to prevent overfull line
##
## @smallexample
## @group
## x = randi (1000, [10, 1]);  # Create empirical data in range 1-1000
## q = quantile (x, [0, 1]);   # Return minimum, maximum of distribution
## q = quantile (x, [0.25 0.5 0.75]); # Return quartiles of distribution
## @end group
## @end smallexample
## @seealso{prctile}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Description: Matlab style quantile function of a discrete/continuous
##              distribution.

function q = quantile (x, p = [], dim, method = 5)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)) || isempty (x))
    error ("quantile: X must be a non-empty numeric vector or matrix");
  endif

  if (isempty (p))
    p = [0.00 0.25, 0.50, 0.75, 1.00];
  endif

  if (! (isnumeric (p) && isvector (p)))
    error ("quantile: P must be a numeric vector");
  endif

  if (nargin < 3)
    ## Find the first non-singleton dimension.
    (dim = find (size (x) > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= ndims (x)))
      error ("quantile: DIM must be an integer and a valid dimension");
    endif
  endif

  ## Set the permutation vector.
  perm = 1:ndims (x);
  perm(1) = dim;
  perm(dim) = 1;

  ## Permute dim to the 1st index.
  x = permute (x, perm);

  ## Save the size of the permuted x N-d array.
  sx = size (x);

  ## Reshape to a 2-d array.
  x = reshape (x, [sx(1), prod(sx(2:end))]);

  ## Calculate the quantiles.
  q = __quantile__ (x, p, method);

  ## Return the shape to the original N-d array.
  q = reshape (q, [numel(p), sx(2:end)]);

  ## Permute the 1st index back to dim.
  q = ipermute (q, perm);

endfunction


%!test
%! p = 0.50;
%! q = quantile (1:4, p);
%! qa = 2.5;
%! assert (q, qa);
%! q = quantile (1:4, p, 1);
%! qa = [1, 2, 3, 4];
%! assert (q, qa);
%! q = quantile (1:4, p, 2);
%! qa = 2.5;
%! assert (q, qa);

%!test
%! p = [0.50 0.75];
%! q = quantile (1:4, p);
%! qa = [2.5 3.5];
%! assert (q, qa);
%! q = quantile (1:4, p, 1);
%! qa = [1, 2, 3, 4; 1, 2, 3, 4];
%! assert (q, qa);
%! q = quantile (1:4, p, 2);
%! qa = [2.5 3.5];
%! assert (q, qa);

%!test
%! p = 0.5;
%! x = sort (rand (11));
%! q = quantile (x, p);
%! assert (q, x(6,:));
%! x = x.';
%! q = quantile (x, p, 2);
%! assert (q, x(:,6));

%!test
%! p = [0.00, 0.25, 0.50, 0.75, 1.00];
%! x = [1; 2; 3; 4];
%! a = [1.0000   1.0000   2.0000   3.0000   4.0000
%!      1.0000   1.5000   2.5000   3.5000   4.0000
%!      1.0000   1.0000   2.0000   3.0000   4.0000
%!      1.0000   1.0000   2.0000   3.0000   4.0000
%!      1.0000   1.5000   2.5000   3.5000   4.0000
%!      1.0000   1.2500   2.5000   3.7500   4.0000
%!      1.0000   1.7500   2.5000   3.2500   4.0000
%!      1.0000   1.4167   2.5000   3.5833   4.0000
%!      1.0000   1.4375   2.5000   3.5625   4.0000];
%! for m = 1:9
%!   q = quantile (x, p, 1, m).';
%!   assert (q, a(m,:), 0.0001);
%! endfor

%!test
%! p = [0.00, 0.25, 0.50, 0.75, 1.00];
%! x = [1; 2; 3; 4; 5];
%! a = [1.0000   2.0000   3.0000   4.0000   5.0000
%!      1.0000   2.0000   3.0000   4.0000   5.0000
%!      1.0000   1.0000   2.0000   4.0000   5.0000
%!      1.0000   1.2500   2.5000   3.7500   5.0000
%!      1.0000   1.7500   3.0000   4.2500   5.0000
%!      1.0000   1.5000   3.0000   4.5000   5.0000
%!      1.0000   2.0000   3.0000   4.0000   5.0000
%!      1.0000   1.6667   3.0000   4.3333   5.0000
%!      1.0000   1.6875   3.0000   4.3125   5.0000];
%! for m = 1:9
%!   q = quantile (x, p, 1, m).';
%!   assert (q, a(m,:), 0.0001);
%! endfor

%!test
%! p = [0.00, 0.25, 0.50, 0.75, 1.00];
%! x = [1; 2; 5; 9];
%! a = [1.0000   1.0000   2.0000   5.0000   9.0000
%!      1.0000   1.5000   3.5000   7.0000   9.0000
%!      1.0000   1.0000   2.0000   5.0000   9.0000
%!      1.0000   1.0000   2.0000   5.0000   9.0000
%!      1.0000   1.5000   3.5000   7.0000   9.0000
%!      1.0000   1.2500   3.5000   8.0000   9.0000
%!      1.0000   1.7500   3.5000   6.0000   9.0000
%!      1.0000   1.4167   3.5000   7.3333   9.0000
%!      1.0000   1.4375   3.5000   7.2500   9.0000];
%! for m = 1:9
%!   q = quantile (x, p, 1, m).';
%!   assert (q, a(m,:), 0.0001);
%! endfor

%!test
%! p = [0.00, 0.25, 0.50, 0.75, 1.00];
%! x = [1; 2; 5; 9; 11];
%! a = [1.0000    2.0000    5.0000    9.0000   11.0000
%!      1.0000    2.0000    5.0000    9.0000   11.0000
%!      1.0000    1.0000    2.0000    9.0000   11.0000
%!      1.0000    1.2500    3.5000    8.0000   11.0000
%!      1.0000    1.7500    5.0000    9.5000   11.0000
%!      1.0000    1.5000    5.0000   10.0000   11.0000
%!      1.0000    2.0000    5.0000    9.0000   11.0000
%!      1.0000    1.6667    5.0000    9.6667   11.0000
%!      1.0000    1.6875    5.0000    9.6250   11.0000];
%! for m = 1:9
%!   q = quantile (x, p, 1, m).';
%!   assert (q, a(m,:), 0.0001);
%! endfor

%!test
%! p = [0.00, 0.25, 0.50, 0.75, 1.00];
%! x = [16; 11; 15; 12; 15;  8; 11; 12;  6; 10];
%! a = [6.0000   10.0000   11.0000   15.0000   16.0000
%!      6.0000   10.0000   11.5000   15.0000   16.0000
%!      6.0000    8.0000   11.0000   15.0000   16.0000
%!      6.0000    9.0000   11.0000   13.5000   16.0000
%!      6.0000   10.0000   11.5000   15.0000   16.0000
%!      6.0000    9.5000   11.5000   15.0000   16.0000
%!      6.0000   10.2500   11.5000   14.2500   16.0000
%!      6.0000    9.8333   11.5000   15.0000   16.0000
%!      6.0000    9.8750   11.5000   15.0000   16.0000];
%! for m = 1:9
%!   q = quantile (x, p, 1, m).';
%!   assert (q, a(m,:), 0.0001);
%! endfor

%!test
%! p = [0.00, 0.25, 0.50, 0.75, 1.00];
%! x = [-0.58851;  0.40048;  0.49527; -2.551500; -0.52057; ...
%!      -0.17841; 0.057322; -0.62523;  0.042906;  0.12337];
%! a = [-2.551474  -0.588505  -0.178409   0.123366   0.495271
%!      -2.551474  -0.588505  -0.067751   0.123366   0.495271
%!      -2.551474  -0.625231  -0.178409   0.123366   0.495271
%!      -2.551474  -0.606868  -0.178409   0.090344   0.495271
%!      -2.551474  -0.588505  -0.067751   0.123366   0.495271
%!      -2.551474  -0.597687  -0.067751   0.192645   0.495271
%!      -2.551474  -0.571522  -0.067751   0.106855   0.495271
%!      -2.551474  -0.591566  -0.067751   0.146459   0.495271
%!      -2.551474  -0.590801  -0.067751   0.140686   0.495271];
%! for m = 1:9
%!   q = quantile (x, p, 1, m).';
%!   assert (q, a(m,:), 0.0001);
%! endfor

%!test
%! p = 0.5;
%! x = [0.112600, 0.114800, 0.052100, 0.236400, 0.139300
%!      0.171800, 0.727300, 0.204100, 0.453100, 0.158500
%!      0.279500, 0.797800, 0.329600, 0.556700, 0.730700
%!      0.428800, 0.875300, 0.647700, 0.628700, 0.816500
%!      0.933100, 0.931200, 0.963500, 0.779600, 0.846100];
%! tol = 0.00001;
%! x(5,5) = NaN;
%! assert (quantile (x, p, 1), [0.27950, 0.79780, 0.32960, 0.55670, 0.44460], tol);
%! x(1,1) = NaN;
%! assert (quantile (x, p, 1), [0.35415, 0.79780, 0.32960, 0.55670, 0.44460], tol);
%! x(3,3) = NaN;
%! assert (quantile (x, p, 1), [0.35415, 0.79780, 0.42590, 0.55670, 0.44460], tol);

%!test
%! sx = [2, 3, 4];
%! x = rand (sx);
%! dim = 2;
%! p = 0.5;
%! yobs = quantile (x, p, dim);
%! yexp = median (x, dim);
%! assert (yobs, yexp);

## Bug #45455 
%!assert (quantile ([1 3 2], 0.5, 1), [1 3 2])

## Test input validation
%!error quantile ()
%!error quantile (1, 2, 3, 4, 5)
%!error quantile (['A'; 'B'], 10)
%!error quantile (1:10, [true, false])
%!error quantile (1:10, ones (2,2))
%!error quantile (1, 1, 1.5)
%!error quantile (1, 1, 0)
%!error quantile (1, 1, 3)
%!error quantile ((1:5)', 0.5, 1, 0)
%!error quantile ((1:5)', 0.5, 1, 10)

## For the cumulative probability values in @var{p}, compute the
## quantiles, @var{q} (the inverse of the cdf), for the sample, @var{x}.
##
## The optional input, @var{method}, refers to nine methods available in R
## (http://www.r-project.org/). The default is @var{method} = 7.
## @seealso{prctile, quantile, statistics}

## Author: Ben Abbott <bpabbott@mac.com>
## Vectorized version: Jaroslav Hajek <highegg@gmail.com>
## Description: Quantile function of empirical samples

function inv = __quantile__ (x, p, method = 5)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (isinteger (x) || islogical (x))
    x = double (x);
  endif

  ## set shape of quantiles to column vector.
  p = p(:);

  ## Save length and set shape of samples.
  x = sort (x, 1);
  m = sum (! isnan (x));
  [xr, xc] = size (x);

  ## Initialize output values.
  inv = Inf (class (x)) * (-(p < 0) + (p > 1));
  inv = repmat (inv, 1, xc);

  ## Do the work.
  if (any (k = find ((p >= 0) & (p <= 1))))
    n = length (k);
    p = p(k);
    ## Special case of 1 row.
    if (xr == 1)
      inv(k,:) = repmat (x, n, 1);
      return;
    endif

    ## The column-distribution indices.
    pcd = kron (ones (n, 1), xr*(0:xc-1));
    mm = kron (ones (n, 1), m);
    switch (method)
      case {1, 2, 3}
        switch (method)
          case 1
            p = max (ceil (kron (p, m)), 1);
            inv(k,:) = x(p + pcd);

          case 2
            p = kron (p, m);
            p_lr = max (ceil (p), 1);
            p_rl = min (floor (p + 1), mm);
            inv(k,:) = (x(p_lr + pcd) + x(p_rl + pcd))/2;

          case 3
           ## Used by SAS, method PCTLDEF=2.
           ## http://support.sas.com/onlinedoc/913/getDoc/en/statug.hlp/stdize_sect14.htm
            t = max (kron (p, m), 1);
            t = roundb (t);
            inv(k,:) = x(t + pcd);
        endswitch

      otherwise
        switch (method)
          case 4
            p = kron (p, m);

          case 5
            ## Used by Matlab.
            p = kron (p, m) + 0.5;

          case 6
            ## Used by Minitab and SPSS.
            p = kron (p, m+1);

          case 7
            ## Used by S and R.
            p = kron (p, m-1) + 1;

          case 8
            ## Median unbiased.
            p = kron (p, m+1/3) + 1/3;

          case 9
            ## Approximately unbiased respecting order statistics.
            p = kron (p, m+0.25) + 0.375;

          otherwise
            error ("quantile: Unknown METHOD, '%d'", method);
        endswitch

        ## Duplicate single values.
        imm1 = (mm(1,:) == 1);
        x(2,imm1) = x(1,imm1);

        ## Interval indices.
        pi = max (min (floor (p), mm-1), 1);
        pr = max (min (p - pi, 1), 0);
        pi += pcd;
        inv(k,:) = (1-pr) .* x(pi) + pr .* x(pi+1);
    endswitch
  endif

endfunction

