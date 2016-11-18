## Copyright (C) 2014-2015 Nir Krakauer
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{x} =} lscov (@var{A}, @var{b})
## @deftypefnx {Function File} {@var{x} =} lscov (@var{A}, @var{b}, @var{V})
## @deftypefnx {Function File} {@var{x} =} lscov (@var{A}, @var{b}, @var{V}, @var{alg})
## @deftypefnx {Function File} {[@var{x}, @var{stdx}, @var{mse}, @var{S}] =} lscov (@dots{})
##
## Compute a generalized linear least squares fit.
##
## Estimate @var{x} under the model @var{b} = @var{A}@var{x} + @var{w},
## where the noise @var{w} is assumed to follow a normal distribution
## with covariance matrix @math{{\sigma^2} V}.
##
## If the size of the coefficient matrix @var{A} is n-by-p, the
## size of the vector/array of constant terms @var{b} must be n-by-k.
##
## The optional input argument @var{V} may be a n-by-1 vector of positive
## weights (inverse variances), or a n-by-n symmetric positive semidefinite
## matrix representing the covariance of @var{b}.  If @var{V} is not
## supplied, the ordinary least squares solution is returned.
##
## The @var{alg} input argument, a guidance on solution method to use, is
## currently ignored.
##
## Besides the least-squares estimate matrix @var{x} (p-by-k), the function
## also returns @var{stdx} (p-by-k), the error standard deviation of
## estimated @var{x}; @var{mse} (k-by-1), the estimated data error covariance
## scale factors (@math{\sigma^2}); and @var{S} (p-by-p, or p-by-p-by-k if k
## > 1), the error covariance of @var{x}.
##
## Reference: @nospell{Golub and Van Loan} (1996),
## @cite{Matrix Computations (3rd Ed.)}, Johns Hopkins, Section 5.6.3
##
## @seealso{ols, gls, lsqnonneg}
## @end deftypefn

## Author: Nir Krakauer

function [x, stdx, mse, S] = lscov (A, b, V = [], alg)

  if (nargin < 2 || (rows (A) != rows (b)))
    print_usage ();
  endif

  n = rows (A);
  p = columns (A);
  k = columns (b);

  if (! isempty (V))
    if (rows (V) != n || ! any (columns (V) == [1 n]))
      error ("lscov: V should be a square matrix or a vector with the same number of rows as A");
    endif

    if (isvector (V))
      ## n-by-1 vector of inverse variances
      v = diag (sqrt (V));
      A = v * A;
      b = v * b;
    else
      ## n-by-n covariance matrix
      try
        ## ordinarily V will be positive definite
        B = chol (V)';
      catch
        ## if V is only positive semidefinite, use its
        ## eigendecomposition to find a factor B such that V = B*B'
        [B, lambda] = eig (V);
        image_dims = (diag (lambda) > 0);
        B = B(:, image_dims) * sqrt (lambda(image_dims, image_dims));
      end_try_catch
      A = B \ A;
      b = B \ b;
    endif
  endif

  pinv_A = pinv (A); #pseudoinverse

  x = pinv_A * b;

  if (isargout (3))
    dof = n - p; #degrees of freedom remaining after fit
    SSE = sumsq (b - A * x);
    mse = SSE / dof;
  endif

  s = pinv_A * pinv_A';

  stdx = sqrt (diag (s) * mse);

  if (isargout (4))
    if (k == 1)
      S = mse * s;
    else
      S = nan (p, p, k);
      for i = 1:k
        S(:, :, i) = mse(i) * s;
      endfor
    endif
  endif
endfunction


%!test
%! ## Longley data from the NIST Statistical Reference Dataset
%! Z = [  60323    83.0   234289   2356     1590    107608  1947
%!        61122    88.5   259426   2325     1456    108632  1948
%!        60171    88.2   258054   3682     1616    109773  1949
%!        61187    89.5   284599   3351     1650    110929  1950
%!        63221    96.2   328975   2099     3099    112075  1951
%!        63639    98.1   346999   1932     3594    113270  1952
%!        64989    99.0   365385   1870     3547    115094  1953
%!        63761   100.0   363112   3578     3350    116219  1954
%!        66019   101.2   397469   2904     3048    117388  1955
%!        67857   104.6   419180   2822     2857    118734  1956
%!        68169   108.4   442769   2936     2798    120445  1957
%!        66513   110.8   444546   4681     2637    121950  1958
%!        68655   112.6   482704   3813     2552    123366  1959
%!        69564   114.2   502601   3931     2514    125368  1960
%!        69331   115.7   518173   4806     2572    127852  1961
%!        70551   116.9   554894   4007     2827    130081  1962 ];
%! ## Results certified by NIST using 500 digit arithmetic
%! ## b and standard error in b
%! V = [  -3482258.63459582         890420.383607373
%!         15.0618722713733         84.9149257747669
%!        -0.358191792925910E-01    0.334910077722432E-01
%!        -2.02022980381683         0.488399681651699
%!        -1.03322686717359         0.214274163161675
%!        -0.511041056535807E-01    0.226073200069370
%!         1829.15146461355         455.478499142212 ];
%! rsd =  304.854073561965;
%! y = Z(:,1); X = [ones(rows(Z),1), Z(:,2:end)];
%! alpha = 0.05;
%! [b, stdb, mse] = lscov (X, y);
%! assert(b, V(:,1), 3e-6);
%! assert(stdb, V(:,2), -1.e-5);
%! assert(sqrt (mse), rsd, -1E-6);

%!test
%! ## Adapted from example in Matlab documentation
%! x1 = [.2 .5 .6 .8 1.0 1.1]';
%! x2 = [.1 .3 .4 .9 1.1 1.4]';
%! X = [ones(size(x1)) x1 x2];
%! y = [.17 .26 .28 .23 .27 .34]';
%! [b, se_b, mse, S] = lscov(X, y);
%! assert(b, [0.1203 0.3284 -0.1312]', 1E-4);
%! assert(se_b, [0.0643 0.2267 0.1488]', 1E-4);
%! assert(mse, 0.0015, 1E-4);
%! assert(S, [0.0041 -0.0130 0.0075; -0.0130 0.0514 -0.0328; 0.0075 -0.0328 0.0221], 1E-4);
%! w = [1 1 1 1 1 .1]';
%! [bw, sew_b, msew] = lscov (X, y, w);
%! assert(bw, [0.1046 0.4614 -0.2621]', 1E-4);
%! assert(sew_b, [0.0309 0.1152 0.0814]', 1E-4);
%! assert(msew, 3.4741e-004, -1E-4);
%! V = .2*ones(length(x1)) + .8*diag(ones(size(x1)));
%! [bg, sew_b, mseg] = lscov (X, y, V);
%! assert(bg, [0.1203 0.3284 -0.1312]', 1E-4);
%! assert(sew_b, [0.0672 0.2267 0.1488]', 1E-4);
%! assert(mseg, 0.0019, 1E-4);
%! y2 = [y 2*y];
%! [b2, se_b2, mse2, S2] = lscov (X, y2);
%! assert(b2, [b 2*b], 2*eps)
%! assert(se_b2, [se_b 2*se_b], eps)
%! assert(mse2, [mse 4*mse], eps)
%! assert(S2(:, :, 1), S, eps)
%! assert(S2(:, :, 2), 4*S, eps)

%!test
%! ## Artificial example with positive semidefinite weight matrix
%! x = (0:0.2:2)';
%! y = round(100*sin(x) + 200*cos(x));
%! X = [ones(size(x)) sin(x) cos(x)];
%! V = eye(numel(x));
%! V(end, end-1) = V(end-1, end) = 1;
%! [b, seb, mseb, S] = lscov (X, y, V);
%! assert(b, [0 100 200]', 0.2);
