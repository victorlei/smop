## Copyright (C) 1995-2015 Kurt Hornik
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
## @deftypefn {Function File} {[@var{pval}, @var{k}, @var{df}] =} kruskal_wallis_test (@var{x1}, @dots{})
## Perform a @nospell{Kruskal-Wallis} one-factor analysis of variance.
##
## Suppose a variable is observed for @var{k} > 1 different groups, and let
## @var{x1}, @dots{}, @var{xk} be the corresponding data vectors.
##
## Under the null hypothesis that the ranks in the pooled sample are not
## affected by the group memberships, the test statistic @var{k} is
## approximately chi-square with @var{df} = @var{k} - 1 degrees of freedom.
##
## If the data contains ties (some value appears more than once)
## @var{k} is divided by
##
## 1 - @var{sum_ties} / (@var{n}^3 - @var{n})
##
## where @var{sum_ties} is the sum of @var{t}^2 - @var{t} over each group of
## ties where @var{t} is the number of ties in the group and @var{n} is the
## total number of values in the input data.  For more info on this
## adjustment see @nospell{William H. Kruskal and W. Allen Wallis},
## @cite{Use of Ranks in One-Criterion Variance Analysis},
## Journal of the American Statistical Association, Vol. 47, No. 260 (Dec 1952).
##
## The p-value (1 minus the CDF of this distribution at @var{k}) is returned
## in @var{pval}.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Kruskal-Wallis test

function [pval, k, df] = kruskal_wallis_test (varargin)

  m = nargin;
  if (m < 2)
    print_usage ();
  endif

  n = [];
  p = [];

  for i = 1 : m;
    x = varargin{i};
    if (! isvector (x))
      error ("kruskal_wallis_test: all arguments must be vectors");
    endif
    l = length (x);
    n = [n, l];
    p = [p, (reshape (x, 1, l))];
  endfor

  r = ranks (p);

  k = 0;
  j = 0;
  for i = 1 : m;
    k = k + (sum (r ((j + 1) : (j + n(i))))) ^ 2 / n(i);
    j = j + n(i);
  endfor

  n = length (p);
  k = 12 * k / (n * (n + 1)) - 3 * (n + 1);

  ## Adjust the result to takes ties into account.
  sum_ties = sum (polyval ([1, 0, -1, 0], runlength (sort (p))));
  k = k / (1 - sum_ties / (n^3 - n));

  df = m - 1;
  pval = 1 - chi2cdf (k, df);

  if (nargout == 0)
    printf ("pval: %g\n", pval);
  endif

endfunction


## Test with ties
%!assert (abs (kruskal_wallis_test ([86 86], [74]) - 0.157299207050285) < 0.0000000000001)

