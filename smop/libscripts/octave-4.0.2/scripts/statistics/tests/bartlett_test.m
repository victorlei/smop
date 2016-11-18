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
## @deftypefn {Function File} {[@var{pval}, @var{chisq}, @var{df}] =} bartlett_test (@var{x1}, @dots{})
## Perform a Bartlett test for the homogeneity of variances in the data
## vectors @var{x1}, @var{x2}, @dots{}, @var{xk}, where @var{k} > 1.
##
## Under the null of equal variances, the test statistic @var{chisq}
## approximately follows a chi-square distribution with @var{df} degrees of
## freedom.
##
## The p-value (1 minus the CDF of this distribution at @var{chisq}) is
## returned in @var{pval}.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Bartlett test for homogeneity of variances

function [pval, chisq, df] = bartlett_test (varargin)

  k = nargin;
  if (k < 2)
    print_usage ();
  endif

  f = zeros (k, 1);
  v = zeros (k, 1);

  for i = 1 : k;
    x = varargin{i};
    if (! isvector (x))
      error ("bartlett_test: all arguments must be vectors");
    endif
    f(i) = length (x) - 1;
    v(i) = var (x);
  endfor

  f_tot = sum (f);
  v_tot = sum (f .* v) / f_tot;
  c     = 1 + (sum (1 ./ f) - 1 / f_tot) / (3 * (k - 1));
  chisq = (f_tot * log (v_tot) - sum (f .* log (v))) / c;
  df    = k;
  pval  = 1 - chi2cdf (chisq, df);

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

