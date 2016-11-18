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
## @deftypefn {Function File} {[@var{pval}, @var{chisq}, @var{df}] =} chisquare_test_independence (@var{x})
## Perform a chi-square test for independence based on the contingency table
## @var{x}.
##
## Under the null hypothesis of independence, @var{chisq} approximately has a
## chi-square distribution with @var{df} degrees of freedom.
##
## The p-value (1 minus the CDF of this distribution at chisq) of the test is
## returned in @var{pval}.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Chi-square test for independence

function [pval, chisq, df] = chisquare_test_independence (x)

  if (nargin != 1)
    print_usage ();
  endif

  [r, s] = size (x);
  df = (r - 1) * (s - 1);
  n = sum (sum (x));
  y = sum (x')' * sum (x) / n;
  x = (x - y) .^2 ./ y;
  chisq = sum (sum (x));
  pval = 1 - chi2cdf (chisq, df);

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

