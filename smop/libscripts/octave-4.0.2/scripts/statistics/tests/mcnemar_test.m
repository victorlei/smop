## Copyright (C) 1996-2015 Kurt Hornik
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
## @deftypefn {Function File} {[@var{pval}, @var{chisq}, @var{df}] =} mcnemar_test (@var{x})
## For a square contingency table @var{x} of data cross-classified on the row
## and column variables, @nospell{McNemar's} test can be used for testing the
## null hypothesis of symmetry of the classification probabilities.
##
## Under the null, @var{chisq} is approximately distributed as chisquare with
## @var{df} degrees of freedom.
##
## The p-value (1 minus the CDF of this distribution at @var{chisq}) is
## returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: McNemar's test for symmetry

function [pval, chisq, df] = mcnemar_test (x)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (min (size (x)) > 1) && issquare (x))
    error ("mcnemar_test: X must be a square matrix of size > 1");
  elseif (! (all ((x(:) >= 0)) && all (x(:) == fix (x(:)))))
    error ("mcnemar_test: all entries of X must be non-negative integers");
  endif

  r = rows (x);
  df = r * (r - 1) / 2;
  if (r == 2)
    num = max (abs (x - x') - 1, 0) .^ 2;
  else
    num = abs (x - x') .^ 2;
  endif

  chisq = sum (sum (triu (num ./ (x + x'), 1)));
  pval = 1 - chi2cdf (chisq, df);

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

