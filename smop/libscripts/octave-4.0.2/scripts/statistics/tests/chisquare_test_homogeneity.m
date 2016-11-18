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
## @deftypefn {Function File} {[@var{pval}, @var{chisq}, @var{df}] =} chisquare_test_homogeneity (@var{x}, @var{y}, @var{c})
## Given two samples @var{x} and @var{y}, perform a chisquare test for
## homogeneity of the null hypothesis that @var{x} and @var{y} come from
## the same distribution, based on the partition induced by the
## (strictly increasing) entries of @var{c}.
##
## For large samples, the test statistic @var{chisq} approximately follows a
## chisquare distribution with @var{df} = @code{length (@var{c})} degrees of
## freedom.
##
## The p-value (1 minus the CDF of this distribution at @var{chisq}) is
## returned in @var{pval}.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Chi-square test for homogeneity

function [pval, chisq, df] = chisquare_test_homogeneity (x, y, c)

  if (nargin != 3)
    print_usage ();
  endif

  if (! (isvector (x) && isvector (y) && isvector (c)))
    error ("chisquare_test_homogeneity: X, Y and C must be vectors");
  endif
  ## Now test c for strictly increasing entries
  df = length (c);
  if (any ((c(2 : df) - c(1 : (df - 1))) <= 0))
    error ("chisquare_test_homogeneity: C must be increasing");
  endif

  c     = [(reshape (c, 1, df)), Inf];
  l_x   = length (x);
  x     = reshape (x, l_x, 1);
  n_x   = sum (x * ones (1, df+1) < ones (l_x, 1) * c);
  l_y   = length (y);
  y     = reshape (y, l_y, 1);
  n_y   = sum (y * ones (1, df+1) < ones (l_y, 1) * c);
  chisq = l_x * l_y * sum ((n_x/l_x - n_y/l_y).^2 ./ (n_x + n_y));
  pval  = 1 - chi2cdf (chisq, df);

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

