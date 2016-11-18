## Copyright (C) 1995-2015 Friedrich Leisch
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
## @deftypefn {Function File} {[@var{pval}, @var{chisq}] =} run_test (@var{x})
## Perform a chi-square test with 6 degrees of freedom based on the upward
## runs in the columns of @var{x}.
##
## @code{run_test} can be used to decide whether @var{x} contains independent
## data.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Run test for independence

function [pval, chisq] = run_test (x)

  if (nargin != 1)
    print_usage ();
  endif

  A = [4529.4,  9044.9, 13568,  18091,  22615,  27892;
       9044.4, 18097,   27139,  36187,  45234,  55789;
      13568,   27139,   40721,  54281,  67852,  83685;
      18091,   36187,   54281,  72414,  90470, 111580;
      22615,   45234,   67852,  90470, 113262, 139476;
      27892,   55789,   83685, 111580, 139476, 172860];

  b = [1/6; 5/24; 11/120; 19/720; 29/5040; 1/840];

  n = rows (x);
  r = run_count (x, 6) - n * b * ones (1, columns (x));

  chisq = diag (r' * A * r)' / n;
  pval  = chi2cdf (chisq, 6);

  if (nargout == 0)
    printf ("pval: %g\n", pval);
  endif

endfunction

