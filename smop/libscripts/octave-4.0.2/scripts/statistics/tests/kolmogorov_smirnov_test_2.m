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
## @deftypefn {Function File} {[@var{pval}, @var{ks}, @var{d}] =} kolmogorov_smirnov_test_2 (@var{x}, @var{y}, @var{alt})
## Perform a 2-sample Kolmogorov-Smirnov test of the null hypothesis that the
## samples @var{x} and @var{y} come from the same (continuous) distribution.
##
## If F and G are the CDFs corresponding to the @var{x} and @var{y} samples,
## respectively, then the null is that F == G.
##
## With the optional argument string @var{alt}, the alternative of interest
## can be selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## is tested against the two-sided alternative F != G@.  In this case, the
## test statistic @var{ks} follows a two-sided Kolmogorov-Smirnov
## distribution.  If @var{alt} is @qcode{">"}, the one-sided alternative F >
## G is considered.  Similarly for @qcode{"<"}, the one-sided alternative F <
## G is considered.  In this case, the test statistic @var{ks} has a
## one-sided Kolmogorov-Smirnov distribution.  The default is the two-sided
## case.
##
## The p-value of the test is returned in @var{pval}.
##
## The third returned value, @var{d}, is the test statistic, the maximum
## vertical distance between the two cumulative distribution functions.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Two-sample Kolmogorov-Smirnov test

function [pval, ks, d] = kolmogorov_smirnov_test_2 (x, y, alt)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (x) && isvector (y)))
    error ("kolmogorov_smirnov_test_2: both X and Y must be vectors");
  endif

  if (nargin == 2)
    alt = "!=";
  else
    if (! ischar (alt))
      error ("kolmogorov_smirnov_test_2: ALT must be a string");
    endif
  endif

  n_x = length (x);
  n_y = length (y);
  n   = n_x * n_y / (n_x + n_y);
  x   = reshape (x, n_x, 1);
  y   = reshape (y, n_y, 1);
  [s, i] = sort ([x; y]);
  count (find (i <= n_x)) = 1 / n_x;
  count (find (i > n_x)) = - 1 / n_y;

  z = cumsum (count);
  ds = diff (s);
  if (any (ds == 0))
    ## There are some ties, so keep only those changes.
    warning ("cannot compute correct p-values with ties");
    elems = [find(ds); n_x+n_y];
    z = z(elems);
  endif

  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    d    = max (abs (z));
    ks   = sqrt (n) * d;
    pval = 1 - kolmogorov_smirnov_cdf (ks);
  elseif (strcmp (alt, ">"))
    d    = max (z);
    ks   = sqrt (n) * d;
    pval = exp (-2 * ks^2);
  elseif (strcmp (alt, "<"))
    d    = min (z);
    ks   = -sqrt (n) * d;
    pval = exp (-2 * ks^2);
  else
    error ("kolmogorov_smirnov_test_2: option %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

