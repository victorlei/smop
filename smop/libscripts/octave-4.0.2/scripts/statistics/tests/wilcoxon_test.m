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
## @deftypefn {Function File} {[@var{pval}, @var{z}] =} wilcoxon_test (@var{x}, @var{y}, @var{alt})
## For two matched-pair sample vectors @var{x} and @var{y}, perform a
## Wilcoxon signed-rank test of the null hypothesis
## PROB (@var{x} > @var{y}) == 1/2.
##
## Under the null, the test statistic @var{z} approximately follows a
## standard normal distribution when @var{n} > 25.
##
## @strong{Caution:} This function assumes a normal distribution for @var{z}
## and thus is invalid for @var{n} @leq{} 25.
##
## With the optional argument string @var{alt}, the alternative of interest
## can be selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## is tested against the two-sided alternative
## PROB (@var{x} > @var{y}) != 1/2.  If alt is @qcode{">"}, the one-sided
## alternative PROB (@var{x} > @var{y}) > 1/2 is considered.  Similarly for
## @qcode{"<"}, the one-sided alternative PROB (@var{x} > @var{y}) < 1/2 is
## considered.  The default is the two-sided case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Wilcoxon signed-rank test

function [pval, z] = wilcoxon_test (x, y, alt)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (x) && isvector (y) && (length (x) == length (y))))
    error ("wilcoxon_test: X and Y must be vectors of the same length");
  endif

  n = length (x);
  x = reshape (x, 1, n);
  y = reshape (y, 1, n);
  d = x - y;
  d = d (find (d != 0));
  n = length (d);
  if (n > 25)
    r = ranks (abs (d));
    z = sum (r (find (d > 0)));
    z = ((z - n * (n + 1) / 4) / sqrt (n * (n + 1) * (2 * n + 1) / 24));
  else
    error ("wilcoxon_test: implementation requires more than 25 different pairs");
  endif

  cdf = stdnormal_cdf (z);

  if (nargin == 2)
    alt = "!=";
  endif

  if (! ischar (alt))
    error ("wilcoxon_test: ALT must be a string");
  elseif (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    pval = cdf;
  else
    error ("wilcoxon_test: option %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

