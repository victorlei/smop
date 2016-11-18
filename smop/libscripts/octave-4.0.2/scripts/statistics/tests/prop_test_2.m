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
## @deftypefn {Function File} {[@var{pval}, @var{z}] =} prop_test_2 (@var{x1}, @var{n1}, @var{x2}, @var{n2}, @var{alt})
## If @var{x1} and @var{n1} are the counts of successes and trials in one
## sample, and @var{x2} and @var{n2} those in a second one, test the null
## hypothesis that the success probabilities @var{p1} and @var{p2} are the
## same.
##
## Under the null, the test statistic @var{z} approximately follows a
## standard normal distribution.
##
## With the optional argument string @var{alt}, the alternative of interest
## can be selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## is tested against the two-sided alternative @var{p1} != @var{p2}.  If
## @var{alt} is @qcode{">"}, the one-sided alternative @var{p1} > @var{p2} is
## used.  Similarly for @qcode{"<"}, the one-sided alternative
## @var{p1} < @var{p2} is used.  The default is the two-sided case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compare two proportions

function [pval, z] = prop_test_2 (x1, n1, x2, n2, alt)

  if (nargin < 4 || nargin > 5)
    print_usage ();
  endif

  ## Could do sanity checking on x1, n1, x2, n2 here

  p1 = x1 / n1;
  p2 = x2 / n2;
  pc = (x1 + x2) / (n1 + n2);

  z  = (p1 - p2) / sqrt (pc * (1 - pc) * (1/n1 + 1/n2));

  cdf = stdnormal_cdf (z);

  if (nargin == 4)
    alt = "!=";
  endif

  if (! ischar (alt))
    error ("prop_test_2: ALT must be a string");
  endif
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    pval = cdf;
  else
    error ("prop_test_2: option %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

