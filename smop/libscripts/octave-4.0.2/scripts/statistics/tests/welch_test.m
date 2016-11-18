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
## @deftypefn {Function File} {[@var{pval}, @var{t}, @var{df}] =} welch_test (@var{x}, @var{y}, @var{alt})
## For two samples @var{x} and @var{y} from normal distributions with
## unknown means and unknown and not necessarily equal variances,
## perform a Welch test of the null hypothesis of equal means.
##
## Under the null, the test statistic @var{t} approximately follows a
## Student distribution with @var{df} degrees of freedom.
##
## With the optional argument string @var{alt}, the alternative of interest
## can be selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## is tested against the two-sided alternative
## @code{mean (@var{x}) != @var{m}}.  If @var{alt} is @qcode{">"}, the
## one-sided alternative mean(x) > @var{m} is considered.  Similarly for
## @qcode{"<"}, the one-sided alternative mean(x) < @var{m} is considered.
## The default is the two-sided case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Welch two-sample t test

function [pval, t, df] = welch_test (x, y, alt)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (x) && isvector (y)))
    error ("welch_test: both X and Y must be vectors");
  endif

  n_x  = length (x);
  n_y  = length (y);
  mu_x = sum (x) / n_x;
  mu_y = sum (y) / n_y;
  v_x  = sumsq (x - mu_x) / (n_x * (n_x - 1));
  v_y  = sumsq (y - mu_y) / (n_y * (n_y - 1));
  c    = v_x / (v_x + v_y);
  df   = 1 / (c^2 / (n_x - 1) + (1 - c)^2 / (n_y - 1));
  t    = (mu_x - mu_y) / sqrt (v_x + v_y);
  cdf  = tcdf (t, df);

  if (nargin == 2)
    alt = "!=";
  endif

  if (! ischar (alt))
    error ("welch_test: ALT must be a string");
  endif
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    pval = cdf;
  else
    error ("welch_test: option %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

