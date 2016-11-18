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
## @deftypefn {Function File} {[@var{pval}, @var{t}, @var{df}] =} t_test_2 (@var{x}, @var{y}, @var{alt})
## For two samples x and y from normal distributions with unknown means and
## unknown equal variances, perform a two-sample t-test of the null
## hypothesis of equal means.
##
## Under the null, the test statistic @var{t} follows a Student distribution
## with @var{df} degrees of freedom.
##
## With the optional argument string @var{alt}, the alternative of interest
## can be selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## is tested against the two-sided alternative @code{mean (@var{x}) != mean
## (@var{y})}.  If @var{alt} is @qcode{">"}, the one-sided alternative
## @code{mean (@var{x}) > mean (@var{y})} is used.  Similarly for
## @qcode{"<"}, the one-sided alternative @code{mean (@var{x}) < mean
## (@var{y})} is used.  The default is the two-sided case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Student's two-sample t test

function [pval, t, df] = t_test_2 (x, y, alt)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (x) && isvector (y)))
    error ("t_test_2: both X and Y must be vectors");
  endif

  n_x  = length (x);
  n_y  = length (y);
  df   = n_x + n_y - 2;
  mu_x = sum (x) / n_x;
  mu_y = sum (y) / n_y;
  v    = sumsq (x - mu_x) + sumsq (y - mu_y);
  t    = (mu_x - mu_y) * sqrt ((n_x * n_y * df) / (v * (n_x + n_y)));
  cdf  = tcdf (t, df);

  if (nargin == 2)
    alt = "!=";
  endif

  if (! ischar (alt))
    error ("t_test_2: ALT must be a string");
  endif
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    pval = cdf;
  else
    error ("t_test_2: option %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

