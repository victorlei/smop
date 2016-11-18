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
## @deftypefn {Function File} {[@var{pval}, @var{f}, @var{df_num}, @var{df_den}] =} var_test (@var{x}, @var{y}, @var{alt})
## For two samples @var{x} and @var{y} from normal distributions with
## unknown means and unknown variances, perform an F-test of the null
## hypothesis of equal variances.
##
## Under the null, the test statistic @var{f} follows an F-distribution with
## @var{df_num} and @var{df_den} degrees of freedom.
##
## With the optional argument string @var{alt}, the alternative of interest
## can be selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## is tested against the two-sided alternative @code{var (@var{x}) != var
## (@var{y})}.  If @var{alt} is @qcode{">"}, the one-sided alternative
## @code{var (@var{x}) > var (@var{y})} is used.  Similarly for "<", the
## one-sided alternative @code{var (@var{x}) > var (@var{y})} is used.  The
## default is the two-sided case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: F test to compare two variances

function [pval, f, df_num, df_den] = var_test (x, y, alt)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (x) && isvector (y)))
    error ("var_test: both X and Y must be vectors");
  endif

  df_num = length (x) - 1;
  df_den = length (y) - 1;
  f      = var (x) / var (y);
  cdf    = fcdf (f, df_num, df_den);

  if (nargin == 2)
    alt = "!=";
  endif

  if (! ischar (alt))
    error ("var_test: ALT must be a string");
  endif
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    pval = cdf;
  else
    error ("var_test: option %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("pval: %g\n", pval);
  endif

endfunction

