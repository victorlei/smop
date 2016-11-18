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
## @deftypefn {Function File} {[@var{pval}, @var{f}, @var{df_b}, @var{df_w}] =} anova (@var{y}, @var{g})
## Perform a one-way analysis of variance (ANOVA).
##
## The goal is to test whether the population means of data taken from
## @var{k} different groups are all equal.
##
## Data may be given in a single vector @var{y} with groups specified by a
## corresponding vector of group labels @var{g} (e.g., numbers from 1 to
## @var{k}).  This is the general form which does not impose any restriction
## on the number of data in each group or the group labels.
##
## If @var{y} is a matrix and @var{g} is omitted, each column of @var{y} is
## treated as a group.  This form is only appropriate for balanced ANOVA in
## which the numbers of samples from each group are all equal.
##
## Under the null of constant means, the statistic @var{f} follows an F
## distribution with @var{df_b} and @var{df_w} degrees of freedom.
##
## The p-value (1 minus the CDF of this distribution at @var{f}) is returned
## in @var{pval}.
##
## If no output argument is given, the standard one-way ANOVA table is printed.
## @seealso{manova}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: One-way analysis of variance (ANOVA)

function [pval, f, df_b, df_w] = anova (y, g)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (nargin == 1)
    if (isvector (y))
      error ("anova: for 'anova (Y)', Y must not be a vector");
    endif
    [group_count, k] = size (y);
    n = group_count * k;
    group_mean = mean (y);
  else
    if (! isvector (y))
      error ("anova: for 'anova (Y, G)', Y must be a vector");
    endif
    n = length (y);
    if (! isvector (g) || (length (g) != n))
      error ("anova: G must be a vector of the same length as Y");
    endif
    s = sort (g);
    i = find (s (2 : n) > s(1 : (n-1)));
    k = length (i) + 1;
    if (k == 1)
      error ("anova: there should be at least 2 groups");
    else
      group_label = s ([1, (reshape (i, 1, k-1) + 1)]);
    endif
    for i = 1 : k;
      v = y (find (g == group_label (i)));
      group_count (i) = length (v);
      group_mean (i) = mean (v);
    endfor

  endif

  total_mean = mean (y(:));
  SSB = sum (group_count .* (group_mean - total_mean) .^ 2);
  SST = sumsq (reshape (y, n, 1) - total_mean);
  SSW = SST - SSB;
  df_b = k - 1;
  df_w = n - k;
  v_b = SSB / df_b;
  v_w = SSW / df_w;
  f = v_b / v_w;
  pval = 1 - fcdf (f, df_b, df_w);

  if (nargout == 0)
    ## This eventually needs to be done more cleanly ...
    printf ("\n");
    printf ("One-way ANOVA Table:\n");
    printf ("\n");
    printf ("Source of Variation   Sum of Squares    df  Empirical Var\n");
    printf ("*********************************************************\n");
    printf ("Between Groups       %15.4f  %4d  %13.4f\n", SSB, df_b, v_b);
    printf ("Within Groups        %15.4f  %4d  %13.4f\n", SSW, df_w, v_w);
    printf ("---------------------------------------------------------\n");
    printf ("Total                %15.4f  %4d\n", SST, n - 1);
    printf ("\n");
    printf ("Test Statistic f     %15.4f\n", f);
    printf ("p-value              %15.4f\n", pval);
    printf ("\n");
  endif

endfunction

