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
## @deftypefn {Function File} {} manova (@var{x}, @var{g})
## Perform a one-way multivariate analysis of variance (MANOVA).
##
## The goal is to test whether the p-dimensional population means of data
## taken from @var{k} different groups are all equal.  All data are assumed
## drawn independently from p-dimensional normal distributions with the same
## covariance matrix.
##
## The data matrix is given by @var{x}.  As usual, rows are observations and
## columns are variables.  The vector @var{g} specifies the corresponding
## group labels (e.g., numbers from 1 to @var{k}).
##
## The LR test statistic (@nospell{Wilks' Lambda}) and approximate p-values are
## computed and displayed.
## @seealso{anova}
## @end deftypefn

## The Hotelling-Lawley and Pillai-Bartlett test statistics are coded.
## However, they are currently disabled until they can be verified by someone
## with sufficient understanding of the algorithms.  Please feel free to
## improve this.

## Author: TF <Thomas.Fuereder@ci.tuwien.ac.at>
## Adapted-By: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: One-way multivariate analysis of variance (MANOVA)

function manova (x, g)

  if (nargin != 2)
    print_usage ();
  endif

  if (isvector (x))
    error ("manova: X must not be a vector");
  endif

  [n, p] = size (x);

  if (! isvector (g) || (length (g) != n))
    error ("manova: G must be a vector of length rows (X)");
  endif

  s = sort (g);
  i = find (s (2:n) > s(1:(n-1)));
  k = length (i) + 1;

  if (k == 1)
    error ("manova: there should be at least 2 groups");
  else
    group_label = s ([1, (reshape (i, 1, k - 1) + 1)]);
  endif

  x = x - ones (n, 1) * mean (x);
  SST = x' * x;

  s = zeros (1, p);
  SSB = zeros (p, p);
  for i = 1 : k;
    v = x (find (g == group_label (i)), :);
    s = sum (v);
    SSB = SSB + s' * s / rows (v);
  endfor
  n_b = k - 1;

  SSW = SST - SSB;
  n_w = n - k;

  l = real (eig (SSB / SSW));

  if (isa (l, "single"))
    l(l < eps ("single")) = 0;
  else
    l(l < eps) = 0;
  endif

  ## Wilks' Lambda
  ## =============

  Lambda = prod (1 ./ (1 + l));

  delta = n_w + n_b - (p + n_b + 1) / 2;
  df_num = p * n_b;
  W_pval_1 = 1 - chi2cdf (- delta * log (Lambda), df_num);

  if (p < 3)
    eta = p;
  else
    eta = sqrt ((p^2 * n_b^2 - 4) / (p^2 + n_b^2 - 5));
  endif

  df_den = delta * eta - df_num / 2 + 1;

  WT = exp (- log (Lambda) / eta) - 1;
  W_pval_2 = 1 - fcdf (WT * df_den / df_num, df_num, df_den);

  if (0)

    ## Hotelling-Lawley Test
    ## =====================

    HL = sum (l);

    theta = min (p, n_b);
    u = (abs (p - n_b) - 1) / 2;
    v = (n_w - p - 1) / 2;

    df_num = theta * (2 * u + theta + 1);
    df_den = 2 * (theta * v + 1);

    HL_pval = 1 - fcdf (HL * df_den / df_num, df_num, df_den);

    ## Pillai-Bartlett
    ## ===============

    PB = sum (l ./ (1 + l));

    df_den = theta * (2 * v + theta + 1);
    PB_pval = 1 - fcdf (PB * df_den / df_num, df_num, df_den);

    printf ("\n");
    printf ("One-way MANOVA Table:\n");
    printf ("\n");
    printf ("Test             Test Statistic      Approximate p\n");
    printf ("**************************************************\n");
    printf ("Wilks            %10.4f           %10.9f \n", Lambda, W_pval_1);
    printf ("                                      %10.9f \n", W_pval_2);
    printf ("Hotelling-Lawley %10.4f           %10.9f \n", HL, HL_pval);
    printf ("Pillai-Bartlett  %10.4f           %10.9f \n", PB, PB_pval);
    printf ("\n");

  endif

  printf ("\n");
  printf ("MANOVA Results:\n");
  printf ("\n");
  printf ("# of groups:    %d\n", k);
  printf ("# of samples:   %d\n", n);
  printf ("# of variables: %d\n", p);
  printf ("\n");
  printf ("Wilks' Lambda:  %5.4f\n", Lambda);
  printf ("Approximate p:  %10.9f (chisquare approximation)\n", W_pval_1);
  printf ("                 %10.9f (F approximation)\n", W_pval_2);
  printf ("\n");

endfunction

