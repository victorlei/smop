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
## @deftypefn {Function File} {} cor_test (@var{x}, @var{y}, @var{alt}, @var{method})
## Test whether two samples @var{x} and @var{y} come from uncorrelated
## populations.
##
## The optional argument string @var{alt} describes the alternative
## hypothesis, and can be @qcode{"!="} or @qcode{"<>"} (nonzero), @qcode{">"}
## (greater than 0), or @qcode{"<"} (less than 0).  The default is the
## two-sided case.
##
## The optional argument string @var{method} specifies which correlation
## coefficient to use for testing.  If @var{method} is @qcode{"pearson"}
## (default), the (usual) Pearson's produt moment correlation coefficient is
## used.  In this case, the data should come from a bivariate normal
## distribution.  Otherwise, the other two methods offer nonparametric
## alternatives.  If @var{method} is @qcode{"kendall"}, then Kendall's rank
## correlation tau is used.  If @var{method} is @qcode{"spearman"}, then
## Spearman's rank correlation rho is used.  Only the first character is
## necessary.
##
## The output is a structure with the following elements:
##
## @table @var
## @item pval
## The p-value of the test.
##
## @item stat
## The value of the test statistic.
##
## @item dist
## The distribution of the test statistic.
##
## @item params
## The parameters of the null distribution of the test statistic.
##
## @item alternative
## The alternative hypothesis.
##
## @item method
## The method used for testing.
## @end table
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Adapted-by: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Test for zero correlation

function t = cor_test (x, y, alt, method)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  if (! isvector (x) || ! isvector (y) || length (x) != length (y))
    error ("cor_test: X and Y must be vectors of the same length");
  endif

  if (nargin < 3)
    alt = "!=";
  elseif (! ischar (alt))
    error ("cor_test: ALT must be a string");
  endif

  if (nargin < 4)
    method = "pearson";
  elseif (! ischar (method))
    error ("cor_test: METHOD must be a string");
  endif

  n = length (x);
  m = method(1);

  if (m == "p")
    r = corr (x, y);
    df = n - 2;
    t.method = "Pearson's product moment correlation";
    t.params = df;
    t.stat = sqrt (df) .* r / sqrt (1 - r.^2);
    t.dist = "t";
    cdf = tcdf (t.stat, df);
  elseif (m == "k")
    tau = kendall (x, y);
    t.method = "Kendall's rank correlation tau";
    t.params = [];
    t.stat = tau / sqrt ((2 * (2*n+5)) / (9*n*(n-1)));
    t.dist = "stdnormal";
    cdf = stdnormal_cdf (t.stat);
  elseif (m == "s")
    rho = spearman (x, y);
    t.method = "Spearman's rank correlation rho";
    t.params = [];
    t.stat = sqrt (n-1) * (rho - 6/(n^3-n));
    t.dist = "stdnormal";
    cdf = stdnormal_cdf (t.stat);
  else
    error ("cor_test: METHOD '%s' not recognized", method);
  endif

  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    t.pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    t.pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    t.pval = cdf;
  else
    error ("cor_test: alternative '%s' not recognized", alt);
  endif

  t.alternative = alt;

  if (nargout == 0)
    printf ("pval: %g\n", t.pval);
  endif

endfunction

