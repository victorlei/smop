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
## @deftypefn {Function File} {[@var{pval}, @var{t}, @var{df}] =} t_test (@var{x}, @var{m}, @var{alt})
## For a sample @var{x} from a normal distribution with unknown mean and
## variance, perform a t-test of the null hypothesis
## @code{mean (@var{x}) == @var{m}}.
##
## Under the null, the test statistic @var{t} follows a Student distribution
## with @code{@var{df} = length (@var{x}) - 1} degrees of freedom.
##
## With the optional argument string @var{alt}, the alternative of interest
## can be selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## is tested against the two-sided alternative @code{mean (@var{x}) !=
## @var{m}}.  If @var{alt} is @qcode{">"}, the one-sided alternative
## @code{mean (@var{x}) > @var{m}} is considered.  Similarly for @var{"<"},
## the one-sided alternative @code{mean (@var{x}) < @var{m}} is considered.
## The default is the two-sided case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Student's one-sample t test

function [pval, t, df] = t_test (x, m, alt)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! isvector (x))
    error ("t_test: X must be a vector");
  endif
  if (! isscalar (m))
    error ("t_test: M must be a scalar");
  endif

  n   = length (x);
  df  = n - 1;
  t   = sqrt (n) * (sum (x) / n - m) / std (x);
  cdf = tcdf (t, df);

  if (nargin == 2)
    alt = "!=";
  endif

  if (! ischar (alt))
    error ("t_test: ALT must be a string");
  endif
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    pval = cdf;
  else
    error ("t_test: option %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction


%!test
%! ## Two-sided (also the default option)
%! x = rand (10,1); n = length (x);
%! u0 = 0.5; # true mean
%! xbar = mean (x);
%! pval = t_test (x, u0, "!=");
%! if (xbar >= u0)
%!   tval = abs (tinv (0.5*pval, n-1));
%! else
%!   tval = -abs (tinv (0.5*pval, n-1));
%! endif
%! unew = tval * std(x)/sqrt(n) + u0;
%! assert (xbar, unew, 100*eps);

%!test
%! x = rand (10,1); n = length (x);
%! u0 = 0.5;
%! pval = t_test (x, u0, ">");
%! tval = tinv (1-pval, n-1);
%! unew = tval * std(x)/sqrt(n) + u0;
%! assert (mean (x), unew, 100*eps);

%!test
%! x = rand (10,1); n = length (x);
%! u0 = 0.5;
%! pval = t_test (x, u0, "<");
%! tval = tinv (pval, n-1);
%! unew = tval * std(x)/sqrt(n) + u0;
%! assert (mean (x), unew, 100*eps);

