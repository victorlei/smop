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
## @deftypefn {Function File} {[@var{pval}, @var{z}] =} z_test (@var{x}, @var{m}, @var{v}, @var{alt})
## Perform a Z-test of the null hypothesis @code{mean (@var{x}) == @var{m}}
## for a sample @var{x} from a normal distribution with unknown mean and known
## variance @var{v}.
##
## Under the null, the test statistic @var{z} follows a standard normal
## distribution.
##
## With the optional argument string @var{alt}, the alternative of interest
## can be selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## is tested against the two-sided alternative
## @code{mean (@var{x}) != @var{m}}.  If @var{alt} is @qcode{">"}, the
## one-sided alternative @code{mean (@var{x}) > @var{m}} is considered.
## Similarly for @qcode{"<"}, the one-sided alternative
## @code{mean (@var{x}) < @var{m}} is considered.  The default is the two-sided
## case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed along
## with some information.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Test for mean of a normal sample with known variance

function [pval, z] = z_test (x, m, v, alt)

  if (nargin < 3 || nargin > 4)
    print_usage ();
  endif

  if (! isvector (x))
    error ("z_test: X must be a vector");
  endif
  if (! isscalar (m))
    error ("z_test: M must be a scalar");
  endif
  if (! (isscalar (v) && (v > 0)))
    error ("z_test: V must be a positive scalar");
  endif

  n = length (x);
  z = sqrt (n/v) * (sum (x) / n - m);
  cdf = stdnormal_cdf (z);

  if (nargin == 3)
    alt = "!=";
  endif

  if (! ischar (alt))
    error ("z_test: ALT must be a string");
  elseif (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    pval = cdf;
  else
    error ("z_test: option %s not recognized", alt);
  endif

  if (nargout == 0)
    s = ["Z-test of mean(x) == %g against mean(x) %s %g,\n", ...
         "with known var(x) == %g:\n",                       ...
         "  pval = %g\n"];
    printf (s, m, alt, m, v, pval);
  endif

endfunction


%!test
%! ## Two-sided (also the default option)
%! x = rand (10,1); n = length (x);
%! u0 = 0.5; v = 1/12; # true mean, var
%! pval = z_test (x, u0, v, "!=");
%! if (mean (x) >= u0)
%!   zval = abs (norminv (0.5*pval));
%! else
%!   zval = -abs (norminv (0.5*pval));
%! endif
%! unew = zval * sqrt (v/n) + u0;
%! assert (mean (x), unew, 100*eps);

%!test
%! x = rand (10,1); n = length (x);
%! u0 = 0.5; v = 1/12;
%! pval = z_test (x, u0, v, ">");
%! zval = norminv (1-pval);
%! unew = zval * sqrt (v/n) + u0;
%! assert (mean (x), unew, 100*eps);

%!test
%! x = rand (10,1); n = length (x);
%! u0 = 0.5; v = 1/12;
%! pval = z_test (x, u0, v, "<");
%! zval = norminv (pval);
%! unew = zval * sqrt (v/n) + u0;
%! assert (mean (x), unew, 100*eps);

