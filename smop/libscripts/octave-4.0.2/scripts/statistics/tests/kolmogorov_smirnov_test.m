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
## @deftypefn {Function File} {[@var{pval}, @var{ks}] =} kolmogorov_smirnov_test (@var{x}, @var{dist}, @var{params}, @var{alt})
## Perform a Kolmogorov-Smirnov test of the null hypothesis that the
## sample @var{x} comes from the (continuous) distribution @var{dist}.
##
## if F and G are the CDFs corresponding to the sample and dist,
## respectively, then the null is that F == G.
##
## The optional argument @var{params} contains a list of parameters of
## @var{dist}.  For example, to test whether a sample @var{x} comes from
## a uniform distribution on [2,4], use
##
## @example
## kolmogorov_smirnov_test (x, "unif", 2, 4)
## @end example
##
## @noindent
## @var{dist} can be any string for which a function @var{distcdf}
## that calculates the CDF of distribution @var{dist} exists.
##
## With the optional argument string @var{alt}, the alternative of interest
## can be selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## is tested against the two-sided alternative F != G@.  In this case, the
## test statistic @var{ks} follows a two-sided Kolmogorov-Smirnov
## distribution.  If @var{alt} is @qcode{">"}, the one-sided alternative F >
## G is considered.  Similarly for @qcode{"<"}, the one-sided alternative F >
## G is considered.  In this case, the test statistic @var{ks} has a
## one-sided Kolmogorov-Smirnov distribution.  The default is the two-sided
## case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: One-sample Kolmogorov-Smirnov test

function [pval, ks] = kolmogorov_smirnov_test (x, dist, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! isvector (x))
    error ("kolmogorov_smirnov_test: X must be a vector");
  endif

  n = length (x);
  s = sort (x);
  try
    f = str2func (sprintf ("%scdf", dist));
  catch
    try
      f = str2func (sprintf ("%s_cdf", dist));
    catch
      error ("kolmogorov_smirnov_test: no %scdf or %s_cdf function found",
             dist, dist);
    end_try_catch
  end_try_catch

  alt = "!=";

  args{1} = s;
  nvargs = numel (varargin);
  if (nvargs > 0)
    if (ischar (varargin{end}))
      alt = varargin{end};
      args(2:nvargs) = varargin(1:end-1);
    else
      args(2:nvargs+1) = varargin;
    endif
  endif

  z = reshape (feval (f, args{:}), 1, n);

  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    ks   = sqrt (n) * max (max ([abs(z - (0:(n-1))/n); abs(z - (1:n)/n)]));
    pval = 1 - kolmogorov_smirnov_cdf (ks);
  elseif (strcmp (alt, ">"))
    ks   = sqrt (n) * max (max ([z - (0:(n-1))/n; z - (1:n)/n]));
    pval = exp (- 2 * ks^2);
  elseif (strcmp (alt, "<"))
    ks   = - sqrt (n) * min (min ([z - (0:(n-1))/n; z - (1:n)/n]));
    pval = exp (- 2 * ks^2);
  else
    error ("kolmogorov_smirnov_test: alternative %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("pval: %g\n", pval);
  endif

endfunction


## test for recognition of unifcdf function
%!assert (kolmogorov_smirnov_test (0:100, "unif", 0, 100), 1.0, eps)
## test for recognition of logistic_cdf function
%!assert (kolmogorov_smirnov_test (0:100, "logistic"), 0)
## test for  F < G
%!assert (kolmogorov_smirnov_test (50:100, "unif", 0, 50, "<"))

%!error kolmogorov_smirnov_test (1)
%!error <X must be a vector> kolmogorov_smirnov_test ({}, "unif", 2, 4)
%!error <no not_a_distcdf or not_a_dist_cdf function found>
%! kolmogorov_smirnov_test (1, "not_a_dist");
%!error <alternative foo not recognized>
%! kolmogorov_smirnov_test (1, "unif", 2, 4, "foo");

