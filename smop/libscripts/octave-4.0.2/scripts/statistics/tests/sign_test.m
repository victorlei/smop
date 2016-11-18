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
## @deftypefn {Function File} {[@var{pval}, @var{b}, @var{n}] =} sign_test (@var{x}, @var{y}, @var{alt})
## For two matched-pair samples @var{x} and @var{y}, perform a sign test
## of the null hypothesis
## PROB (@var{x} > @var{y}) == PROB (@var{x} < @var{y}) == 1/2.
##
## Under the null, the test statistic @var{b} roughly follows a
## binomial distribution with parameters
## @code{@var{n} = sum (@var{x} != @var{y})} and @var{p} = 1/2.
##
## With the optional argument @code{alt}, the alternative of interest can be
## selected.  If @var{alt} is @qcode{"!="} or @qcode{"<>"}, the null
## hypothesis is tested against the two-sided alternative
## PROB (@var{x} < @var{y}) != 1/2.  If @var{alt} is @qcode{">"}, the one-sided
## alternative PROB (@var{x} > @var{y}) > 1/2 ("x is stochastically greater
## than y") is considered.  Similarly for @qcode{"<"}, the one-sided
## alternative PROB (@var{x} > @var{y}) < 1/2 ("x is stochastically less than
## y") is considered.  The default is the two-sided case.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Sign test

function [pval, b, n] = sign_test (x, y, alt)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (x) && isvector (y) && (length (x) == length (y))))
    error ("sign_test: X and Y must be vectors of the same length");
  endif

  n   = length (x);
  x   = reshape (x, 1, n);
  y   = reshape (y, 1, n);
  n   = sum (x != y);
  b   = sum (x > y);
  cdf = binocdf (b, n, 1/2);

  if (nargin == 2)
    alt = "!=";
  endif

  if (! ischar (alt))
    error ("sign_test: ALT must be a string");
  endif
  if (strcmp (alt, "!=") || strcmp (alt, "<>"))
    pval = 2 * min (cdf, 1 - cdf);
  elseif (strcmp (alt, ">"))
    pval = 1 - cdf;
  elseif (strcmp (alt, "<"))
    pval = cdf;
  else
    error ("sign_test: option %s not recognized", alt);
  endif

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

