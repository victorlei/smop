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
## @deftypefn {Function File} {[@var{pval}, @var{tsq}] =} hotelling_test (@var{x}, @var{m})
## For a sample @var{x} from a multivariate normal distribution with unknown
## mean and covariance matrix, test the null hypothesis that
## @code{mean (@var{x}) == @var{m}}.
##
## Hotelling's @math{T^2} is returned in @var{tsq}.  Under the null,
## @math{(n-p) T^2 / (p(n-1))} has an F distribution with @math{p} and
## @math{n-p} degrees of freedom, where @math{n} and @math{p} are the
## numbers of samples and variables, respectively.
##
## The p-value of the test is returned in @var{pval}.
##
## If no output argument is given, the p-value of the test is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Test for mean of a multivariate normal

function [pval, Tsq] = hotelling_test (x, m)

  if (nargin != 2)
    print_usage ();
  endif

  if (isvector (x))
    if (! isscalar (m))
      error ("hotelling_test: if X is a vector, M must be a scalar");
    endif
    n = length (x);
    p = 1;
  elseif (ismatrix (x))
    [n, p] = size (x);
    if (n <= p)
      error ("hotelling_test: X must have more rows than columns");
    endif
    if (isvector (m) && length (m) == p)
      m = reshape (m, 1, p);
    else
      error ("hotelling_test: if X is a matrix, M must be a vector of length columns (X)");
    endif
  else
    error ("hotelling_test: X must be a matrix or vector");
  endif

  d    = mean (x) - m;
  Tsq  = n * d * (cov (x) \ d');
  pval = 1 - fcdf ((n-p) * Tsq / (p * (n-1)), p, n-p);

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

