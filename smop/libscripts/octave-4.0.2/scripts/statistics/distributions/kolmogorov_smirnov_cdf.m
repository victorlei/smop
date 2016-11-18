## Copyright (C) 2012 Rik Wehbring
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
## @deftypefn {Function File} {} kolmogorov_smirnov_cdf (@var{x}, @var{tol})
## Return the cumulative distribution function (CDF) at @var{x} of the
## Kolmogorov-Smirnov distribution.
##
## This is defined as
## @tex
## $$ Q(x) = \sum_{k=-\infty}^\infty (-1)^k \exp (-2 k^2 x^2) $$
## @end tex
## @ifnottex
##
## @example
## @group
##          Inf
## Q(x) =   SUM    (-1)^k exp (-2 k^2 x^2)
##        k = -Inf
## @end group
## @end example
##
## @end ifnottex
## @noindent
## for @var{x} > 0.
##
## The optional parameter @var{tol} specifies the precision up to which
## the series should be evaluated; the default is @var{tol} = @code{eps}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Kolmogorov-Smirnov distribution

function cdf = kolmogorov_smirnov_cdf (x, tol)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    if (isa (x, "single"))
      tol = eps ("single");
    else
      tol = eps;
    endif
  else
    if (! (isscalar (tol) && (tol > 0)))
      error ("kolmogorov_smirnov_cdf: TOL must be a positive scalar");
    endif
  endif

  if (numel (x) == 0)
    error ("kolmogorov_smirnov_cdf: X must not be empty");
  endif

  cdf = zeros (size (x));

  ind = find (x > 0);
  if (length (ind) > 0)
    if (columns (ind) < rows (ind))
      y = x(ind.');
    else
      y = x(ind);
    endif
    K   = ceil (sqrt (- log (tol) / 2) / min (y));
    k   = (1:K)';
    A   = exp (- 2 * k.^2 * y.^2);
    odd = find (rem (k, 2) == 1);
    A(odd,:) = -A(odd,:);
    cdf(ind) = 1 + 2 * sum (A);
  endif

endfunction


## Test input validation
%!error kolmogorov_smirnov_cdf ()
%!error kolmogorov_smirnov_cdf (1,2,3)
%!error kolmogorov_smirnov_cdf (1, ones (2))
%!error kolmogorov_smirnov_cdf ([], 1)

