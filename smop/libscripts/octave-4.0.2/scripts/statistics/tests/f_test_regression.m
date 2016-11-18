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
## @deftypefn {Function File} {[@var{pval}, @var{f}, @var{df_num}, @var{df_den}] =} f_test_regression (@var{y}, @var{x}, @var{rr}, @var{r})
## Perform an F test for the null hypothesis @nospell{rr * b = r} in a
## classical normal regression model y = X * b + e.
##
## Under the null, the test statistic @var{f} follows an F distribution with
## @var{df_num} and @var{df_den} degrees of freedom.
##
## The p-value (1 minus the CDF of this distribution at @var{f}) is returned
## in @var{pval}.
##
## If not given explicitly, @var{r} = 0.
##
## If no output argument is given, the p-value is displayed.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Test linear hypotheses in linear regression model

function [pval, f, df_num, df_den] = f_test_regression (y, x, rr, r)

  if (nargin < 3 || nargin > 4)
    print_usage ();
  endif

  [T, k] = size (x);
  if (! (isvector (y) && (length (y) == T)))
    error ("f_test_regression: Y must be a vector of length rows (X)");
  endif
  y = reshape (y, T, 1);

  [q, c_R ] = size (rr);
  if (c_R != k)
    error ("f_test_regression: RR must have as many columns as X");
  endif

  if (nargin == 4)
    s_r = size (r);
    if ((min (s_r) != 1) || (max (s_r) != q))
      error ("f_test_regression: R must be a vector of length rows (RR)");
    endif
    r = reshape (r, q, 1);
  else
    r = zeros (q, 1);
  endif

  df_num = q;
  df_den = T - k;

  [b, v] = ols (y, x);
  diff   = rr * b - r;
  f      = diff' * inv (rr * inv (x' * x) * rr') * diff / (q * v);
  pval   = 1 - fcdf (f, df_num, df_den);

  if (nargout == 0)
    printf ("  pval: %g\n", pval);
  endif

endfunction

