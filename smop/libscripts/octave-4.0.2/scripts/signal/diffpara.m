## Copyright (C) 1995-2015 Friedrich Leisch
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
## @deftypefn {Function File} {[@var{d}, @var{dd}] =} diffpara (@var{x}, @var{a}, @var{b})
## Return the estimator @var{d} for the differencing parameter of an
## integrated time series.
##
## The frequencies from @math{[2*pi*a/t, 2*pi*b/T]} are used for the
## estimation.  If @var{b} is omitted, the interval
## @math{[2*pi/T, 2*pi*a/T]} is used.  If both @var{b} and @var{a} are omitted
## then @math{a = 0.5 * sqrt (T)} and @math{b = 1.5 * sqrt (T)} is used, where
## @math{T} is the sample size.  If @var{x} is a matrix, the differencing
## parameter of each column is estimated.
##
## The estimators for all frequencies in the intervals described above is
## returned in @var{dd}.
##
## The value of @var{d} is simply the mean of @var{dd}.
##
## Reference: @nospell{P.J. Brockwell & R.A. Davis}. @cite{Time Series:
## Theory and Methods}. Springer 1987.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Estimate the fractional differencing parameter

function [d, dd] = diffpara (x, a, b)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (isvector (x))
    n = length (x);
    k = 1;
    x = reshape (x, n, 1);
  else
    [n, k] = size (x);
  endif
  if (nargin == 1)
    a = 0.5 * sqrt (n);
    b = 1.5 * sqrt (n);
  elseif (nargin == 2)
    b = a;
    a = 1;
  endif

  if (! (isscalar (a) && isscalar (b)))
    error ("diffpara: A and B must be scalars");
  endif

  dd = zeros (b - a + 1, k);

  for l = 1:k

    w = 2 * pi * (1 : n-1) / n;

    x = 2 * log (abs (1 - exp (-i*w)));
    y = log (periodogram (x(2:n,l)));

    x = center (x);
    y = center (y);

    for m = a:b
      dd(m-a+1) = - x(1:m) * y(1:m) / sumsq (x(1:m));
    endfor

  endfor

  d = mean (dd);

endfunction

