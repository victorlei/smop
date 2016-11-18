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
## @deftypefn {Function File} {[@var{a}, @var{b}] =} arch_fit (@var{y}, @var{x}, @var{p}, @var{iter}, @var{gamma}, @var{a0}, @var{b0})
## Fit an ARCH regression model to the time series @var{y} using the scoring
## algorithm in @nospell{Engle's} original ARCH paper.
##
## The model is
##
## @example
## @group
## y(t) = b(1) * x(t,1) + @dots{} + b(k) * x(t,k) + e(t),
## h(t) = a(1) + a(2) * e(t-1)^2 + @dots{} + a(p+1) * e(t-p)^2
## @end group
## @end example
##
## @noindent
## in which @math{e(t)} is @math{N(0, h(t))}, given a time-series vector
## @var{y} up to time @math{t-1} and a matrix of (ordinary) regressors @var{x}
## up to @math{t}.  The order of the regression of the residual variance is
## specified by @var{p}.
##
## If invoked as @code{arch_fit (@var{y}, @var{k}, @var{p})} with a positive
## integer @var{k}, fit an ARCH(@var{k}, @var{p}) process, i.e., do the above
## with the @math{t}-th row of @var{x} given by
##
## @example
## [1, y(t-1), @dots{}, y(t-k)]
## @end example
##
## Optionally, one can specify the number of iterations @var{iter}, the
## updating factor @var{gamma}, and initial values @math{a0} and @math{b0}
## for the scoring algorithm.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Fit an ARCH regression model

function [a, b] = arch_fit (y, x, p, iter, gamma, a0, b0)

  if (nargin < 3 || nargin == 6 || nargin > 7)
    print_usage ();
  endif

  if (! (isvector (y)))
    error ("arch_fit: Y must be a vector");
  endif

  T = length (y);
  y = reshape (y, T, 1);
  [rx, cx] = size (x);
  if ((rx == 1) && (cx == 1))
    x = autoreg_matrix (y, x);
  elseif (! (rx == T))
    error ("arch_fit: either rows (X) == length (Y), or X is a scalar");
  endif

  [T, k] = size (x);

  if (nargin == 7)
    a = a0;
    b = b0;
    e = y - x * b;
  else
    [b, v_b, e] = ols (y, x);
    a = [v_b, (zeros (1, p))]';
    if (nargin < 5)
      gamma = 0.1;
      if (nargin < 4)
        iter = 50;
      endif
    endif
  endif

  esq = e.^2;
  Z = autoreg_matrix (esq, p);

  for i = 1 : iter;
    h   = Z * a;
    tmp = esq ./ h.^2 - 1 ./ h;
    s   = 1 ./ h(1:T-p);
    for j = 1 : p;
      s = s - a(j+1) * tmp(j+1:T-p+j);
    endfor
    r = 1 ./ h(1:T-p);
    for j = 1:p;
      r = r + 2 * h(j+1:T-p+j).^2 .* esq(1:T-p);
    endfor
    r = sqrt (r);
    X_tilde = x(1:T-p, :) .* (r * ones (1,k));
    e_tilde = e(1:T-p) .*s ./ r;
    delta_b = inv (X_tilde' * X_tilde) * X_tilde' * e_tilde;
    b   = b + gamma * delta_b;
    e   = y - x * b;
    esq = e .^ 2;
    Z   = autoreg_matrix (esq, p);
    h   = Z * a;
    f   = esq ./ h - ones (T,1);
    Z_tilde = Z ./ (h * ones (1, p+1));
    delta_a = inv (Z_tilde' * Z_tilde) * Z_tilde' * f;
    a = a + gamma * delta_a;
  endfor

endfunction

