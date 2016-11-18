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
## @deftypefn {Function File} {[@var{dl}, @var{d2l}] =} logistic_regression_derivatives (@var{x}, @var{z}, @var{z1}, @var{g}, @var{g1}, @var{p})
## Calculate derivatives of the log-likelihood for ordinal logistic regression
## model.
##
## Private function called by @code{logistic_regression}.
## @seealso{logistic_regression}
## @end deftypefn

## Author: Gordon K. Smyth <gks@maths.uq.oz.au>
## Adapted-By: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Derivates of log-likelihood in logistic regression

function [dl, d2l] = logistic_regression_derivatives (x, z, z1, g, g1, p)

  ## first derivative
  v = g .* (1 - g) ./ p; v1 = g1 .* (1 - g1) ./ p;
  dlogp = [(diag (v) * z - diag (v1) * z1), (diag (v - v1) * x)];
  dl = sum (dlogp)';

  ## second derivative
  w = v .* (1 - 2 * g); w1 = v1 .* (1 - 2 * g1);
  d2l = [z, x]' * diag (w) * [z, x] - [z1, x]' * diag (w1) * [z1, x] ...
      - dlogp' * dlogp;

endfunction

