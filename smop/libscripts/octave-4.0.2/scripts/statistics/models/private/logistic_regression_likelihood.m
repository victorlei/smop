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
## @deftypefn {Function File} {[@var{g}, @var{g1}, @var{p}, @var{dev}] =} logistic_regression_likelihood (@var{y}, @var{x}, @var{beta}, @var{z}, @var{z1})
## Calculate the likelihood for the ordinal logistic regression model.
##
## Private function called by @code{logistic_regression}.
## @seealso{logistic_regression}
## @end deftypefn

## Author: Gordon K. Smyth <gks@maths.uq.oz.au>
## Adapted-By: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Likelihood in logistic regression

function [g, g1, p, dev] = logistic_regression_likelihood (y, x, beta, z, z1)

  e = exp ([z, x] * beta); e1 = exp ([z1, x] * beta);
  g = e ./ (1 + e); g1 = e1 ./ (1 + e1);
  g = max (y == max (y), g); g1 = min (y > min (y), g1);

  p = g - g1;
  dev = -2 * sum (log (p));

endfunction

