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
## @deftypefn {Function File} {[@var{theta}, @var{beta}, @var{dev}, @var{dl}, @var{d2l}, @var{p}] =} logistic_regression (@var{y}, @var{x}, @var{print}, @var{theta}, @var{beta})
## Perform ordinal logistic regression.
##
## Suppose @var{y} takes values in @var{k} ordered categories, and let
## @code{gamma_i (@var{x})} be the cumulative probability that @var{y}
## falls in one of the first @var{i} categories given the covariate
## @var{x}.  Then
##
## @example
## [theta, beta] = logistic_regression (y, x)
## @end example
##
## @noindent
## fits the model
##
## @example
## logit (gamma_i (x)) = theta_i - beta' * x,   i = 1 @dots{} k-1
## @end example
##
## The number of ordinal categories, @var{k}, is taken to be the number
## of distinct values of @code{round (@var{y})}.  If @var{k} equals 2,
## @var{y} is binary and the model is ordinary logistic regression.  The
## matrix @var{x} is assumed to have full column rank.
##
## Given @var{y} only, @code{theta = logistic_regression (y)}
## fits the model with baseline logit odds only.
##
## The full form is
##
## @example
## @group
## [theta, beta, dev, dl, d2l, gamma]
##    = logistic_regression (y, x, print, theta, beta)
## @end group
## @end example
##
## @noindent
## in which all output arguments and all input arguments except @var{y}
## are optional.
##
## Setting @var{print} to 1 requests summary information about the fitted
## model to be displayed.  Setting @var{print} to 2 requests information
## about convergence at each iteration.  Other values request no
## information to be displayed.  The input arguments @var{theta} and
## @var{beta} give initial estimates for @var{theta} and @var{beta}.
##
## The returned value @var{dev} holds minus twice the log-likelihood.
##
## The returned values @var{dl} and @var{d2l} are the vector of first
## and the matrix of second derivatives of the log-likelihood with
## respect to @var{theta} and @var{beta}.
##
## @var{p} holds estimates for the conditional distribution of @var{y}
## given @var{x}.
## @end deftypefn

## Original for MATLAB written by Gordon K Smyth <gks@maths.uq.oz.au>,
## U of Queensland, Australia, on Nov 19, 1990.  Last revision Aug 3,
## 1992.

## Author: Gordon K Smyth <gks@maths.uq.oz.au>,
## Adapted-By: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Ordinal logistic regression

## Uses the auxiliary functions logistic_regression_derivatives and
## logistic_regression_likelihood.

function [theta, beta, dev, dl, d2l, p] = logistic_regression (y, x, print, theta, beta)

  ## check input
  y = round (vec (y));
  [my, ny] = size (y);
  if (nargin < 2)
    x = zeros (my, 0);
  endif;
  [mx, nx] = size (x);
  if (mx != my)
    error ("logistic_regression: X and Y must have the same number of observations");
  endif

  ## initial calculations
  x = -x;
  tol = 1e-6; incr = 10; decr = 2;
  ymin = min (y); ymax = max (y); yrange = ymax - ymin;
  z  = (y * ones (1, yrange)) == ((y * 0 + 1) * (ymin : (ymax - 1)));
  z1 = (y * ones (1, yrange)) == ((y * 0 + 1) * ((ymin + 1) : ymax));
  z  = z(:, any (z));
  z1 = z1(:, any(z1));
  [mz, nz] = size (z);

  ## starting values
  if (nargin < 3)
    print = 0;
  endif;
  if (nargin < 4)
    beta = zeros (nx, 1);
  endif;
  if (nargin < 5)
    g = cumsum (sum (z))' ./ my;
    theta = log (g ./ (1 - g));
  endif;
  tb = [theta; beta];

  ## likelihood and derivatives at starting values
  [g, g1, p, dev] = logistic_regression_likelihood (y, x, tb, z, z1);
  [dl, d2l] = logistic_regression_derivatives (x, z, z1, g, g1, p);
  epsilon = std (vec (d2l)) / 1000;

  ## maximize likelihood using Levenberg modified Newton's method
  iter = 0;
  while (abs (dl' * (d2l \ dl) / length (dl)) > tol)
    iter = iter + 1;
    tbold = tb;
    devold = dev;
    tb = tbold - d2l \ dl;
    [g, g1, p, dev] = logistic_regression_likelihood (y, x, tb, z, z1);
    if ((dev - devold) / (dl' * (tb - tbold)) < 0)
      epsilon = epsilon / decr;
    else
      while ((dev - devold) / (dl' * (tb - tbold)) > 0)
        epsilon = epsilon * incr;
         if (epsilon > 1e+15)
           error ("logistic_regression: epsilon too large");
         endif
         tb = tbold - (d2l - epsilon * eye (size (d2l))) \ dl;
         [g, g1, p, dev] = logistic_regression_likelihood (y, x, tb, z, z1);
         disp ("epsilon"); disp (epsilon);
      endwhile
    endif
    [dl, d2l] = logistic_regression_derivatives (x, z, z1, g, g1, p);
    if (print == 2)
      disp ("Iteration"); disp (iter);
      disp ("Deviance"); disp (dev);
      disp ("First derivative"); disp (dl');
      disp ("Eigenvalues of second derivative"); disp (eig (d2l)');
    endif
  endwhile

  ## tidy up output

  theta = tb(1 : nz, 1);
  beta  = tb((nz + 1) : (nz + nx), 1);

  if (print >= 1)
    printf ("\n");
    printf ("Logistic Regression Results:\n");
    printf ("\n");
    printf ("Number of Iterations: %d\n", iter);
    printf ("Deviance:             %f\n", dev);
    printf ("Parameter Estimates:\n");
    printf ("     Theta         S.E.\n");
    se = sqrt (diag (inv (-d2l)));
    for i = 1 : nz
      printf ("   %8.4f     %8.4f\n", tb (i), se (i));
    endfor
    if (nx > 0)
      printf ("      Beta         S.E.\n");
      for i = (nz + 1) : (nz + nx)
        printf ("   %8.4f     %8.4f\n", tb (i), se (i));
      endfor
    endif
  endif

  if (nargout == 6)
    if (nx > 0)
      e = ((x * beta) * ones (1, nz)) + ((y * 0 + 1) * theta');
    else
      e = (y * 0 + 1) * theta';
    endif
    gamma = diff ([(y * 0), (exp (e) ./ (1 + exp (e))), (y * 0 + 1)]')';
  endif

endfunction

