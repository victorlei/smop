## Copyright (C) 2008-2015 David Bateman
## Copyright (C) 2012 Alexander Klein
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
## @deftypefn  {Function File} {@var{q} =} quadv (@var{f}, @var{a}, @var{b})
## @deftypefnx {Function File} {@var{q} =} quadv (@var{f}, @var{a}, @var{b}, @var{tol})
## @deftypefnx {Function File} {@var{q} =} quadv (@var{f}, @var{a}, @var{b}, @var{tol}, @var{trace})
## @deftypefnx {Function File} {@var{q} =} quadv (@var{f}, @var{a}, @var{b}, @var{tol}, @var{trace}, @var{p1}, @var{p2}, @dots{})
## @deftypefnx {Function File} {[@var{q}, @var{nfun}] =} quadv (@dots{})
##
## Numerically evaluate the integral of @var{f} from @var{a} to @var{b}
## using an adaptive Simpson's rule.
##
## @var{f} is a function handle, inline function, or string containing the name
## of the function to evaluate.  @code{quadv} is a vectorized version of
## @code{quad} and the function defined by @var{f} must accept a scalar or
## vector as input and return a scalar, vector, or array as output.
##
## @var{a} and @var{b} are the lower and upper limits of integration.  Both
## limits must be finite.
##
## The optional argument @var{tol} defines the absolute tolerance used to stop
## the adaptation procedure.  The default value is 1e-6.
##
## The algorithm used by @code{quadv} involves recursively subdividing the
## integration interval and applying Simpson's rule on each subinterval.
## If @var{trace} is true then after computing each of these partial
## integrals display: (1) the total number of function evaluations,
## (2) the left end of the subinterval, (3) the length of the subinterval,
## (4) the approximation of the integral over the subinterval.
##
## Additional arguments @var{p1}, etc., are passed directly to the function
## @var{f}.  To use default values for @var{tol} and @var{trace}, one may pass
## empty matrices ([]).
##
## The result of the integration is returned in @var{q}
##
## @var{nfun} indicates the number of function evaluations that were made.
##
## Note: @code{quadv} is written in Octave's scripting language and can be
## used recursively in @code{dblquad} and @code{triplequad}, unlike the
## @code{quad} function.
## @seealso{quad, quadl, quadgk, quadcc, trapz, dblquad, triplequad}
## @end deftypefn

function [q, nfun] = quadv (f, a, b, tol, trace, varargin)
  ## TODO: Make norm for convergence testing configurable

  if (nargin < 3)
    print_usage ();
  endif
  if (nargin < 4)
    tol = [];
  endif
  if (nargin < 5)
    trace = [];
  endif
  if (isa (a, "single") || isa (b, "single"))
    myeps = eps ("single");
  else
    myeps = eps;
  endif
  if (isempty (tol))
    tol = 1e-6;
  endif
  if (isempty (trace))
    trace = 0;
  endif

  ## Split the interval into 3 abscissa, and apply a 3 point Simpson's rule
  c = (a + b) / 2;
  fa = feval (f, a, varargin{:});
  fc = feval (f, c, varargin{:});
  fb = feval (f, b, varargin{:});
  nfun = 3;

  ## If have edge singularities, move edge point by eps*(b-a) as
  ## discussed in Shampine paper used to implement quadgk
  if (any (isinf (fa(:))))
    fa = feval (f, a + myeps * (b-a), varargin{:});
  endif
  if (any (isinf (fb(:))))
    fb = feval (f, b - myeps * (b-a), varargin{:});
  endif

  h = (b - a);
  q = (b - a) / 6 * (fa + 4 * fc + fb);

  [q, nfun, hmin] = simpsonstp (f, a, b, c, fa, fb, fc, q, nfun, abs (h),
                                tol, trace, varargin{:});

  if (nfun > 10000)
    warning ("maximum iteration count reached");
  elseif (any (! isfinite (q(:))))
    warning ("infinite or NaN function evaluations were returned");
  elseif (hmin < (b - a) * myeps)
    warning ("minimum step size reached -- possibly singular integral");
  endif
endfunction

function [q, nfun, hmin] = simpsonstp (f, a, b, c, fa, fb, fc, q0,
                                       nfun, hmin, tol, trace, varargin)
  if (nfun > 10000)
    q = q0;
  else
    d = (a + c) / 2;
    e = (c + b) / 2;
    fd = feval (f, d, varargin{:});
    fe = feval (f, e, varargin{:});
    nfun += 2;
    q1 = (c - a) / 6 * (fa + 4 * fd + fc);
    q2 = (b - c) / 6 * (fc + 4 * fe + fb);
    q = q1 + q2;

    if (abs(a -  c) < hmin)
      hmin = abs (a - c);
    endif

    if (trace)
      disp ([nfun, a, b-a, q]);
    endif

    ## Force at least one adpative step.
    ## Not vectorizing q-q0 in the norm provides a more rigid criterion for
    ## matrix-valued functions.
    if (nfun == 5 || norm (q - q0, Inf) > tol)
      [q1, nfun, hmin] = simpsonstp (f, a, c, d, fa, fc, fd, q1, nfun, hmin,
                                    tol, trace, varargin{:});
      [q2, nfun, hmin] = simpsonstp (f, c, b, e, fc, fb, fe, q2, nfun, hmin,
                                     tol, trace, varargin{:});
      q = q1 + q2;
    endif
  endif
endfunction


%!assert (quadv (@sin, 0, 2 * pi), 0, 1e-5)
%!assert (quadv (@sin, 0, pi), 2, 1e-5)

## Handles weak singularities at the edge
%!assert (quadv (@(x) 1 ./ sqrt (x), 0, 1), 2, 1e-5)

## Handles vector-valued functions
%!assert (quadv (@(x) [(sin (x)), (sin (2 * x))], 0, pi), [2, 0], 1e-5)

## Handles matrix-valued functions
%!assert (quadv (@(x) [ x, x, x; x, 1./sqrt(x), x; x, x, x ], 0, 1 ), [0.5, 0.5, 0.5; 0.5, 2, 0.5; 0.5, 0.5, 0.5], 1e-5)

