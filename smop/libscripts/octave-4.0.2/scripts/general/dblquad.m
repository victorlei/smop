## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn  {Function File} {} dblquad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb})
## @deftypefnx {Function File} {} dblquad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{tol})
## @deftypefnx {Function File} {} dblquad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{tol}, @var{quadf})
## @deftypefnx {Function File} {} dblquad (@var{f}, @var{xa}, @var{xb}, @var{ya}, @var{yb}, @var{tol}, @var{quadf}, @dots{})
## Numerically evaluate the double integral of @var{f}.
##
## @var{f} is a function handle, inline function, or string containing the name
## of the function to evaluate.  The function @var{f} must have the form
## @math{z = f(x,y)} where @var{x} is a vector and @var{y} is a scalar.  It
## should return a vector of the same length and orientation as @var{x}.
##
## @var{xa}, @var{ya} and @var{xb}, @var{yb} are the lower and upper limits of
## integration for x and y respectively.  The underlying integrator determines
## whether infinite bounds are accepted.
##
## The optional argument @var{tol} defines the absolute tolerance used to
## integrate each sub-integral.  The default value is @math{1e^{-6}}.
##
## The optional argument @var{quadf} specifies which underlying integrator
## function to use.  Any choice but @code{quad} is available and the default
## is @code{quadcc}.
##
## Additional arguments, are passed directly to @var{f}.  To use the default
## value for @var{tol} or @var{quadf} one may pass @qcode{':'} or an empty
## matrix ([]).
## @seealso{triplequad, quad, quadv, quadl, quadgk, quadcc, trapz}
## @end deftypefn

function q = dblquad (f, xa, xb, ya, yb, tol = 1e-6, quadf = @quadcc, varargin)

  if (nargin < 5)
    print_usage ();
  endif
  if (isempty (tol))
    tol = 1e-6;
  endif
  if (isempty (quadf))
    quadf = @quadcc;
  endif

  inner = @__dblquad_inner__;
  if (ischar (f))
    f = @(x,y) feval (f, x, y, varargin{:});
    varargin = {};
  endif

  q = feval (quadf, @(y) inner (y, f, xa, xb, tol, quadf,
                                varargin{:}), ya, yb, tol);
endfunction

function q = __dblquad_inner__ (y, f, xa, xb, tol, quadf, varargin)
  q = zeros (size (y));
  for i = 1 : length (y)
    q(i) = feval (quadf, @(x) f(x, y(i), varargin{:}), xa, xb, tol);
  endfor
endfunction


## Nasty integrand to show quadcc off
%!assert (dblquad (@(x,y) 1 ./ (x+y), 0, 1, 0, 1), 2*log (2), 1e-6)

%!assert (dblquad (@(x,y) exp (-x.^2 - y.^2) , -1, 1, -1, 1, 1e-6, @quadgk), pi * erf (1).^2, 1e-6)
%!assert (dblquad (@(x,y) exp (-x.^2 - y.^2) , -1, 1, -1, 1, 1e-6, @quadl), pi * erf (1).^2, 1e-6)
%!assert (dblquad (@(x,y) exp (-x.^2 - y.^2) , -1, 1, -1, 1, 1e-6, @quadv), pi * erf (1).^2, 1e-6)

