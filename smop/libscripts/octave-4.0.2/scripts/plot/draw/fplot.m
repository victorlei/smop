## Copyright (C) 2005-2015 Paul Kienzle
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
## @deftypefn  {Function File} {} fplot (@var{fn}, @var{limits})
## @deftypefnx {Function File} {} fplot (@dots{}, @var{tol})
## @deftypefnx {Function File} {} fplot (@dots{}, @var{n})
## @deftypefnx {Function File} {} fplot (@dots{}, @var{fmt})
## @deftypefnx {Function File} {[@var{x}, @var{y}] =} fplot (@dots{})
## Plot a function @var{fn} within the range defined by @var{limits}.
##
## @var{fn} is a function handle, inline function, or string containing the
## name of the function to evaluate.
##
## The limits of the plot are of the form @w{@code{[@var{xlo}, @var{xhi}]}} or
## @w{@code{[@var{xlo}, @var{xhi}, @var{ylo}, @var{yhi}]}}.
##
## The next three arguments are all optional and any number of them may be
## given in any order.
##
## @var{tol} is the relative tolerance to use for the plot and defaults
## to 2e-3 (.2%).
##
## @var{n} is the minimum number of points to use.  When @var{n} is specified,
## the maximum stepsize will be @code{@var{xhi} - @var{xlo} / @var{n}}.  More
## than @var{n} points may still be used in order to meet the relative
## tolerance requirement.
##
## The @var{fmt} argument specifies the linestyle to be used by the plot
## command.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## With no output arguments the results are immediately plotted.  With two
## output arguments the 2-D plot data is returned.  The data can subsequently
## be plotted manually with @code{plot (@var{x}, @var{y})}.
##
## Example:
##
## @example
## @group
## fplot (@@cos, [0, 2*pi])
## fplot ("[cos(x), sin(x)]", [0, 2*pi])
## @end group
## @end example
##
## Programming Notes:
##
## @code{fplot} works best with continuous functions.  Functions with
## discontinuities are unlikely to plot well.  This restriction may be removed
## in the future.
##
## @code{fplot} requires that the function accept and return a vector argument.
## Consider this when writing user-defined functions and use @code{.*},
## @code{./}, etc.  See the function @code{vectorize} for potentially
## converting inline or anonymous functions to vectorized versions.
##  
## @seealso{ezplot, plot, vectorize}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function [X, Y] = fplot (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("fplot", varargin{:});

  if (nargin < 2 || nargin > 5)
    print_usage ();
  endif

  fn = varargin{1};
  limits = varargin{2};
  varargin = varargin(3:end);

  if (strcmp (typeinfo (fn), "inline function"))
    fn = vectorize (fn);
    nam = formula (fn);
  elseif (isa (fn, "function_handle"))
    nam = func2str (fn);
  elseif (all (isalnum (fn)))
    nam = fn;
  elseif (ischar (fn))
    fn = vectorize (inline (fn));
    nam = formula (fn);
  else
    error ("fplot: FN must be a function handle, inline function, or string");
  endif

  if (iscomplex (limits) || (numel (limits) != 2 && numel (limits) != 4))
    error ("fplot: LIMITS must be a real vector with 2 or 4 elements");
  endif

  n = 5;
  tol = 2e-3;
  fmt = "";
  for i = 1:numel (varargin)
    arg = varargin{i};
    if (ischar (arg))
      fmt = arg;
    elseif (isnumeric (arg) && isscalar (arg) && arg > 0)
      if (arg == fix (arg))
        n = arg;
      else
        tol = arg;
      endif
    else
      error ("fplot: bad input in position %d", i+2);
    endif
  endfor

  if (n != 5)
    ## n was specified
    x0 = linspace (limits(1), limits(2), n/2 + 1)';
    y0 = feval (fn, x0);
    x = linspace (limits(1), limits(2), n)';
    y = feval (fn, x);
  else
    x0 = linspace (limits(1), limits(2), 5)';
    y0 = feval (fn, x0);
    n = 8;
    x = linspace (limits(1), limits(2), n)';
    y = feval (fn, x);
  endif

  if (rows (x0) != rows (y0))
    ## FN is a constant value function
    y0 = repmat (y0, size (x0));
    y = repmat (y, size (x));
  endif

  err0 = Inf;

  ## FIXME: This algorithm should really use adaptive scaling as the
  ##        the numerical quadrature algorithms do so that extra points are
  ##        used where they are needed and not spread evenly over the entire
  ##        x-range.  Try any function with a discontinuity, such as
  ##        fplot (@tan, [-2, 2]) or fplot ("1./x", [-3, 2]), to see the
  ##        problems with the current solution.

  while (n < 2^18)    # Something is wrong if we need more than 250K points
    yi = interp1 (x0, y0, x, "linear");
    ## relative error calculation using average of [yi,y] as reference
    ## since neither estimate is known a priori to be better than the other.
    err = 0.5 * max (abs ((yi - y) ./ (yi + y))(:));
    if (err < tol || abs (err - err0) < tol/2)
      ## Either relative tolerance has been met OR
      ## algorithm has stopped making any reasonable progress per iteration.
      break;
    endif
    x0 = x;
    y0 = y;
    err0 = err;
    n = 2 * (n - 1) + 1;
    x = linspace (limits(1), limits(2), n)';
    y = feval (fn, x);
  endwhile

  if (nargout == 2)
    X = x;
    Y = y;
  else
    if (isempty (hax))
      hax = gca ();
    endif
    plot (hax, x, y, fmt);
    axis (hax, limits);
    if (isvector (y))
      legend (hax, nam);
    else
      for i = 1:columns (y)
        nams{i} = sprintf ("%s(:,%i)", nam, i);
      endfor
      legend (hax, nams{:});
    endif
  endif

endfunction


%!demo
%! clf;
%! fplot (@cos, [0, 2*pi]);
%! title ('fplot() single function');

%!demo
%! clf;
%! fplot ('[cos(x), sin(x)]', [0, 2*pi]);
%! title ('fplot() multiple functions');

%!demo
%! clf;
%! %% sinc function
%! fh = @(x) sin (pi*x) ./ (pi*x);
%! fplot (fh, [-5, 5]);
%! title ('fplot() sinc function');

%!test
%! [x, y] = fplot ("[cos(x), sin(x)]", [0, 2*pi]);
%! assert (columns (y) == 2);
%! assert (rows (x) == rows (y));
%! assert (y, [cos(x), sin(x)], -2e-3);

## Test input validation
%!error fplot (1)
%!error fplot (1,2,3,4,5,6)
%!error <FN must be a function handle> fplot (1, [0 1])
%!error <LIMITS must be a real vector> fplot (@cos, [i, 2*i])
%!error <LIMITS must be a real vector with 2 or 4> fplot (@cos, [1])
%!error <LIMITS must be a real vector with 2 or 4> fplot (@cos, [1 2 3])
%!error <bad input in position 3> fplot (@cos,[-1,1], {1})

