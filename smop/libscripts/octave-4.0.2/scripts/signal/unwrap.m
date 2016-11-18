## Copyright (C) 2000-2015 Bill Lash
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
## @deftypefn  {Function File} {@var{b} =} unwrap (@var{x})
## @deftypefnx {Function File} {@var{b} =} unwrap (@var{x}, @var{tol})
## @deftypefnx {Function File} {@var{b} =} unwrap (@var{x}, @var{tol}, @var{dim})
##
## Unwrap radian phases by adding multiples of 2*pi as appropriate to remove
## jumps greater than @var{tol}.
##
## @var{tol} defaults to pi.
##
## Unwrap will work along the dimension @var{dim}.  If @var{dim}
## is unspecified it defaults to the first non-singleton dimension.
## @end deftypefn

## Author: Bill Lash <lash@tellabs.com>

function retval = unwrap (x, tol, dim)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! isnumeric (x))
    error ("unwrap: X must be a numeric matrix or vector");
  endif

  if (nargin < 2 || isempty (tol))
    tol = pi;
  endif

  ## Don't let anyone use a negative value for TOL.
  tol = abs (tol);

  nd = ndims (x);
  sz = size (x);
  if (nargin == 3)
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("unwrap: DIM must be an integer and a valid dimension");
    endif
  else
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  endif

  rng = 2*pi;
  m = sz(dim);

  ## Handle case where we are trying to unwrap a scalar, or only have
  ## one sample in the specified dimension.
  if (m == 1)
    retval = x;
    return;
  endif

  ## Take first order difference to see so that wraps will show up
  ## as large values, and the sign will show direction.
  idx = repmat ({':'}, nd, 1);
  idx{dim} = [1,1:m-1];
  d = x(idx{:}) - x;

  ## Find only the peaks, and multiply them by the appropriate amount
  ## of ranges so that there are kronecker deltas at each wrap point
  ## multiplied by the appropriate amount of range values.
  p = ceil (abs (d)./rng) .* rng .* (((d > tol) > 0) - ((d < -tol) > 0));

  ## Now need to "integrate" this so that the deltas become steps.
  r = cumsum (p, dim);

  ## Now add the "steps" to the original data and put output in the
  ## same shape as originally.
  retval = x + r;

endfunction


%!function t = __xassert (a,b,tol)
%!  if (nargin == 1)
%!    t = all (a(:));
%!  else
%!    if (nargin == 2)
%!      tol = 0;
%!    endif
%!    if (any (size (a) != size (b)))
%!      t = 0;
%!    elseif (any (abs (a(:) - b(:)) > tol))
%!      t = 0;
%!    else
%!      t = 1;
%!    endif
%!  endif
%!endfunction
%!
%!test
%!
%! i = 0;
%! t = [];
%!
%! r = [0:100];                        # original vector
%! w = r - 2*pi*floor ((r+pi)/(2*pi)); # wrapped into [-pi,pi]
%! tol = 1e3*eps;                      # maximum expected deviation
%!
%! t(++i) = __xassert (r, unwrap (w), tol);              #unwrap single row
%! t(++i) = __xassert (r', unwrap (w'), tol);            #unwrap single column
%! t(++i) = __xassert ([r',r'], unwrap ([w',w']), tol);  #unwrap 2 columns
%! t(++i) = __xassert ([r;r], unwrap ([w;w],[],2), tol); #check that dim works
%! t(++i) = __xassert (r+10, unwrap (10+w), tol);        #check r(1)>pi works
%!
%! t(++i) = __xassert (w', unwrap (w',[],2));  #unwrap col by rows should not change it
%! t(++i) = __xassert (w, unwrap (w,[],1));    #unwrap row by cols should not change it
%! t(++i) = __xassert ([w;w], unwrap ([w;w])); #unwrap 2 rows by cols should not change them
%!
%! ## verify that setting tolerance too low will cause bad results.
%! t(++i) = __xassert (any (abs (r - unwrap (w,0.8)) > 100));
%!
%! assert (all (t));
%!
%!test
%! A = [pi*(-4), pi*(-2+1/6), pi/4, pi*(2+1/3), pi*(4+1/2), pi*(8+2/3), pi*(16+1), pi*(32+3/2), pi*64];
%! assert (unwrap (A), unwrap (A, pi));
%! assert (unwrap (A, pi), unwrap (A, pi, 2));
%! assert (unwrap (A', pi), unwrap (A', pi, 1));
%!
%!test
%! A = [pi*(-4); pi*(2+1/3); pi*(16+1)];
%! B = [pi*(-2+1/6); pi*(4+1/2); pi*(32+3/2)];
%! C = [pi/4; pi*(8+2/3); pi*64];
%! D = [pi*(-2+1/6); pi*(2+1/3); pi*(8+2/3)];
%! E(:, :, 1) = [A, B, C, D];
%! E(:, :, 2) = [A+B, B+C, C+D, D+A];
%! F(:, :, 1) = [unwrap(A), unwrap(B), unwrap(C), unwrap(D)];
%! F(:, :, 2) = [unwrap(A+B), unwrap(B+C), unwrap(C+D), unwrap(D+A)];
%! assert (unwrap (E), F);
%!
%!test
%! A = [0, 2*pi, 4*pi, 8*pi, 16*pi, 65536*pi];
%! B = [pi*(-2+1/6), pi/4, pi*(2+1/3), pi*(4+1/2), pi*(8+2/3), pi*(16+1), pi*(32+3/2), pi*64];
%! assert (unwrap (A), zeros (1, length (A)));
%! assert (diff (unwrap (B), 1) < 2*pi, true (1, length (B)-1));

%!error unwrap()

