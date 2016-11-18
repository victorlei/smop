## Copyright (C) 2000-2015 Kai Habel
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
## @deftypefn  {Function File} {@var{q} =} trapz (@var{y})
## @deftypefnx {Function File} {@var{q} =} trapz (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{q} =} trapz (@dots{}, @var{dim})
##
## Numerically evaluate the integral of points @var{y} using the trapezoidal
## method.
##
## @w{@code{trapz (@var{y})}} computes the integral of @var{y} along the first
## non-singleton dimension.  When the argument @var{x} is omitted an equally
## spaced @var{x} vector with unit spacing (1) is assumed.
## @code{trapz (@var{x}, @var{y})} evaluates the integral with respect to the
## spacing in @var{x} and the values in @var{y}.  This is useful if the points
## in @var{y} have been sampled unevenly.
##
## If the optional @var{dim} argument is given, operate along this dimension.
##
## Application Note: If @var{x} is not specified then unit spacing will be
## used.  To scale the integral to the correct value you must multiply by the
## actual spacing value (deltaX).  As an example, the integral of @math{x^3}
## over the range [0, 1] is @math{x^4/4} or 0.25.  The following code uses
## @code{trapz} to calculate the integral in three different ways.
##
## @example
## @group
## x = 0:0.1:1;
## y = x.^3;
## q = trapz (y)
##   @result{} q = 2.525   # No scaling
## q * 0.1
##   @result{} q = 0.2525  # Approximation to integral by scaling
## trapz (x, y)
##   @result{} q = 0.2525  # Same result by specifying @var{x}
## @end group
## @end example
##
## @seealso{cumtrapz}
## @end deftypefn

## Author:      Kai Habel <kai.habel@gmx.de>
##
## also: June 2000 - Paul Kienzle (fixes,suggestions)
## 2006-05-12 David Bateman - Modified for NDArrays

function z = trapz (x, y, dim)

  if (nargin < 1) || (nargin > 3)
    print_usage ();
  endif

  have_xy = have_dim = false;

  if (nargin == 3)
    have_xy = true;
    have_dim = true;
  elseif (nargin == 2)
    if (! size_equal (x, y) && isscalar (y))
      dim = y;
      have_dim = true;
    else
      have_xy = true;
    endif
  endif

  if (have_xy)
    nd = ndims (y);
    sz = size (y);
  else
    nd = ndims (x);
    sz = size (x);
  endif

  if (! have_dim)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("trapz: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);
  idx1 = idx2 = repmat ({':'}, [nd, 1]);
  idx1{dim} = 2 : n;
  idx2{dim} = 1 : (n - 1);

  if (! have_xy)
    z = 0.5 * sum (x(idx1{:}) + x(idx2{:}), dim);
  else
    if (isvector (x) && ! isvector (y))
      if (length (x) != sz(dim))
        error ("trapz: length of X and length of Y along DIM must match");
      endif
      ## Reshape vector to point along dimension DIM
      shape = ones (nd, 1);
      shape(dim) = sz(dim);
      x = reshape (x, shape);
      z = 0.5 * sum (bsxfun (@times, diff (x), y(idx1{:}) + y(idx2{:})), dim);
    else
      if (! size_equal (x, y))
        error ("trapz: X and Y must have same shape");
      endif
      z = 0.5 * sum (diff (x, 1, dim) .* (y(idx1{:}) + y(idx2{:})), dim);
    endif
  endif
endfunction


%!assert (trapz (1:5), 12)
%!assert (trapz (0:0.5:2,1:5), 6)
%!assert (trapz ([1:5;1:5].',1), [12,12])
%!assert (trapz ([1:5;1:5],2), [12;12])
%!assert (trapz (repmat (reshape (1:5,1,1,5),2,2), 3), [12 12; 12 12])
%!assert (trapz ([0:0.5:2;0:0.5:2].',[1:5;1:5].',1), [6, 6])
%!assert (trapz ([0:0.5:2;0:0.5:2],[1:5;1:5],2), [6; 6])
%!assert (trapz (repmat (reshape ([0:0.5:2],1,1,5),2,2), ...
%!               repmat (reshape (1:5,1,1,5),2,2), 3), [6 6; 6 6])
%!assert (trapz (0:0.5:2,[(1:5)',(1:5)']), [6, 6])
%!assert (trapz (0:0.5:2,[(1:5);(1:5)],2), [6; 6])
%!assert (trapz (0:0.5:2,repmat (reshape (1:5,1,1,5),2,2),3), [6 6; 6 6])

