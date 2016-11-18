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
## @deftypefn  {Function File} {@var{dx} =} gradient (@var{m})
## @deftypefnx {Function File} {[@var{dx}, @var{dy}, @var{dz}, @dots{}] =} gradient (@var{m})
## @deftypefnx {Function File} {[@dots{}] =} gradient (@var{m}, @var{s})
## @deftypefnx {Function File} {[@dots{}] =} gradient (@var{m}, @var{x}, @var{y}, @var{z}, @dots{})
## @deftypefnx {Function File} {[@dots{}] =} gradient (@var{f}, @var{x0})
## @deftypefnx {Function File} {[@dots{}] =} gradient (@var{f}, @var{x0}, @var{s})
## @deftypefnx {Function File} {[@dots{}] =} gradient (@var{f}, @var{x0}, @var{x}, @var{y}, @dots{})
##
## Calculate the gradient of sampled data or a function.
##
## If @var{m} is a vector, calculate the one-dimensional gradient of @var{m}.
## If @var{m} is a matrix the gradient is calculated for each dimension.
##
## @code{[@var{dx}, @var{dy}] = gradient (@var{m})} calculates the
## one-dimensional gradient for @var{x} and @var{y} direction if @var{m} is a
## matrix.  Additional return arguments can be use for multi-dimensional
## matrices.
##
## A constant spacing between two points can be provided by the @var{s}
## parameter.  If @var{s} is a scalar, it is assumed to be the spacing for all
## dimensions.  Otherwise, separate values of the spacing can be supplied by
## the @var{x}, @dots{} arguments.  Scalar values specify an equidistant
## spacing.  Vector values for the @var{x}, @dots{} arguments specify the
## coordinate for that dimension.  The length must match their respective
## dimension of @var{m}.
##
## At boundary points a linear extrapolation is applied.  Interior points
## are calculated with the first approximation of the numerical gradient
##
## @example
## y'(i) = 1/(x(i+1)-x(i-1)) * (y(i-1)-y(i+1)).
## @end example
##
## If the first argument @var{f} is a function handle, the gradient of the
## function at the points in @var{x0} is approximated using central difference.
## For example, @code{gradient (@@cos, 0)} approximates the gradient of the
## cosine function in the point @math{x0 = 0}.  As with sampled data, the
## spacing values between the points from which the gradient is estimated can
## be set via the @var{s} or @var{dx}, @var{dy}, @dots{} arguments.  By default
## a spacing of 1 is used.
## @seealso{diff, del2}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>
## Modified: David Bateman <dbateman@free.fr> Added NDArray support

function varargout = gradient (m, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  nargout_with_ans = max (1,nargout);
  if (isnumeric (m))
    [varargout{1:nargout_with_ans}] = matrix_gradient (m, varargin{:});
  elseif (isa (m, "function_handle"))
    [varargout{1:nargout_with_ans}] = handle_gradient (m, varargin{:});
  elseif (ischar (m))
    [varargout{1:nargout_with_ans}] = handle_gradient (str2func (m), ...
                                                       varargin{:});
  else
    error ("gradient: first input must be an array or a function");
  endif

endfunction

function varargout = matrix_gradient (m, varargin)
  transposed = false;
  if (isvector (m))
    ## make a row vector.
    transposed = (columns (m) == 1);
    m = m(:).';
  endif

  nd = ndims (m);
  sz = size (m);
  if (length (sz) > 1)
    tmp = sz(1); sz(1) = sz(2); sz(2) = tmp;
  endif

  if (nargin > 2 && nargin != nd + 1)
    print_usage ();
  endif

  ## cell d stores a spacing vector for each dimension
  d = cell (1, nd);
  if (nargin == 1)
    ## no spacing given - assume 1.0 for all dimensions
    for i = 1:nd
      d{i} = ones (sz(i) - 1, 1);
    endfor
  elseif (nargin == 2)
    if (isscalar (varargin{1}))
      ## single scalar value for all dimensions
      for i = 1:nd
        d{i} = varargin{1} * ones (sz(i) - 1, 1);
      endfor
    else
      ## vector for one-dimensional derivative
      d{1} = diff (varargin{1}(:));
    endif
  else
    ## have spacing value for each dimension
    if (length(varargin) != nd)
      error ("gradient: dimensions and number of spacing values do not match");
    endif
    for i = 1:nd
      if (isscalar (varargin{i}))
        d{i} = varargin{i} * ones (sz(i) - 1, 1);
      else
        d{i} = diff (varargin{i}(:));
      endif
    endfor
  endif

  m = shiftdim (m, 1);
  for i = 1:min (nd, nargout)
    mr = rows (m);
    mc = numel (m) / mr;
    Y = zeros (size (m), class (m));

    if (mr > 1)
      ## Top and bottom boundary.
      Y(1,:) = diff (m(1:2, :)) / d{i}(1);
      Y(mr,:) = diff (m(mr-1:mr, :) / d{i}(mr - 1));
    endif

    if (mr > 2)
      ## Interior points.
      Y(2:mr-1,:) = ((m(3:mr,:) - m(1:mr-2,:))
          ./ kron (d{i}(1:mr-2) + d{i}(2:mr-1), ones (1, mc)));
    endif

    ## turn multi-dimensional matrix in a way, that gradient
    ## along x-direction is calculated first then y, z, ...

    if (i == 1)
      varargout{i} = shiftdim (Y, nd - 1);
      m = shiftdim (m, nd - 1);
    elseif (i == 2)
      varargout{i} = Y;
      m = shiftdim (m, 2);
    else
      varargout{i} = shiftdim (Y, nd - i + 1);
      m = shiftdim (m, 1);
    endif
  endfor

  if (transposed)
    varargout{1} = varargout{1}.';
  endif
endfunction

function varargout = handle_gradient (f, p0, varargin)
  ## Input checking
  p0_size = size (p0);

  if (numel (p0_size) != 2)
    error ("gradient: the second input argument should either be a vector or a matrix");
  endif

  if (any (p0_size == 1))
    p0 = p0(:);
    dim = 1;
    num_points = numel (p0);
  else
    num_points = p0_size (1);
    dim = p0_size (2);
  endif

  if (length (varargin) == 0)
    delta = 1;
  elseif (length (varargin) == 1 || length (varargin) == dim)
    try
      delta = [varargin{:}];
    catch
      error ("gradient: spacing parameters must be scalars or a vector");
    end_try_catch
  else
    error ("gradient: incorrect number of spacing parameters");
  endif

  if (isscalar (delta))
    delta = repmat (delta, 1, dim);
  elseif (! isvector (delta))
    error ("gradient: spacing values must be scalars or a vector");
  endif

  ## Calculate the gradient
  p0 = mat2cell (p0, num_points, ones (1, dim));
  varargout = cell (1, dim);
  for d = 1:dim
    s = delta(d);
    df_dx = (f (p0{1:d-1}, p0{d}+s, p0{d+1:end})
           - f (p0{1:d-1}, p0{d}-s, p0{d+1:end})) ./ (2*s);
    if (dim == 1)
      varargout{d} = reshape (df_dx, p0_size);
    else
      varargout{d} = df_dx;
    endif
  endfor
endfunction


%!test
%! data = [1, 2, 4, 2];
%! dx = gradient (data);
%! dx2 = gradient (data, 0.25);
%! dx3 = gradient (data, [0.25, 0.5, 1, 3]);
%! assert (dx, [1, 3/2, 0, -2]);
%! assert (dx2, [4, 6, 0, -8]);
%! assert (dx3, [4, 4, 0, -1]);
%! assert (size_equal (data, dx));

%!test
%! [Y,X,Z,U] = ndgrid (2:2:8,1:5,4:4:12,3:5:30);
%! [dX,dY,dZ,dU] = gradient (X);
%! assert (all (dX(:) == 1));
%! assert (all (dY(:) == 0));
%! assert (all (dZ(:) == 0));
%! assert (all (dU(:) == 0));
%! [dX,dY,dZ,dU] = gradient (Y);
%! assert (all (dX(:) == 0));
%! assert (all (dY(:) == 2));
%! assert (all (dZ(:) == 0));
%! assert (all (dU(:) == 0));
%! [dX,dY,dZ,dU] = gradient (Z);
%! assert (all (dX(:) == 0));
%! assert (all (dY(:) == 0));
%! assert (all (dZ(:) == 4));
%! assert (all (dU(:) == 0));
%! [dX,dY,dZ,dU] = gradient (U);
%! assert (all (dX(:) == 0));
%! assert (all (dY(:) == 0));
%! assert (all (dZ(:) == 0));
%! assert (all (dU(:) == 5));
%! assert (size_equal (dX, dY, dZ, dU, X, Y, Z, U));
%! [dX,dY,dZ,dU] = gradient (U, 5.0);
%! assert (all (dU(:) == 1));
%! [dX,dY,dZ,dU] = gradient (U, 1.0, 2.0, 3.0, 2.5);
%! assert (all (dU(:) == 2));

%!test
%! [Y,X,Z,U] = ndgrid (2:2:8,1:5,4:4:12,3:5:30);
%! [dX,dY,dZ,dU] = gradient (X+j*X);
%! assert (all (dX(:) == 1+1j));
%! assert (all (dY(:) == 0));
%! assert (all (dZ(:) == 0));
%! assert (all (dU(:) == 0));
%! [dX,dY,dZ,dU] = gradient (Y-j*Y);
%! assert (all (dX(:) == 0));
%! assert (all (dY(:) == 2-j*2));
%! assert (all (dZ(:) == 0));
%! assert (all (dU(:) == 0));
%! [dX,dY,dZ,dU] = gradient (Z+j*1);
%! assert (all (dX(:) == 0));
%! assert (all (dY(:) == 0));
%! assert (all (dZ(:) == 4));
%! assert (all (dU(:) == 0));
%! [dX,dY,dZ,dU] = gradient (U-j*1);
%! assert (all (dX(:) == 0));
%! assert (all (dY(:) == 0));
%! assert (all (dZ(:) == 0));
%! assert (all (dU(:) == 5));
%! assert (size_equal (dX, dY, dZ, dU, X, Y, Z, U));
%! [dX,dY,dZ,dU] = gradient (U, 5.0);
%! assert (all (dU(:) == 1));
%! [dX,dY,dZ,dU] = gradient (U, 1.0, 2.0, 3.0, 2.5);
%! assert (all (dU(:) == 2));

%!test
%! x = 0:10;
%! f = @cos;
%! df_dx = @(x) -sin (x);
%! assert (gradient (f, x), df_dx (x), 0.2);
%! assert (gradient (f, x, 0.5), df_dx (x), 0.1);

%!test
%! xy = reshape (1:10, 5, 2);
%! f = @(x,y) sin (x) .* cos (y);
%! df_dx = @(x, y) cos (x) .* cos (y);
%! df_dy = @(x, y) -sin (x) .* sin (y);
%! [dx, dy] = gradient (f, xy);
%! assert (dx, df_dx (xy (:, 1), xy (:, 2)), 0.1)
%! assert (dy, df_dy (xy (:, 1), xy (:, 2)), 0.1)

