## Copyright (C) 2000-2016 Kai Habel
## Copyright (C) 2009 Jaroslav Hajek
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
## @deftypefn  {Function File} {@var{zi} =} interp2 (@var{x}, @var{y}, @var{z}, @var{xi}, @var{yi})
## @deftypefnx {Function File} {@var{zi} =} interp2 (@var{z}, @var{xi}, @var{yi})
## @deftypefnx {Function File} {@var{zi} =} interp2 (@var{z}, @var{n})
## @deftypefnx {Function File} {@var{zi} =} interp2 (@var{z})
## @deftypefnx {Function File} {@var{zi} =} interp2 (@dots{}, @var{method})
## @deftypefnx {Function File} {@var{zi} =} interp2 (@dots{}, @var{method}, @var{extrap})
##
## Two-dimensional interpolation.
##
## Interpolate reference data @var{x}, @var{y}, @var{z} to determine @var{zi}
## at the coordinates @var{xi}, @var{yi}.  The reference data @var{x}, @var{y}
## can be matrices, as returned by @code{meshgrid}, in which case the sizes of
## @var{x}, @var{y}, and @var{z} must be equal.  If @var{x}, @var{y} are
## vectors describing a grid then @code{length (@var{x}) == columns (@var{z})}
## and @code{length (@var{y}) == rows (@var{z})}.  In either case the input
## data must be strictly monotonic.
##
## If called without @var{x}, @var{y}, and just a single reference data matrix
## @var{z}, the 2-D region
## @code{@var{x} = 1:columns (@var{z}), @var{y} = 1:rows (@var{z})} is assumed.
## This saves memory if the grid is regular and the distance between points is
## not important.
##
## If called with a single reference data matrix @var{z} and a refinement
## value @var{n}, then perform interpolation over a grid where each original
## interval has been recursively subdivided @var{n} times.  This results in
## @code{2^@var{n}-1} additional points for every interval in the original
## grid.  If @var{n} is omitted a value of 1 is used.  As an example, the
## interval [0,1] with @code{@var{n}==2} results in a refined interval with
## points at [0, 1/4, 1/2, 3/4, 1].
##
## The interpolation @var{method} is one of:
##
## @table @asis
## @item @qcode{"nearest"}
## Return the nearest neighbor.
##
## @item @qcode{"linear"} (default)
## Linear interpolation from nearest neighbors.
##
## @item @qcode{"pchip"}
## Piecewise cubic Hermite interpolating polynomial---shape-preserving
## interpolation with smooth first derivative.
##
## @item @qcode{"cubic"}
## Cubic interpolation (same as @qcode{"pchip"}).
##
## @item @qcode{"spline"}
## Cubic spline interpolation---smooth first and second derivatives
## throughout the curve.
## @end table
##
## @var{extrap} is a scalar number.  It replaces values beyond the endpoints
## with @var{extrap}.  Note that if @var{extrapval} is used, @var{method} must
## be specified as well.  If @var{extrap} is omitted and the @var{method} is
## @qcode{"spline"}, then the extrapolated values of the @qcode{"spline"} are
## used.  Otherwise the default @var{extrap} value for any other @var{method}
## is @qcode{"NA"}.
## @seealso{interp1, interp3, interpn, meshgrid}
## @end deftypefn

function ZI = interp2 (varargin)

  narginchk (1, 7);
  nargs = nargin;

  Z = X = Y = XI = YI = n = [];
  method = "linear";
  extrap = [];

  ## Check for method and extrap
  if (nargs > 1 && ischar (varargin{end-1}))
    if (! isnumeric (varargin{end}) || ! isscalar (varargin{end}))
      error ("interp2: EXTRAP must be a numeric scalar");
    endif
    extrap = varargin{end};
    method = varargin{end-1};
    nargs -= 2;
  elseif (ischar (varargin{end}))
    method = varargin{end};
    nargs--;
  endif
  if (method(1) == "*")
    warning ("interp2: ignoring unsupported '*' flag to METHOD");
    method(1) = [];
  endif
  method = validatestring (method, ...
                           {"nearest", "linear", "pchip", "cubic", "spline"});

  ## Read numeric input
  switch (nargs)
    case 1
      Z = varargin{1};
      n = 1;
    case 2
      [Z, n] = deal (varargin{1:nargs});
    case 3
      [Z, XI, YI] = deal (varargin{1:nargs});
    case 5
      [X, Y, Z, XI, YI] = deal (varargin{1:nargs});
    otherwise
      print_usage ();
  endswitch

  ## Type checking
  if (! isnumeric (Z) || isscalar (Z) || ! ismatrix (Z))
    error ("interp2: Z must be a 2-D matrix");
  endif
  if (! isempty (n) && ! (isscalar (n) && n >= 0 && n == fix (n)))
    error ("interp2: N must be an integer >= 0");
  endif

  ## Define X, Y, XI, YI if needed
  [zr, zc] = size (Z);
  if (isempty (X))
    X = 1:zc;
    Y = 1:zr;
  endif
  if (! isnumeric (X) || ! isnumeric (Y))
    error ("interp2: X, Y must be numeric matrices");
  endif
  if (! isempty (n))
    ## Calculate the interleaved input vectors.
    p = 2^n;
    XI = (p:p*zc)/p;
    YI = (p:p*zr)'/p;
  endif
  if (! isnumeric (XI) || ! isnumeric (YI))
    error ("interp2: XI, YI must be numeric");
  endif

  if (isvector (X) && isvector (Y))
    X = X(:);  Y = Y(:);
  elseif (size_equal (X, Y))
    X = X(1,:).';  Y = Y(:,1);
  else
    error ("interp2: X and Y must be matrices of equal size");
  endif
  if (columns (Z) != length (X) || rows (Z) != length (Y))
    error ("interp2: X and Y size must match the dimensions of Z");
  endif
  dx = diff (X);
  if (all (dx < 0))
    X = flipud (X);
    Z = fliplr (Z);
  elseif (any (dx <= 0))
    error ("interp2: X must be strictly monotonic");
  endif
  dy = diff (Y);
  if (all (dy < 0))
    Y = flipud (Y);
    Z = flipud (Z);
  elseif (any (dy <= 0))
    error ("interp2: Y must be strictly monotonic");
  endif

  if (any (strcmp (method, {"nearest", "linear", "pchip", "cubic"})))

    ## If Xi and Yi are vectors of different orientation build a grid
    if ((rows (XI) == 1 && columns (YI) == 1)
        || (columns (XI) == 1 && rows (YI) == 1))
      [XI, YI] = meshgrid (XI, YI);
    elseif (! size_equal (XI, YI))
      error ("interp2: XI and YI must be matrices of equal size");
    endif

    ## if XI, YI are vectors, X and Y should share their orientation.
    if (rows (XI) == 1)
      if (rows (X) != 1)
        X = X.';
      endif
      if (rows (Y) != 1)
        Y = Y.';
      endif
    elseif (columns (XI) == 1)
      if (columns (X) != 1)
        X = X.';
      endif
      if (columns (Y) != 1)
        Y = Y.';
      endif
    endif

    xidx = lookup (X, XI, "lr");
    yidx = lookup (Y, YI, "lr");

    if (strcmp (method, "linear"))
      ## each quad satisfies the equation z(x,y)=a+b*x+c*y+d*xy
      ##
      ## a-b
      ## | |
      ## c-d
      a = Z(1:(zr - 1), 1:(zc - 1));
      b = Z(1:(zr - 1), 2:zc) - a;
      c = Z(2:zr, 1:(zc - 1)) - a;
      d = Z(2:zr, 2:zc) - a - b - c;

      ## scale XI, YI values to a 1-spaced grid
      Xsc = (XI - X(xidx)) ./ (diff (X)(xidx));
      Ysc = (YI - Y(yidx)) ./ (diff (Y)(yidx));

      ## Get 2D index.
      idx = sub2ind (size (a), yidx, xidx);
      ## We can dispose of the 1D indices at this point to save memory.
      clear xidx yidx;

      ## apply plane equation
      ZI = a(idx) + b(idx).*Xsc + c(idx).*Ysc + d(idx).*Xsc.*Ysc;

    elseif (strcmp (method, "nearest"))
      ii = (XI - X(xidx) >= X(xidx + 1) - XI);
      jj = (YI - Y(yidx) >= Y(yidx + 1) - YI);
      idx = sub2ind (size (Z), yidx+jj, xidx+ii);
      ZI = Z(idx);

    elseif (strcmp (method, "pchip") || strcmp (method, "cubic"))

      if (length (X) < 2 || length (Y) < 2)
        error ("interp2: %s requires at least 2 points in each dimension",
               method);
      endif

      ## first order derivatives
      DX = __pchip_deriv__ (X, Z, 2);
      DY = __pchip_deriv__ (Y, Z, 1);
      ## Compute mixed derivatives row-wise and column-wise, use the average.
      DXY = (__pchip_deriv__ (X, DY, 2) + __pchip_deriv__ (Y, DX, 1))/2;

      ## do the bicubic interpolation
      hx = diff (X); hx = hx(xidx);
      hy = diff (Y); hy = hy(yidx);

      tx = (XI - X(xidx)) ./ hx;
      ty = (YI - Y(yidx)) ./ hy;

      ## construct the cubic hermite base functions in x, y

      ## formulas:
      ## b{1,1} =    ( 2*t.^3 - 3*t.^2     + 1);
      ## b{2,1} = h.*(   t.^3 - 2*t.^2 + t    );
      ## b{1,2} =    (-2*t.^3 + 3*t.^2        );
      ## b{2,2} = h.*(   t.^3 -   t.^2        );

      ## optimized equivalents of the above:
      t1 = tx.^2;
      t2 = tx.*t1 - t1;
      xb{2,2} = hx.*t2;
      t1 = t2 - t1;
      xb{2,1} = hx.*(t1 + tx);
      t2 += t1;
      xb{1,2} = -t2;
      xb{1,1} = t2 + 1;

      t1 = ty.^2;
      t2 = ty.*t1 - t1;
      yb{2,2} = hy.*t2;
      t1 = t2 - t1;
      yb{2,1} = hy.*(t1 + ty);
      t2 += t1;
      yb{1,2} = -t2;
      yb{1,1} = t2 + 1;

      ZI = zeros (size (XI));
      for i = 1:2
        for j = 1:2
          zidx = sub2ind (size (Z), yidx+(j-1), xidx+(i-1));
          ZI += xb{1,i} .* yb{1,j} .*   Z(zidx);
          ZI += xb{2,i} .* yb{1,j} .*  DX(zidx);
          ZI += xb{1,i} .* yb{2,j} .*  DY(zidx);
          ZI += xb{2,i} .* yb{2,j} .* DXY(zidx);
        endfor
      endfor

    endif

  else

    ## Check dimensions of XI and YI
    if (isvector (XI) && isvector (YI) && ! size_equal (XI, YI))
      XI = XI(:).';  YI = YI(:);
    elseif (! size_equal (XI, YI))
      error ("interp2: XI and YI must be matrices of equal size");
    endif

    if (strcmp (method, "spline"))
      if (isgriddata (XI) && isgriddata (YI'))
        ZI = __splinen__ ({Y, X}, Z, {YI(:,1), XI(1,:)}, extrap, "spline");
      else
        error ("interp2: XI, YI must have uniform spacing ('meshgrid' format)");
      endif
    endif

    return; # spline doesn't need NA extrapolation value (MATLAB compatibility)

  endif

  ## extrapolation 'extrap'
  if (isempty (extrap))
    extrap = NA;
  endif

  if (X(1) < X(end))
    if (Y(1) < Y(end))
      ZI(XI < X(1,1) | XI > X(end) | YI < Y(1,1) | YI > Y(end)) = extrap;
    else
      ZI(XI < X(1) | XI > X(end) | YI < Y(end) | YI > Y(1)) = extrap;
    endif
  else
    if (Y(1) < Y(end))
      ZI(XI < X(end) | XI > X(1) | YI < Y(1) | YI > Y(end)) = extrap;
    else
      ZI(XI < X(1,end) | XI > X(1) | YI < Y(end) | YI > Y(1)) = extrap;
    endif
  endif

endfunction

function b = isgriddata (X)
  d1 = diff (X, 1, 1);
  b = all (d1(:) == 0);
endfunction

## Compute the bicubic interpolation coefficients
function o = bc (x)
  x = abs (x);
  o = zeros (size (x));
  idx1 = (x < 1);
  idx2 = !idx1 & (x < 2);
  o(idx1) = 1 - 2.*x(idx1).^2 + x(idx1).^3;
  o(idx2) = 4 - 8.*x(idx2) + 5.*x(idx2).^2 - x(idx2).^3;
endfunction

## This version of sub2ind behaves as if the data was symmetrically padded
function ind = sym_sub2ind (sz, Y, X)
  Y(Y < 1) = 1 - Y(Y < 1);
  while (any (Y(:) > 2*sz(1)))
    Y(Y > 2*sz(1)) = round (Y(Y > 2*sz(1)) / 2);
  endwhile
  Y(Y > sz(1)) = 1 + 2*sz(1) - Y(Y > sz(1));
  X(X < 1) = 1 - X(X < 1);
  while (any (X(:) > 2*sz(2)))
    X(X > 2 * sz(2)) = round (X(X > 2*sz(2)) / 2);
  endwhile
  X(X > sz(2)) = 1 + 2*sz(2) - X(X > sz(2));
  ind = sub2ind (sz, Y, X);
endfunction


%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,4];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "linear"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:)';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "linear"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,4];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "nearest"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:)';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "nearest"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

## 'pchip' commented out since it is the same as 'cubic'
%!#demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,2];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "pchip"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

## 'pchip' commented out since it is the same as 'cubic'
%!#demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:)';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "pchip"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,2];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "cubic"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:)';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "cubic"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,2];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "spline"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:)';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41)';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "spline"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!test  # simple test
%! x = [1,2,3];
%! y = [4,5,6,7];
%! [X, Y] = meshgrid (x, y);
%! orig = X.^2 + Y.^3;
%! xi = [1.2,2, 1.5];
%! yi = [6.2, 4.0, 5.0]';
%!
%! expected = ...
%!   [243,   245.4,  243.9;
%!     65.6,  68,     66.5;
%!    126.6, 129,    127.5];
%! result = interp2 (x,y,orig, xi, yi);
%!
%! assert (result, expected, 1000*eps);

%!test  # 2^n refinement form
%! x = [1,2,3];
%! y = [4,5,6,7];
%! [X, Y] = meshgrid (x, y);
%! orig = X.^2 + Y.^3;
%! xi = [1:0.25:3];  yi = [4:0.25:7]';
%! expected = interp2 (x,y,orig, xi, yi);
%! result = interp2 (orig, 2);
%!
%! assert (result, expected, 10*eps);

%!test  # matrix slice
%! A = eye (4);
%! assert (interp2 (A,[1:4],[1:4]), [1,1,1,1]);

%!test  # non-gridded XI,YI
%! A = eye (4);
%! assert (interp2 (A,[1,2;3,4],[1,3;2,4]), [1,0;0,1]);

%!test  # for values outside of boundaries
%! x = [1,2,3];
%! y = [4,5,6,7];
%! [X, Y] = meshgrid (x,y);
%! orig = X.^2 + Y.^3;
%! xi = [0,4];
%! yi = [3,8]';
%! assert (interp2 (x,y,orig, xi, yi), [NA,NA;NA,NA]);
%! assert (interp2 (x,y,orig, xi, yi,"linear", 0), [0,0;0,0]);
%! assert (interp2 (x,y,orig, xi, yi,"linear", 2), [2,2;2,2]);
%! assert (interp2 (x,y,orig, xi, yi,"spline", 2), [2,2;2,2]);
%! assert (interp2 (x,y,orig, xi, yi,"linear", 0+1i), [0+1i,0+1i;0+1i,0+1i]);
%! assert (interp2 (x,y,orig, xi, yi,"spline"), [27,43;512,528]);


%!test  # for values at boundaries
%! A = [1,2;3,4];
%! x = [0,1];
%! y = [2,3]';
%! assert (interp2 (x,y,A,x,y,"linear"), A);
%! assert (interp2 (x,y,A,x,y,"nearest"), A);

%!test  # for Matlab-compatible rounding for 'nearest'
%! X = meshgrid (1:4);
%! assert (interp2 (X, 2.5, 2.5, "nearest"), 3);

## re-order monotonically decreasing (bug #41838).
%!assert (interp2 ([1 2 3], [3 2 1], magic (3), 2.5, 3), 3.5);
%!assert (interp2 ([3 2 1], [1 2 3], magic (3), 1.5, 1), 3.5);

%!shared z, zout, tol
%! z = [1 3 5; 3 5 7; 5 7 9];
%! zout = [1 2 3 4 5; 2 3 4 5 6; 3 4 5 6 7; 4 5 6 7 8; 5 6 7 8 9];
%! tol = 2 * eps;
%!
%!assert (interp2 (z), zout, tol)
%!assert (interp2 (z, "linear"), zout, tol)
%!assert (interp2 (z, "pchip"), zout, tol)
%!assert (interp2 (z, "cubic"), zout, 10 * tol)
%!assert (interp2 (z, "spline"), zout, tol)
%!assert (interp2 (z, [2 3 1], [2 2 2]', "linear"), repmat ([5, 7, 3], [3, 1]), tol)
%!assert (interp2 (z, [2 3 1], [2 2 2]', "pchip"), repmat ([5, 7, 3], [3, 1]), tol)
%!assert (interp2 (z, [2 3 1], [2 2 2]', "cubic"), repmat ([5, 7, 3], [3, 1]), 10 * tol)
%!assert (interp2 (z, [2 3 1], [2 2 2]', "spline"), repmat ([5, 7, 3], [3, 1]), tol)
%!assert (interp2 (z, [2 3 1], [2 2 2], "linear"), [5 7 3], tol)
%!assert (interp2 (z, [2 3 1], [2 2 2], "pchip"), [5 7 3], tol)
%!assert (interp2 (z, [2 3 1], [2 2 2], "cubic"), [5 7 3], 10 * tol)
%!assert (interp2 (z, [2 3 1], [2 2 2], "spline"), [5 7 3], tol)

## Test input validation
%!error interp2 (1, 1, 1, 1, 1, 2)    # only 5 numeric inputs
%!error interp2 (1, 1, 1, 1, 1, 2, 2) # only 5 numeric inputs
%!error <Z must be a 2-D matrix> interp2 ({1})
%!error <Z must be a 2-D matrix> interp2 (1,1,1)
%!error <Z must be a 2-D matrix> interp2 (ones (2,2,2))
%!error <N must be an integer .= 0> interp2 (ones (2), ones (2))
%!error <N must be an integer .= 0> interp2 (ones (2), -1)
%!error <N must be an integer .= 0> interp2 (ones (2), 1.5)
%!warning <ignoring unsupported '\*' flag> interp2 (rand (3,3), 1, "*linear");
%!error <EXTRAP must be a numeric scalar> interp2 (1, 1, 1, 1, 1, 'linear', {1})
%!error <EXTRAP must be a numeric scalar> interp2 (1, 1, 1, 1, 1, 'linear', ones (2,2))
%!error <EXTRAP must be a numeric scalar> interp2 (1, 1, 1, 1, 1, 'linear', "abc")
%!error <EXTRAP must be a numeric scalar> interp2 (1, 1, 1, 1, 1, 'linear', "extrap")
%!error <X, Y must be numeric matrices> interp2 ({1}, 1, ones (2), 1, 1)
%!error <X, Y must be numeric matrices> interp2 (1, {1}, ones (2), 1, 1)
%!error <XI, YI must be numeric> interp2 (1, 1, ones (2), {1}, 1)
%!error <XI, YI must be numeric> interp2 (1, 1, ones (2), 1, {1})
%!error <X and Y must be matrices of equal size> interp2 (ones(2,2), 1, ones (2), 1, 1)
%!error <X and Y must be matrices of equal size> interp2 (ones(2,2), ones(2,3), ones (2), 1, 1)
%!error <X and Y size must match the dimensions of Z> interp2 (1:3, 1:3, ones (3,2), 1, 1)
%!error <X and Y size must match the dimensions of Z> interp2 (1:2, 1:2, ones (3,2), 1, 1)
%!error <X must be strictly monotonic> interp2 ([1 0 2], 1:3, ones (3,3), 1, 1)
%!error <Y must be strictly monotonic> interp2 (1:3, [1 0 2], ones (3,3), 1, 1)
%!error <XI and YI must be matrices of equal size> interp2 (1:2, 1:2, ones (2), ones(2,2), 1)
%!error <XI and YI must be matrices of equal size> interp2 (1:2, 1:2, ones (2), 1, ones(2,2))
%!error <XI, YI must have uniform spacing> interp2 (1:2, 1:2, ones (2), [1 2 4], [1 2 3], "spline")
%!error <XI, YI must have uniform spacing> interp2 (1:2, 1:2, ones (2), [1 2 3], [1 2 4], "spline")
%!error interp2 (1, 1, 1, 1, 1, "foobar")

