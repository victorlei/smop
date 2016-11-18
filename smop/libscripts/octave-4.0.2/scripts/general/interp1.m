## Copyright (C) 2014-2015 Nir Krakauer
## Copyright (C) 2000-2015 Paul Kienzle
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {@var{yi} =} interp1 (@var{x}, @var{y}, @var{xi})
## @deftypefnx {Function File} {@var{yi} =} interp1 (@var{y}, @var{xi})
## @deftypefnx {Function File} {@var{yi} =} interp1 (@dots{}, @var{method})
## @deftypefnx {Function File} {@var{yi} =} interp1 (@dots{}, @var{extrap})
## @deftypefnx {Function File} {@var{yi} =} interp1 (@dots{}, "left")
## @deftypefnx {Function File} {@var{yi} =} interp1 (@dots{}, "right")
## @deftypefnx {Function File} {@var{pp} =} interp1 (@dots{}, "pp")
##
## One-dimensional interpolation.
##
## Interpolate input data to determine the value of @var{yi} at the points
## @var{xi}.  If not specified, @var{x} is taken to be the indices of @var{y}
## (@code{1:length (@var{y})}).  If @var{y} is a matrix or an N-dimensional
## array, the interpolation is performed on each column of @var{y}.
##
## The interpolation @var{method} is one of:
##
## @table @asis
## @item @qcode{"nearest"}
## Return the nearest neighbor.
##
## @item @qcode{"previous"}
## Return the previous neighbor.
##
## @item @qcode{"next"}
## Return the next neighbor.
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
## Adding '*' to the start of any method above forces @code{interp1}
## to assume that @var{x} is uniformly spaced, and only @code{@var{x}(1)}
## and @code{@var{x}(2)} are referenced.  This is usually faster,
## and is never slower.  The default method is @qcode{"linear"}.
##
## If @var{extrap} is the string @qcode{"extrap"}, then extrapolate values
## beyond the endpoints using the current @var{method}.  If @var{extrap} is a
## number, then replace values beyond the endpoints with that number.  When
## unspecified, @var{extrap} defaults to @code{NA}.
##
## If the string argument @qcode{"pp"} is specified, then @var{xi} should not
## be supplied and @code{interp1} returns a piecewise polynomial object.  This
## object can later be used with @code{ppval} to evaluate the interpolation.
## There is an equivalence, such that @code{ppval (interp1 (@var{x},
## @var{y}, @var{method}, @qcode{"pp"}), @var{xi}) == interp1 (@var{x}, @var{y},
## @var{xi}, @var{method}, @qcode{"extrap"})}.
##
## Duplicate points in @var{x} specify a discontinuous interpolant.  There
## may be at most 2 consecutive points with the same value.
## If @var{x} is increasing, the default discontinuous interpolant is
## right-continuous.  If @var{x} is decreasing, the default discontinuous
## interpolant is left-continuous.
## The continuity condition of the interpolant may be specified by using
## the options @qcode{"left"} or @qcode{"right"} to select a left-continuous
## or right-continuous interpolant, respectively.
## Discontinuous interpolation is only allowed for @qcode{"nearest"} and
## @qcode{"linear"} methods; in all other cases, the @var{x}-values must be
## unique.
##
## An example of the use of @code{interp1} is
##
## @example
## @group
## xf = [0:0.05:10];
## yf = sin (2*pi*xf/5);
## xp = [0:10];
## yp = sin (2*pi*xp/5);
## lin = interp1 (xp, yp, xf);
## near = interp1 (xp, yp, xf, "nearest");
## pch = interp1 (xp, yp, xf, "pchip");
## spl = interp1 (xp, yp, xf, "spline");
## plot (xf,yf,"r", xf,near,"g", xf,lin,"b", xf,pch,"c", xf,spl,"m",
##       xp,yp,"r*");
## legend ("original", "nearest", "linear", "pchip", "spline");
## @end group
## @end example
##
## @seealso{pchip, spline, interpft, interp2, interp3, interpn}
## @end deftypefn

## Author: Paul Kienzle
## Date: 2000-03-25
##    added 'nearest' as suggested by Kai Habel
## 2000-07-17 Paul Kienzle
##    added '*' methods and matrix y
##    check for proper table lengths
## 2002-01-23 Paul Kienzle
##    fixed extrapolation

function yi = interp1 (x, y, varargin)

  if (nargin < 2 || nargin > 6)
    print_usage ();
  endif

  method = "linear";
  extrap = NA;
  xi = [];
  ispp = false;
  firstnumeric = true;
  rightcontinuous = NaN;

  if (nargin > 2)
    for i = 1:length (varargin)
      arg = varargin{i};
      if (ischar (arg))
        arg = tolower (arg);
        switch (arg)
          case "extrap"
            extrap = "extrap";
          case "pp"
            ispp = true;
          case {"right", "-right"}
            rightcontinuous = true;
          case {"left", "-left"}
            rightcontinuous = false;
          otherwise
            method = arg;
        endswitch
      else
        if (firstnumeric)
          xi = arg;
          firstnumeric = false;
        else
          extrap = arg;
        endif
      endif
    endfor
  endif

  if (isempty (xi) && firstnumeric && ! ispp)
    xi = y;
    y = x;
    if (isvector (y))
      x = 1:numel (y);
    else
      x = 1:rows (y);
    endif
  endif

  ## reshape matrices for convenience
  x = x(:);
  nx = rows (x);
  szx = size (xi);
  if (isvector (y))
    y = y(:);
  endif

  szy = size (y);
  y = y(:,:);
  [ny, nc] = size (y);
  xi = xi(:);

  ## determine sizes
  if (nx < 2 || ny < 2)
    error ("interp1: minimum of 2 points required in each dimension");
  endif

  ## check whether x is sorted; sort if not.
  if (! issorted (x, "either"))
    [x, p] = sort (x);
    y = y(p,:);
  endif

  if (any (strcmp (method, {"previous", "*previous", "next", "*next"})))
    rightcontinuous = NaN; # needed for these methods to work
  endif

  if (isnan (rightcontinuous))
    ## If not specified, set the continuity condition
    if (x(end) < x(1))
      rightcontinuous = false;
    else
      rightcontinuous = true;
    endif
  elseif ((rightcontinuous && (x(end) < x(1)))
          || (! rightcontinuous && (x(end) > x(1))))
    ## Switch between left-continuous and right-continuous
    x = flipud (x);
    y = flipud (y);
  endif

  ## Because of the way mkpp works, it's easiest to implement "next"
  ## by running "previous" with vectors flipped.
  if (strcmp (method, "next"))
    x = flipud (x);
    y = flipud (y);
    method = "previous";
  elseif (strcmp (method, "*next"))
    x = flipud (x);
    y = flipud (y);
    method = "*previous";
  endif

  starmethod = method(1) == "*";

  if (starmethod)
    dx = x(2) - x(1);
  else
    jumps = x(1:end-1) == x(2:end);
    have_jumps = any (jumps);
    if (have_jumps)
      if (strcmp (method, "linear") || strcmp (method, ("nearest")))
        if (any (jumps(1:nx-2) & jumps(2:nx-1)))
          warning ("interp1: multiple discontinuities at the same X value");
        endif
      else
        error ("interp1: discontinuities not supported for method '%s'",
                                                                   method);
      endif
    endif
  endif

  ## Proceed with interpolating by all methods.
  switch (method)

    case "nearest"
      pp = mkpp ([x(1); (x(1:nx-1)+x(2:nx))/2; x(nx)],
                 shiftdim (y, 1), szy(2:end));
      pp.orient = "first";

      if (ispp)
        yi = pp;
      else
        yi = ppval (pp, reshape (xi, szx));
      endif

    case "*nearest"
      pp = mkpp ([x(1), x(1)+[0.5:(nx-1)]*dx, x(nx)],
                 shiftdim (y, 1), szy(2:end));
      pp.orient = "first";

      if (ispp)
        yi = pp;
      else
        yi = ppval (pp, reshape (xi, szx));
      endif

    case "previous"
      pp = mkpp ([x(1:nx); 2*x(nx)-x(nx-1)],
                 shiftdim (y, 1), szy(2:end));
      pp.orient = "first";

      if (ispp)
        yi = pp;
      else
        yi = ppval (pp, reshape (xi, szx));
      endif

    case "*previous"
      pp = mkpp (x(1)+[0:nx]*dx,
                 shiftdim (y, 1), szy(2:end));
      pp.orient = "first";

      if (ispp)
        yi = pp;
      else
        yi = ppval (pp, reshape (xi, szx));
      endif

    case "linear"

      xx = x;
      nxx = nx;
      yy = y;
      dy = diff (yy);
      if (have_jumps)
        ## Omit zero-size intervals.
        xx(jumps) = [];
        nxx = rows (xx);
        yy(jumps, :) = [];
        dy(jumps, :) = [];
      endif

      dx = diff (xx);
      dx = repmat (dx, [1 size(dy)(2:end)]);

      coefs = [(dy./dx).', yy(1:nxx-1, :).'];

      pp = mkpp (xx, coefs, szy(2:end));
      pp.orient = "first";

      if (ispp)
        yi = pp;
      else
        yi = ppval (pp, reshape (xi, szx));
      endif

    case "*linear"
      dy = diff (y);
      coefs = [(dy/dx).'(:), y(1:nx-1, :).'(:)];
      pp = mkpp (x, coefs, szy(2:end));
      pp.orient = "first";

      if (ispp)
        yi = pp;
      else
        yi = ppval (pp, reshape (xi, szx));
      endif

    case {"pchip", "*pchip", "cubic", "*cubic"}
      if (nx == 2 || starmethod)
        x = linspace (x(1), x(nx), ny);
      endif

      if (ispp)
        y = shiftdim (reshape (y, szy), 1);
        yi = pchip (x, y);
        yi.orient = "first";
      else
        y = shiftdim (y, 1);
        yi = pchip (x, y, reshape (xi, szx));
        if (! isvector (y))
          yi = shiftdim (yi, 1);
        endif
      endif

    case {"spline", "*spline"}
      if (nx == 2 || starmethod)
        x = linspace (x(1), x(nx), ny);
      endif

      if (ispp)
        y = shiftdim (reshape (y, szy), 1);
        yi = spline (x, y);
        yi.orient = "first";
      else
        y = shiftdim (y, 1);
        yi = spline (x, y, reshape (xi, szx));
        if (! isvector (y))
          yi = shiftdim (yi, 1);
        endif
      endif

    otherwise
      error ("interp1: invalid method '%s'", method);

  endswitch

  if (! ispp && isnumeric (extrap))
    ## determine which values are out of range and set them to extrap,
    ## unless extrap == "extrap".
    minx = min (x(1), x(nx));
    maxx = max (x(1), x(nx));

    xi = reshape (xi, szx);
    outliers = xi < minx | ! (xi <= maxx); # this even catches NaNs
    if (size_equal (outliers, yi))
      yi(outliers) = extrap;
      yi = reshape (yi, szx);
    elseif (! isvector (yi))
      yi(outliers, :) = extrap;
    else
      yi(outliers.') = extrap;
    endif

  endif

endfunction


%!demo
%! clf;
%! xf = 0:0.05:10;  yf = sin (2*pi*xf/5);
%! xp = 0:10;       yp = sin (2*pi*xp/5);
%! lin = interp1 (xp,yp,xf, 'linear');
%! spl = interp1 (xp,yp,xf, 'spline');
%! pch = interp1 (xp,yp,xf, 'pchip');
%! near= interp1 (xp,yp,xf, 'nearest');
%! plot (xf,yf,'r',xf,near,'g',xf,lin,'b',xf,pch,'c',xf,spl,'m',xp,yp,'r*');
%! legend ('original', 'nearest', 'linear', 'pchip', 'spline');
%! title ('Interpolation of continuous function sin (x) w/various methods');
%! %--------------------------------------------------------
%! % confirm that interpolated function matches the original

%!demo
%! clf;
%! xf = 0:0.05:10;  yf = sin (2*pi*xf/5);
%! xp = 0:10;       yp = sin (2*pi*xp/5);
%! lin = interp1 (xp,yp,xf, '*linear');
%! spl = interp1 (xp,yp,xf, '*spline');
%! pch = interp1 (xp,yp,xf, '*pchip');
%! near= interp1 (xp,yp,xf, '*nearest');
%! plot (xf,yf,'r',xf,near,'g',xf,lin,'b',xf,pch,'c',xf,spl,'m',xp,yp,'r*');
%! legend ('*original', '*nearest', '*linear', '*pchip', '*spline');
%! title ('Interpolation of continuous function sin (x) w/various *methods');
%! %--------------------------------------------------------
%! % confirm that interpolated function matches the original

%!demo
%! clf;
%! fstep = @(x) x > 1;
%! xf = 0:0.05:2;  yf = fstep (xf);
%! xp = linspace (0,2,10);  yp = fstep (xp);
%! pch = interp1 (xp,yp,xf, 'pchip');
%! spl = interp1 (xp,yp,xf, 'spline');
%! plot (xf,yf,'r',xf,pch,'b',xf,spl,'m',xp,yp,'r*');
%! title ({'Interpolation of step function with discontinuity at x==1', ...
%!         'Note: "pchip" is shape-preserving, "spline" (continuous 1st, 2nd derivatives) is not'});
%! legend ('original', 'pchip', 'spline');

%!demo
%! clf;
%! t = 0 : 0.3 : pi; dt = t(2)-t(1);
%! n = length (t); k = 100; dti = dt*n/k;
%! ti = t(1) + [0 : k-1]*dti;
%! y = sin (4*t + 0.3) .* cos (3*t - 0.1);
%! ddys = diff (diff (interp1 (t,y,ti, 'spline'))./dti)./dti;
%! ddyp = diff (diff (interp1 (t,y,ti, 'pchip')) ./dti)./dti;
%! ddyc = diff (diff (interp1 (t,y,ti, 'cubic')) ./dti)./dti;
%! plot (ti(2:end-1),ddys,'b*', ti(2:end-1),ddyp,'c^', ti(2:end-1),ddyc,'g+');
%! title ({'Second derivative of interpolated "sin (4*t + 0.3) .* cos (3*t - 0.1)"', ...
%!         'Note: "spline" has continous 2nd derivative, others do not'});
%! legend ('spline', 'pchip', 'cubic');

%!demo
%! clf;
%! xf = 0:0.05:10;                yf = sin (2*pi*xf/5) - (xf >= 5);
%! xp = [0:.5:4.5,4.99,5:.5:10];  yp = sin (2*pi*xp/5) - (xp >= 5);
%! lin = interp1 (xp,yp,xf, 'linear');
%! near= interp1 (xp,yp,xf, 'nearest');
%! plot (xf,yf,'r', xf,near,'g', xf,lin,'b', xp,yp,'r*');
%! legend ('original', 'nearest', 'linear');
%! %--------------------------------------------------------
%! % confirm that interpolated function matches the original

%!demo
%! clf;
%! x = 0:0.5:3;
%! x1 = [3 2 2 1];
%! x2 = [1 2 2 3];
%! y1 = [1 1 0 0];
%! y2 = [0 0 1 1];
%! h = plot (x, interp1 (x1, y1, x), 'b', x1, y1, 'sb');
%! hold on
%! g = plot (x, interp1 (x2, y2, x), 'r', x2, y2, '*r');
%! axis ([0.5 3.5 -0.5 1.5])
%! legend ([h(1), g(1)], {'left-continuous', 'right-continuous'}, ...
%!         'location', 'northwest')
%! legend boxoff
%! %--------------------------------------------------------
%! % red curve is left-continuous and blue is right-continuous at x = 2

##FIXME: add test for N-d arguments here

## For each type of interpolated test, confirm that the interpolated
## value at the knots match the values at the knots.  Points away
## from the knots are requested, but only "nearest" and "linear"
## confirm they are the correct values.

%!shared xp, yp, xi, style
%! xp = 0:2:10;
%! yp = sin (2*pi*xp/5);
%! xi = [-1, 0, 2.2, 4, 6.6, 10, 11];

## The following BLOCK/ENDBLOCK section is repeated for each style
##    nearest, previous, next, linear, cubic, spline, pchip
## The test for ppval of cubic has looser tolerance, but otherwise
## the tests are identical.
## Note that the block checks style and *style; if you add more tests
## be sure to add them to both sections of each block.  One test,
## style vs. *style, occurs only in the first section.
## There is an ENDBLOCKTEST after the final block

%!test style = "nearest";
## BLOCK
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
%!assert (interp1 (xp,[yp',yp'],xi,style),
%!        interp1 (xp,[yp',yp'],xi,["*",style]),100*eps)
%!test style = ["*",style];
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
## ENDBLOCK

%!test style = "previous";
## BLOCK
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
## This test is expected to fail, so commented out.
## "previous" and "next" options are not symmetric w.r.t to flipping xp,yp
#%!assert (interp1 (xp,yp,xi,style),...
#%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
%!assert (interp1 (xp,[yp',yp'],xi,style),
%!        interp1 (xp,[yp',yp'],xi,["*",style]),100*eps)
%!test style = ["*",style];
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
#%!assert (interp1 (xp,yp,xi,style),...
#%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
## ENDBLOCK

%!test style = "next";
## BLOCK
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
#%!assert (interp1 (xp,yp,xi,style),...
#%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
%!assert (interp1 (xp,[yp',yp'],xi,style),
%!        interp1 (xp,[yp',yp'],xi,["*",style]),100*eps)
%!test style = ["*",style];
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
#%!assert (interp1 (xp,yp,xi,style),...
#%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
## ENDBLOCK

%!test style = "linear";
## BLOCK
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
%!assert (interp1 (xp,[yp',yp'],xi,style),
%!        interp1 (xp,[yp',yp'],xi,["*",style]),100*eps)
%!test style = ['*',style];
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!assert (interp1 ([1 2 2 3], [1 2 3 4], 2), 3);
%!assert (interp1 ([3 2 2 1], [4 3 2 1], 2), 2);
%!error interp1 (1,1,1, style)
## ENDBLOCK

%!test style = "cubic";
## BLOCK
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),100*eps)
%!error interp1 (1,1,1, style)
%!assert (interp1 (xp,[yp',yp'],xi,style),
%!        interp1 (xp,[yp',yp'],xi,["*",style]),100*eps)
%!test style = ["*",style];
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),100*eps)
%!error interp1 (1,1,1, style)
## ENDBLOCK

%!test style = "pchip";
## BLOCK
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
%!assert (interp1 (xp,[yp',yp'],xi,style),
%!        interp1 (xp,[yp',yp'],xi,["*",style]),100*eps)
%!test style = ["*",style];
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
## ENDBLOCK

%!test style = "spline";
## BLOCK
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
%!assert (interp1 (xp,[yp',yp'],xi,style),
%!        interp1 (xp,[yp',yp'],xi,["*",style]),100*eps)
%!test style = ["*",style];
%!assert (interp1 (xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA])
%!assert (interp1 (xp,yp,xp,style), yp, 100*eps)
%!assert (interp1 (xp,yp,xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp',style), yp', 100*eps)
%!assert (interp1 (xp',yp',xp,style), yp, 100*eps)
%!assert (isempty (interp1 (xp',yp',[],style)))
%!assert (isempty (interp1 (xp,yp,[],style)))
%!assert (interp1 (xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)])
%!assert (interp1 (xp,yp,xi,style),...
%!        interp1 (fliplr (xp),fliplr (yp),xi,style),100*eps)
%!assert (ppval (interp1 (xp,yp,style,"pp"),xi),
%!        interp1 (xp,yp,xi,style,"extrap"),10*eps)
%!error interp1 (1,1,1, style)
## ENDBLOCK
## ENDBLOCKTEST

## test extrapolation
%!assert (interp1 ([1:5],[3:2:11],[0,6],"linear","extrap"), [1, 13], eps)
%!assert (interp1 ([1:5],[3:2:11],[0,6],"nearest","extrap"), [3, 11], eps)
%!assert (interp1 ([1:5],[3:2:11],[0,6],"previous","extrap"), [3, 11], eps)
%!assert (interp1 ([1:5],[3:2:11],[0,6],"next","extrap"), [3, 11], eps)
%!assert (interp1 (xp, yp, [-1, max(xp)+1],"linear",5), [5, 5])
%!assert (interp1 ([0,1],[1,0],[0.1,0.9;0.2,1.1]), [0.9 0.1; 0.8 NA], eps)
%!assert (interp1 ([0,1],[1,0],[0.1,0.9;0.2,1]), [0.9 0.1; 0.8 0], eps)

## Basic sanity checks
%!assert (interp1 (1:2,1:2,1.4,"nearest"), 1)
%!assert (interp1 (1:2,1:2,1.6,"previous"), 1)
%!assert (interp1 (1:2,1:2,1.4,"next"), 2)
%!assert (interp1 (1:2,1:2,1.4,"linear"), 1.4)
%!assert (interp1 (1:4,1:4,1.4,"cubic"), 1.4)
%!assert (interp1 (1:2,1:2,1.1,"spline"), 1.1)
%!assert (interp1 (1:3,1:3,1.4,"spline"), 1.4)

%!assert (interp1 (1:2:4,1:2:4,1.4,"*nearest"), 1)
%!assert (interp1 (1:2:4,1:2:4,2.2,"*previous"), 1)
%!assert (interp1 (1:2:4,1:2:4,1.4,"*next"), 3)
%!assert (interp1 (1:2:4,1:2:4,[0,1,1.4,3,4],"*linear"), [NA,1,1.4,3,NA])
%!assert (interp1 (1:2:8,1:2:8,1.4,"*cubic"), 1.4)
%!assert (interp1 (1:2,1:2,1.3, "*spline"), 1.3)
%!assert (interp1 (1:2:6,1:2:6,1.4,"*spline"), 1.4)

%!assert (interp1 ([3,2,1],[3,2,2],2.5), 2.5)

%!assert (interp1 ([4,4,3,2,0],[0,1,4,2,1],[1.5,4,4.5], "linear"), [1.75,1,NA])
%!assert (interp1 (0:4, 2.5), 1.5)

## Left and Right discontinuities
%!assert (interp1 ([1,2,2,3,4],[0,1,4,2,1],[-1,1.5,2,2.5,3.5], "linear", "extrap", "right"), [-2,0.5,4,3,1.5])
%!assert (interp1 ([1,2,2,3,4],[0,1,4,2,1],[-1,1.5,2,2.5,3.5], "linear", "extrap", "left"), [-2,0.5,1,3,1.5])

## Test input validation
%!error interp1 ()
%!error interp1 (1,2,3,4,5,6,7)
%!error <minimum of 2 points required> interp1 (1,1,1, "linear")
%!error <minimum of 2 points required> interp1 (1,1,1, "*nearest")
%!error <minimum of 2 points required> interp1 (1,1,1, "*linear")
%!error <minimum of 2 points required> interp1 (1,1,1, "previous")
%!error <minimum of 2 points required> interp1 (1,1,1, "*previous")
%!warning <multiple discontinuities> interp1 ([1 1 1 2], [1 2 3 4], 1);
%!error <discontinuities not supported> interp1 ([1 1],[1 2],1, "next")
%!error <discontinuities not supported> interp1 ([1 1],[1 2],1, "pchip")
%!error <discontinuities not supported> interp1 ([1 1],[1 2],1, "cubic")
%!error <discontinuities not supported> interp1 ([1 1],[1 2],1, "spline")
%!error <invalid method 'bogus'> interp1 (1:2,1:2,1, "bogus")

