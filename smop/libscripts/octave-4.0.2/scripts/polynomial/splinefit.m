## Copyright (C) 2012-2015 Ben Abbott, Jonas Lundgren
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
## @deftypefn  {Function File} {@var{pp} =} splinefit (@var{x}, @var{y}, @var{breaks})
## @deftypefnx {Function File} {@var{pp} =} splinefit (@var{x}, @var{y}, @var{p})
## @deftypefnx {Function File} {@var{pp} =} splinefit (@dots{}, "periodic", @var{periodic})
## @deftypefnx {Function File} {@var{pp} =} splinefit (@dots{}, "robust", @var{robust})
## @deftypefnx {Function File} {@var{pp} =} splinefit (@dots{}, "beta", @var{beta})
## @deftypefnx {Function File} {@var{pp} =} splinefit (@dots{}, "order", @var{order})
## @deftypefnx {Function File} {@var{pp} =} splinefit (@dots{}, "constraints", @var{constraints})
##
## Fit a piecewise cubic spline with breaks (knots) @var{breaks} to the
## noisy data, @var{x} and @var{y}.
##
## @var{x} is a vector, and @var{y} is a vector or N-D array.  If @var{y} is an
## N-D array, then @var{x}(j) is matched to @var{y}(:,@dots{},:,j).
##
## @var{p} is a positive integer defining the number of intervals along @var{x},
## and @var{p}+1 is the number of breaks.  The number of points in each interval
## differ by no more than 1.
##
## The optional property @var{periodic} is a logical value which specifies
## whether a periodic boundary condition is applied to the spline.  The
## length of the period is @code{max (@var{breaks}) - min (@var{breaks})}.
## The default value is @code{false}.
##
## The optional property @var{robust} is a logical value which specifies
## if robust fitting is to be applied to reduce the influence of outlying
## data points.  Three iterations of weighted least squares are performed.
## Weights are computed from previous residuals.  The sensitivity of outlier
## identification is controlled by the property @var{beta}.  The value of
## @var{beta} is restricted to the range, 0 < @var{beta} < 1.  The default
## value is @var{beta} = 1/2.  Values close to 0 give all data equal
## weighting.  Increasing values of @var{beta} reduce the influence of
## outlying data.  Values close to unity may cause instability or rank
## deficiency.
##
## The fitted spline is returned as a piecewise polynomial, @var{pp}, and
## may be evaluated using @code{ppval}.
##
## The splines are constructed of polynomials with degree @var{order}.
## The default is a cubic, @var{order}=3.  A spline with P pieces has
## P+@var{order} degrees of freedom.  With periodic boundary conditions
## the degrees of freedom are reduced to P.
##
## The optional property, @var{constaints}, is a structure specifying linear
## constraints on the fit.  The structure has three fields, @qcode{"xc"},
## @qcode{"yc"}, and @qcode{"cc"}.
##
## @table @asis
## @item @qcode{"xc"}
## Vector of the x-locations of the constraints.
##
## @item @qcode{"yc"}
## Constraining values at the locations @var{xc}.
## The default is an array of zeros.
##
## @item @qcode{"cc"}
## Coefficients (matrix).  The default is an array of ones.  The number of
## rows is limited to the order of the piecewise polynomials, @var{order}.
## @end table
##
## Constraints are linear combinations of derivatives of order 0 to
## @var{order}-1 according to
##
## @example
## @group
## @tex
## $cc(1,j) \cdot y(xc(j)) + cc(2,j) \cdot y\prime(xc(j)) + ... = yc(:,\dots,:,j)$.
## @end tex
## @ifnottex
## cc(1,j) * y(xc(j)) + cc(2,j) * y'(xc(j)) + ... = yc(:,...,:,j).
## @end ifnottex
## @end group
## @end example
##
## @seealso{interp1, unmkpp, ppval, spline, pchip, ppder, ppint, ppjumps}
## @end deftypefn

function pp = splinefit (x, y, breaks, varargin)
  if (nargin > 3)
    n = cellfun ("isclass", varargin, "char");
    varargin(n) = lower (varargin(n));
    try
      props = struct (varargin{:});
    catch
      print_usage ();
    end_try_catch
  else
    props = struct ();
  endif
  fields = fieldnames (props);
  for f = 1:numel (fields)
    if (! any (strcmp (fields{f},
                       {"periodic", "robust", "beta", "order", "constraints"})))
      error ("splinefit:invalidproperty",
             "unrecognized property '%s'", fields{f});
    endif
  endfor
  args = {};
  if (isfield (props, "periodic") && props.periodic)
    args{end+1} = "p";
  endif
  if (isfield (props, "robust") && props.robust)
    args{end+1} = "r";
  endif
  if (isfield (props, "beta"))
    if (0 < props.beta && props.beta < 1)
      args{end+1} = props.beta;
    else
      error ("splinefit:invalidbeta", "invalid beta parameter (0 < beta < 1)");
    endif
  endif
  if (isfield (props, "order"))
    if (props.order >= 0)
      args{end+1} = props.order + 1;
    else
      error ("splinefit:invalidorder", "invalid order");
    endif
  endif
  if (isfield (props, "constraints"))
    args{end+1} = props.constraints;
  endif
  if (nargin < 3)
    print_usage ();
  elseif (! isnumeric (breaks) || ! isvector (breaks))
    print_usage ();
  endif
  pp = __splinefit__ (x, y, breaks, args{:});
endfunction


%!demo
%! % Noisy data
%! x = linspace (0, 2*pi, 100);
%! y = sin (x) + 0.1 * randn (size (x));
%! % Breaks
%! breaks = [0:5, 2*pi];
%! % Fit a spline of order 5
%! pp = splinefit (x, y, breaks, "order", 4);
%! clf ()
%! plot (x, y, "s", x, ppval (pp, x), "r", breaks, ppval (pp, breaks), "+r")
%! xlabel ("Independent Variable")
%! ylabel ("Dependent Variable")
%! title ("Fit a piece-wise polynomial of order 4");
%! legend ({"data", "fit", "breaks"})
%! axis tight
%! ylim auto

%!demo
%! % Noisy data
%! x = linspace (0,2*pi, 100);
%! y = sin (x) + 0.1 * randn (size (x));
%! % Breaks
%! breaks = [0:5, 2*pi];
%! % Fit a spline of order 3 with periodic boundary conditions
%! pp = splinefit (x, y, breaks, "order", 2, "periodic", true);
%! clf ()
%! plot (x, y, "s", x, ppval (pp, x), "r", breaks, ppval (pp, breaks), "+r")
%! xlabel ("Independent Variable")
%! ylabel ("Dependent Variable")
%! title ("Fit a periodic piece-wise polynomial of order 2");
%! legend ({"data", "fit", "breaks"})
%! axis tight
%! ylim auto

%!demo
%! % Noisy data
%! x = linspace (0, 2*pi, 100);
%! y = sin (x) + 0.1 * randn (size (x));
%! % Breaks
%! breaks = [0:5, 2*pi];
%! % Constraints: y(0) = 0, y'(0) = 1 and y(3) + y"(3) = 0
%! xc = [0 0 3];
%! yc = [0 1 0];
%! cc = [1 0 1; 0 1 0; 0 0 1];
%! con = struct ("xc", xc, "yc", yc, "cc", cc);
%! % Fit a cubic spline with 8 pieces and constraints
%! pp = splinefit (x, y, 8, "constraints", con);
%! clf ()
%! plot (x, y, "s", x, ppval (pp, x), "r", breaks, ppval (pp, breaks), "+r")
%! xlabel ("Independent Variable")
%! ylabel ("Dependent Variable")
%! title ("Fit a cubic spline with constraints")
%! legend ({"data", "fit", "breaks"})
%! axis tight
%! ylim auto

%!demo
%! % Noisy data
%! x = linspace (0, 2*pi, 100);
%! y = sin (x) + 0.1 * randn (size (x));
%! % Breaks
%! breaks = [0:5, 2*pi];
%! xc = [0 0 3];
%! yc = [0 1 0];
%! cc = [1 0 1; 0 1 0; 0 0 1];
%! con = struct ("xc", xc, "yc", yc, "cc", cc);
%! % Fit a spline of order 6 with constraints and periodicity
%! pp = splinefit (x, y, breaks, "constraints", con, "order", 5, "periodic", true);
%! clf ()
%! plot (x, y, "s", x, ppval (pp, x), "r", breaks, ppval (pp, breaks), "+r")
%! xlabel ("Independent Variable")
%! ylabel ("Dependent Variable")
%! title ("Fit a 5th order piece-wise periodic polynomial with constraints")
%! legend ({"data", "fit", "breaks"})
%! axis tight
%! ylim auto

%!shared xb, yb, x
%! xb = 0:2:10;
%! yb = 2*rand (size (xb)) - 1;
%! x = 0:0.1:10;

%!test
%! y = interp1 (xb, yb, x, "linear");
%! assert (ppval (splinefit (x, y, xb, "order", 1), x), y, 15 * eps ());
%!test
%! y = interp1 (xb, yb, x, "spline");
%! assert (ppval (splinefit (x, y, xb, "order", 3), x), y, 15 * eps ());
%!test
%! y = interp1 (xb, yb, x, "spline");
%! assert (ppval (splinefit (x, y, xb), x), y, 15 * eps ());

