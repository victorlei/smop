## Copyright (C) 2008-2015 VZLU Prague, a.s.
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
##
## Author: Jaroslav Hajek <highegg@gmail.com>

## -*- texinfo -*-
## @deftypefn  {Function File} {} fsolve (@var{fcn}, @var{x0}, @var{options})
## @deftypefnx {Function File} {[@var{x}, @var{fvec}, @var{info}, @var{output}, @var{fjac}] =} fsolve (@var{fcn}, @dots{})
## Solve a system of nonlinear equations defined by the function @var{fcn}.
##
## @var{fcn} should accept a vector (array) defining the unknown variables,
## and return a vector of left-hand sides of the equations.  Right-hand sides
## are defined to be zeros.  In other words, this function attempts to
## determine a vector @var{x} such that @code{@var{fcn} (@var{x})} gives
## (approximately) all zeros.
##
## @var{x0} determines a starting guess.  The shape of @var{x0} is preserved
## in all calls to @var{fcn}, but otherwise it is treated as a column vector.
##
## @var{options} is a structure specifying additional options.  Currently,
## @code{fsolve} recognizes these options:
## @qcode{"FunValCheck"}, @qcode{"OutputFcn"}, @qcode{"TolX"},
## @qcode{"TolFun"}, @qcode{"MaxIter"}, @qcode{"MaxFunEvals"},
## @qcode{"Jacobian"}, @qcode{"Updating"}, @qcode{"ComplexEqn"}
## @qcode{"TypicalX"}, @qcode{"AutoScaling"} and @qcode{"FinDiffType"}.
##
## If @qcode{"Jacobian"} is @qcode{"on"}, it specifies that @var{fcn}, called
## with 2 output arguments also returns the Jacobian matrix of right-hand sides
## at the requested point.  @qcode{"TolX"} specifies the termination tolerance
## in the unknown variables, while @qcode{"TolFun"} is a tolerance for
## equations.  Default is @code{1e-7} for both @qcode{"TolX"} and
## @qcode{"TolFun"}.
##
## If @qcode{"AutoScaling"} is on, the variables will be automatically scaled
## according to the column norms of the (estimated) Jacobian.  As a result,
## TolF becomes scaling-independent.  By default, this option is off because
## it may sometimes deliver unexpected (though mathematically correct) results.
##
## If @qcode{"Updating"} is @qcode{"on"}, the function will attempt to use
## @nospell{Broyden} updates to update the Jacobian, in order to reduce the
## amount of Jacobian calculations.  If your user function always calculates the
## Jacobian (regardless of number of output arguments) then this option provides
## no advantage and should be set to false.
##
## @qcode{"ComplexEqn"} is @qcode{"on"}, @code{fsolve} will attempt to solve
## complex equations in complex variables, assuming that the equations possess
## a complex derivative (i.e., are holomorphic).  If this is not what you want,
## you should unpack the real and imaginary parts of the system to get a real
## system.
##
## For description of the other options, see @code{optimset}.
##
## On return, @var{fval} contains the value of the function @var{fcn}
## evaluated at @var{x}.
##
## @var{info} may be one of the following values:
##
## @table @asis
## @item 1
## Converged to a solution point.  Relative residual error is less than
## specified by TolFun.
##
## @item 2
## Last relative step size was less that TolX.
##
## @item 3
## Last relative decrease in residual was less than TolF.
##
## @item 0
## Iteration limit exceeded.
##
## @item -3
## The trust region radius became excessively small.
## @end table
##
## Note: If you only have a single nonlinear equation of one variable, using
## @code{fzero} is usually a much better idea.
##
## Note about user-supplied Jacobians:
## As an inherent property of the algorithm, a Jacobian is always requested for
## a solution vector whose residual vector is already known, and it is the last
## accepted successful step.  Often this will be one of the last two calls, but
## not always.  If the savings by reusing intermediate results from residual
## calculation in Jacobian calculation are significant, the best strategy is to
## employ OutputFcn: After a vector is evaluated for residuals, if OutputFcn is
## called with that vector, then the intermediate results should be saved for
## future Jacobian evaluation, and should be kept until a Jacobian evaluation
## is requested or until OutputFcn is called with a different vector, in which
## case they should be dropped in favor of this most recent vector.  A short
## example how this can be achieved follows:
##
## @example
## function [fvec, fjac] = user_func (x, optimvalues, state)
## persistent sav = [], sav0 = [];
## if (nargin == 1)
##   ## evaluation call
##   if (nargout == 1)
##     sav0.x = x; # mark saved vector
##     ## calculate fvec, save results to sav0.
##   elseif (nargout == 2)
##     ## calculate fjac using sav.
##   endif
## else
##   ## outputfcn call.
##   if (all (x == sav0.x))
##     sav = sav0;
##   endif
##   ## maybe output iteration status, etc.
## endif
## endfunction
##
## ## @dots{}
##
## fsolve (@@user_func, x0, optimset ("OutputFcn", @@user_func, @dots{}))
## @end example
## @seealso{fzero, optimset}
## @end deftypefn

## PKG_ADD: ## Discard result to avoid polluting workspace with ans at startup.
## PKG_ADD: [~] = __all_opts__ ("fsolve");

function [x, fvec, info, output, fjac] = fsolve (fcn, x0, options = struct ())

  ## Get default options if requested.
  if (nargin == 1 && ischar (fcn) && strcmp (fcn, 'defaults'))
    x = optimset ("MaxIter", 400, "MaxFunEvals", Inf, ...
    "Jacobian", "off", "TolX", 1e-7, "TolFun", 1e-7,
    "OutputFcn", [], "Updating", "on", "FunValCheck", "off",
    "ComplexEqn", "off", "FinDiffType", "central",
    "TypicalX", [], "AutoScaling", "off");
    return;
  endif

  if (nargin < 2 || nargin > 3 || ! isnumeric (x0))
    print_usage ();
  endif

  if (ischar (fcn))
    fcn = str2func (fcn, "global");
  elseif (iscell (fcn))
    fcn = @(x) make_fcn_jac (x, fcn{1}, fcn{2});
  endif

  xsiz = size (x0);
  n = numel (x0);

  has_jac = strcmpi (optimget (options, "Jacobian", "off"), "on");
  cdif = strcmpi (optimget (options, "FinDiffType", "central"), "central");
  maxiter = optimget (options, "MaxIter", 400);
  maxfev = optimget (options, "MaxFunEvals", Inf);
  outfcn = optimget (options, "OutputFcn");
  updating = strcmpi (optimget (options, "Updating", "on"), "on");
  complexeqn = strcmpi (optimget (options, "ComplexEqn", "off"), "on");

  ## Get scaling matrix using the TypicalX option. If set to "auto", the
  ## scaling matrix is estimated using the Jacobian.
  typicalx = optimget (options, "TypicalX");
  if (isempty (typicalx))
    typicalx = ones (n, 1);
  endif
  autoscale = strcmpi (optimget (options, "AutoScaling", "off"), "on");
  if (! autoscale)
    dg = 1 ./ typicalx;
  endif

  funvalchk = strcmpi (optimget (options, "FunValCheck", "off"), "on");

  if (funvalchk)
    ## Replace fcn with a guarded version.
    fcn = @(x) guarded_eval (fcn, x, complexeqn);
  endif

  ## These defaults are rather stringent. I think that normally, user
  ## prefers accuracy to performance.

  macheps = eps (class (x0));

  tolx = optimget (options, "TolX", 1e-7);
  tolf = optimget (options, "TolFun", 1e-7);

  factor = 1;

  niter = 1;
  nfev = 1;

  x = x0(:);
  info = 0;

  ## Initial evaluation.
  ## Handle arbitrary shapes of x and f and remember them.
  fvec = fcn (reshape (x, xsiz));
  fsiz = size (fvec);
  fvec = fvec(:);
  fn = norm (fvec);
  m = length (fvec);
  n = length (x);

  if (! isempty (outfcn))
    optimvalues.iter = niter;
    optimvalues.funccount = nfev;
    optimvalues.fval = fn;
    optimvalues.searchdirection = zeros (n, 1);
    state = 'init';
    stop = outfcn (x, optimvalues, state);
    if (stop)
      info = -1;
      break;
    endif
  endif

  nsuciter = 0;

  ## Outer loop.
  while (niter < maxiter && nfev < maxfev && ! info)

    ## Calculate function value and Jacobian (possibly via FD).
    if (has_jac)
      [fvec, fjac] = fcn (reshape (x, xsiz));
      ## If the Jacobian is sparse, disable Broyden updating.
      if (issparse (fjac))
        updating = false;
      endif
      fvec = fvec(:);
      nfev ++;
    else
      fjac = __fdjac__ (fcn, reshape (x, xsiz), fvec, typicalx, cdif);
      nfev += (1 + cdif) * length (x);
    endif

    ## For square and overdetermined systems, we update a QR
    ## factorization of the Jacobian to avoid solving a full system in each
    ## step. In this case, we pass a triangular matrix to __dogleg__.
    useqr = updating && m >= n && n > 10;

    if (useqr)
      ## FIXME: Currently, pivoting is mostly useless because the \ operator
      ## cannot exploit the resulting props of the triangular factor.
      ## Unpivoted QR is significantly faster so it doesn't seem right to pivot
      ## just to get invariance. Original MINPACK didn't pivot either, at least
      ## when qr updating was used.
      [q, r] = qr (fjac, 0);
    endif

    if (autoscale)
      ## Get column norms, use them as scaling factors.
      jcn = norm (fjac, 'columns').';
      if (niter == 1)
        dg = jcn;
        dg(dg == 0) = 1;
      else
        ## Rescale adaptively.
        ## FIXME: the original minpack used the following rescaling strategy:
        ##   dg = max (dg, jcn);
        ## but it seems not good if we start with a bad guess yielding Jacobian
        ## columns with large norms that later decrease, because the
        ## corresponding variable will still be overscaled. So instead, we only
        ## give the old scaling a small momentum, but do not honor it.

        dg = max (0.1*dg, jcn);
      endif
    endif

    if (niter == 1)
      xn = norm (dg .* x);
      ## FIXME: something better?
      delta = factor * max (xn, 1);
    endif

    ## It also seems that in the case of fast (and inhomogeneously) changing
    ## Jacobian, the Broyden updates are of little use, so maybe we could
    ## skip them if a big disproportional change is expected. The question is,
    ## of course, how to define the above terms :)

    lastratio = 0;
    nfail = 0;
    nsuc = 0;
    decfac = 0.5;

    ## Inner loop.
    while (niter <= maxiter && nfev < maxfev && ! info)

      ## Get trust-region model (dogleg) minimizer.
      if (useqr)
        qtf = q'*fvec;
        s = - __dogleg__ (r, qtf, dg, delta);
        w = qtf + r * s;
      else
        s = - __dogleg__ (fjac, fvec, dg, delta);
        w = fvec + fjac * s;
      endif

      sn = norm (dg .* s);
      if (niter == 1)
        delta = min (delta, sn);
      endif

      fvec1 = fcn (reshape (x + s, xsiz)) (:);
      fn1 = norm (fvec1);
      nfev ++;

      if (fn1 < fn)
        ## Scaled actual reduction.
        actred = 1 - (fn1/fn)^2;
      else
        actred = -1;
      endif

      ## Scaled predicted reduction, and ratio.
      t = norm (w);
      if (t < fn)
        prered = 1 - (t/fn)^2;
        ratio = actred / prered;
      else
        prered = 0;
        ratio = 0;
      endif

      ## Update delta.
      if (ratio < min (max (0.1, 0.8*lastratio), 0.9))
        nsuc = 0;
        nfail ++;
        delta *= decfac;
        decfac ^= 1.4142;
        if (delta <= 1e1*macheps*xn)
          ## Trust region became uselessly small.
          info = -3;
          break;
        endif
      else
        lastratio = ratio;
        decfac = 0.5;
        nfail = 0;
        nsuc ++;
        if (abs (1-ratio) <= 0.1)
          delta = 1.4142*sn;
        elseif (ratio >= 0.5 || nsuc > 1)
          delta = max (delta, 1.4142*sn);
        endif
      endif

      if (ratio >= 1e-4)
        ## Successful iteration.
        x += s;
        xn = norm (dg .* x);
        fvec = fvec1;
        fn = fn1;
        nsuciter ++;
      endif

      niter ++;

      ## FIXME: should outputfcn be only called after a successful iteration?
      if (! isempty (outfcn))
        optimvalues.iter = niter;
        optimvalues.funccount = nfev;
        optimvalues.fval = fn;
        optimvalues.searchdirection = s;
        state = 'iter';
        stop = outfcn (x, optimvalues, state);
        if (stop)
          info = -1;
          break;
        endif
      endif

      ## Tests for termination conditions. A mysterious place, anything
      ## can happen if you change something here...

      ## The rule of thumb (which I'm not sure M*b is quite following)
      ## is that for a tolerance that depends on scaling, only 0 makes
      ## sense as a default value. But 0 usually means uselessly long
      ## iterations, so we need scaling-independent tolerances wherever
      ## possible.

      ## FIXME: Why tolf*n*xn? If abs (e) ~ abs(x) * eps is a vector
      ## of perturbations of x, then norm (fjac*e) <= eps*n*xn, i.e. by
      ## tolf ~ eps we demand as much accuracy as we can expect.
      if (fn <= tolf*n*xn)
        info = 1;
        ## The following tests done only after successful step.
      elseif (ratio >= 1e-4)
        ## This one is classic. Note that we use scaled variables again,
        ## but compare to scaled step, so nothing bad.
        if (sn <= tolx*xn)
          info = 2;
          ## Again a classic one. It seems weird to use the same tolf
          ## for two different tests, but that's what M*b manual appears
          ## to say.
        elseif (actred < tolf)
          info = 3;
        endif
      endif

      ## Criterion for recalculating Jacobian.
      if (! updating || nfail == 2 || nsuciter < 2)
        break;
      endif

      ## Compute the scaled Broyden update.
      if (useqr)
        u = (fvec1 - q*w) / sn;
        v = dg .* ((dg .* s) / sn);

        ## Update the QR factorization.
        [q, r] = qrupdate (q, r, u, v);
      else
        u = (fvec1 - w);
        v = dg .* ((dg .* s) / sn);

        ## update the Jacobian
        fjac += u * v';
      endif
    endwhile
  endwhile

  ## Restore original shapes.
  x = reshape (x, xsiz);
  fvec = reshape (fvec, fsiz);

  output.iterations = niter;
  output.successful = nsuciter;
  output.funcCount = nfev;

endfunction

## An assistant function that evaluates a function handle and checks for
## bad results.
function [fx, jx] = guarded_eval (fun, x, complexeqn)
  if (nargout > 1)
    [fx, jx] = fun (x);
  else
    fx = fun (x);
    jx = [];
  endif

  if (! complexeqn && ! (isreal (fx) && isreal (jx)))
    error ("fsolve:notreal", "fsolve: non-real value encountered");
  elseif (complexeqn && ! (isnumeric (fx) && isnumeric (jx)))
    error ("fsolve:notnum", "fsolve: non-numeric value encountered");
  elseif (any (isnan (fx(:))))
    error ("fsolve:isnan", "fsolve: NaN value encountered");
  elseif (any (isinf (fx(:))))
    error ("fsolve:isinf", "fsolve: Inf value encountered");
  endif
endfunction

function [fx, jx] = make_fcn_jac (x, fcn, fjac)
  fx = fcn (x);
  if (nargout == 2)
    jx = fjac (x);
  endif
endfunction


%!function retval = __f (p)
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  retval = zeros (3, 1);
%!  retval(1) = sin (x) + y^2 + log (z) - 7;
%!  retval(2) = 3*x + 2^y -z^3 + 1;
%!  retval(3) = x + y + z - 5;
%!endfunction
%!test
%! x_opt = [ 0.599054;
%!           2.395931;
%!           2.005014 ];
%! tol = 1.0e-5;
%! [x, fval, info] = fsolve (@__f, [ 0.5; 2.0; 2.5 ]);
%! assert (info > 0);
%! assert (norm (x - x_opt, Inf) < tol);
%! assert (norm (fval) < tol);

%!function retval = __f (p)
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  w = p(4);
%!  retval = zeros (4, 1);
%!  retval(1) = 3*x + 4*y + exp (z + w) - 1.007;
%!  retval(2) = 6*x - 4*y + exp (3*z + w) - 11;
%!  retval(3) = x^4 - 4*y^2 + 6*z - 8*w - 20;
%!  retval(4) = x^2 + 2*y^3 + z - w - 4;
%!endfunction
%!test
%! x_opt = [ -0.767297326653401, 0.590671081117440, ...
%!            1.47190018629642, -1.52719341133957 ];
%! tol = 1.0e-5;
%! [x, fval, info] = fsolve (@__f, [-1, 1, 2, -1]);
%! assert (info > 0);
%! assert (norm (x - x_opt, Inf) < tol);
%! assert (norm (fval) < tol);

%!function retval = __f (p)
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  retval = zeros (3, 1);
%!  retval(1) = sin (x) + y^2 + log (z) - 7;
%!  retval(2) = 3*x + 2^y -z^3 + 1;
%!  retval(3) = x + y + z - 5;
%!  retval(4) = x*x + y - z*log (z) - 1.36;
%!endfunction
%!test
%! x_opt = [ 0.599054;
%!           2.395931;
%!           2.005014 ];
%! tol = 1.0e-5;
%! [x, fval, info] = fsolve (@__f, [ 0.5; 2.0; 2.5 ]);
%! assert (info > 0);
%! assert (norm (x - x_opt, Inf) < tol);
%! assert (norm (fval) < tol);

%!function retval = __f (p)
%!  x = p(1);
%!  y = p(2);
%!  z = p(3);
%!  retval = zeros (3, 1);
%!  retval(1) = sin (x) + y^2 + log (z) - 7;
%!  retval(2) = 3*x + 2^y -z^3 + 1;
%!  retval(3) = x + y + z - 5;
%!endfunction
%!test
%! x_opt = [ 0.599054;
%!           2.395931;
%!           2.005014 ];
%! tol = 1.0e-5;
%! opt = optimset ("Updating", "qrp");
%! [x, fval, info] = fsolve (@__f, [ 0.5; 2.0; 2.5 ], opt);
%! assert (info > 0);
%! assert (norm (x - x_opt, Inf) < tol);
%! assert (norm (fval) < tol);

%!test
%! b0 = 3;
%! a0 = 0.2;
%! x = 0:.5:5;
%! noise = 1e-5 * sin (100*x);
%! y = exp (-a0*x) + b0 + noise;
%! c_opt = [a0, b0];
%! tol = 1e-5;
%!
%! [c, fval, info, output] =  fsolve (@(c) (exp(-c(1)*x) + c(2) - y), [0, 0]);
%! assert (info > 0);
%! assert (norm (c - c_opt, Inf) < tol);
%! assert (norm (fval) < norm (noise));

%!function y = cfun (x)
%!  y(1) = (1+i)*x(1)^2 - (1-i)*x(2) - 2;
%!  y(2) = sqrt (x(1)*x(2)) - (1-2i)*x(3) + (3-4i);
%!  y(3) = x(1) * x(2) - x(3)^2 + (3+2i);
%!endfunction

%!test
%! x_opt = [-1+i, 1-i, 2+i];
%! x = [i, 1, 1+i];
%!
%! [x, f, info] = fsolve (@cfun, x, optimset ("ComplexEqn", "on"));
%! tol = 1e-5;
%! assert (norm (f) < tol);
%! assert (norm (x - x_opt, Inf) < tol);

## Solve the double dogleg trust-region least-squares problem:
## Minimize norm(r*x-b) subject to the constraint norm(d.*x) <= delta,
## x being a convex combination of the gauss-newton and scaled gradient.

## TODO: error checks
## TODO: handle singularity, or leave it up to mldivide?

function x = __dogleg__ (r, b, d, delta)
  ## Get Gauss-Newton direction.
  x = r \ b;
  xn = norm (d .* x);
  if (xn > delta)
    ## GN is too big, get scaled gradient.
    s = (r' * b) ./ d;
    sn = norm (s);
    if (sn > 0)
      ## Normalize and rescale.
      s = (s / sn) ./ d;
      ## Get the line minimizer in s direction.
      tn = norm (r*s);
      snm = (sn / tn) / tn;
      if (snm < delta)
        ## Get the dogleg path minimizer.
        bn = norm (b);
        dxn = delta/xn; snmd = snm/delta;
        t = (bn/sn) * (bn/xn) * snmd;
        t -= dxn * snmd^2 - sqrt ((t-dxn)^2 + (1-dxn^2)*(1-snmd^2));
        alpha = dxn*(1-snmd^2) / t;
      else
        alpha = 0;
      endif
    else
      alpha = delta / xn;
      snm = 0;
    endif
    ## Form the appropriate convex combination.
    x = alpha * x + ((1-alpha) * min (snm, delta)) * s;
  endif
endfunction

