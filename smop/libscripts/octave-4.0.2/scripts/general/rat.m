## Copyright (C) 2001-2015 Paul Kienzle
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
## @deftypefn  {Function File} {@var{s} =} rat (@var{x}, @var{tol})
## @deftypefnx {Function File} {[@var{n}, @var{d}] =} rat (@var{x}, @var{tol})
##
## Find a rational approximation to @var{x} within the tolerance defined by
## @var{tol} using a continued fraction expansion.
##
## For example:
##
## @example
## @group
## rat (pi) = 3 + 1/(7 + 1/16) = 355/113
## rat (e) = 3 + 1/(-4 + 1/(2 + 1/(5 + 1/(-2 + 1/(-7)))))
##         = 1457/536
## @end group
## @end example
##
## When called with two output arguments return the numerator and denominator
## separately as two matrices.
## @seealso{rats}
## @end deftypefn

function [n,d] = rat (x,tol)

  if (nargin != [1,2] || nargout > 2)
    print_usage ();
  endif

  y = x(:);

  ## Replace Inf with 0 while calculating ratios.
  y(isinf(y)) = 0;

  ## default norm
  if (nargin < 2)
    tol = 1e-6 * norm (y,1);
  endif

  ## First step in the approximation is the integer portion

  ## First element in the continued fraction.
  n = round (y);
  d = ones (size (y));
  frac = y-n;
  lastn = ones (size (y));
  lastd = zeros (size (y));

  nd = ndims (y);
  nsz = numel (y);
  steps = zeros ([nsz, 0]);

  ## Grab new factors until all continued fractions converge.
  while (1)
    ## Determine which fractions have not yet converged.
    idx = find (abs (y-n./d) >= tol);
    if (isempty (idx))
      if (isempty (steps))
        steps = NaN (nsz, 1);
      endif
      break;
    endif

    ## Grab the next step in the continued fraction.
    flip = 1./frac(idx);
    ## Next element in the continued fraction.
    step = round (flip);

    if (nargout < 2)
      tsteps = NaN (nsz, 1);
      tsteps (idx) = step;
      steps = [steps, tsteps];
    endif

    frac(idx) = flip-step;

    ## Update the numerator/denominator.
    nextn = n;
    nextd = d;
    n(idx) = n(idx).*step + lastn(idx);
    d(idx) = d(idx).*step + lastd(idx);
    lastn = nextn;
    lastd = nextd;
  endwhile

  if (nargout == 2)
    ## Move the minus sign to the top.
    n = n .* sign (d);
    d = abs (d);

    ## Return the same shape as you receive.
    n = reshape (n, size (x));
    d = reshape (d, size (x));

    ## Use 1/0 for Inf.
    n(isinf (x)) = sign (x(isinf (x)));
    d(isinf (x)) = 0;

    ## Reshape the output.
    n = reshape (n, size (x));
    d = reshape (d, size (x));
  else
    n = "";
    nsteps = columns (steps);
    for i = 1: nsz
      s = [int2str(y(i))," "];
      j = 1;

      while (true)
        step = steps(i, j++);
        if (isnan (step))
          break;
        endif
        if (j > nsteps || isnan (steps(i, j)))
          if (step < 0)
            s = [s(1:end-1), " + 1/(", int2str(step), ")"];
          else
            s = [s(1:end-1), " + 1/", int2str(step)];
          endif
          break;
        else
          s = [s(1:end-1), " + 1/(", int2str(step), ")"];
        endif
      endwhile
      s = [s, repmat(")", 1, j-2)];
      n_nc = columns (n);
      s_nc = columns (s);
      if (n_nc > s_nc)
        s(:,s_nc+1:n_nc) = " ";
      elseif (s_nc > n_nc && n_nc != 0)
        n(:,n_nc+1:s_nc) = " ";
      endif
      n = cat (1, n, s);
    endfor
  endif

endfunction


%!test
%! [n, d] = rat ([0.5, 0.3, 1/3]);
%! assert (n, [1, 3, 1]);
%! assert (d, [2, 10, 3]);

## bug #43374
%!assert (eval (rat (0.75)), [0.75])

%!error rat ();
%!error rat (1, 2, 3);

