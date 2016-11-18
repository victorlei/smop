## Copyright (C) 2004-2015 Paul Kienzle
## Copyright (C) 2010 VZLU Prague
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
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn {Function File} {} nthroot (@var{x}, @var{n})
##
## Compute the real (non-complex) @var{n}-th root of @var{x}.
##
## @var{x} must have all real entries and @var{n} must be a scalar.
## If @var{n} is an even integer and @var{x} has negative entries then
## @code{nthroot} aborts and issues an error.
##
## Example:
##
## @example
## @group
## nthroot (-1, 3)
## @result{} -1
## (-1) ^ (1 / 3)
## @result{} 0.50000 - 0.86603i
## @end group
## @end example
## @seealso{realsqrt, sqrt, cbrt}
## @end deftypefn

function y = nthroot (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (iscomplex (x))
    error ("nthroot: X must not contain complex values");
  endif

  if (! isreal (n) || ! isscalar (n) || n == 0)
    error ("nthroot: N must be a real nonzero scalar");
  endif

  if (n == 3)
    y = cbrt (x);
  elseif (n == -3)
    y = 1 ./ cbrt (x);
  elseif (n < 0)
    y = 1 ./ nthroot (x, -n);
  else
    ## Compute using power.
    integer_n = n == fix (n);
    if (integer_n && mod (n, 2) == 1)
      y = abs (x) .^ (1/n) .* sign (x);
    elseif (any (x(:) < 0))
      error ("nthroot: N must be an odd integer if X contains negative values");
    else
      y = x .^ (1/n);
    endif

    if (integer_n && n > 0 && isfinite (n))
      if (isscalar (y) && y == 0)
        ## Don't apply correction which leads to division by zero (bug #43492)
      else
        ## FIXME: What is this correction for?
        y = ((n-1)*y + x ./ (y.^(n-1))) / n;
        y = merge (isfinite (y), y, x);
      endif
    endif
  endif

endfunction


%!assert (nthroot (-32, 5), -2)
%!assert (nthroot (81, 4), 3)
%!assert (nthroot (Inf, 4), Inf)
%!assert (nthroot (-Inf, 7), -Inf)
%!assert (nthroot (-Inf, -7), 0)

## Bug #43492.  This should not generate a division by zero warning
%!test
%! warnmsg = lastwarn ();
%! assert (nthroot (0, 2), 0);
%! assert (lastwarn (), warnmsg);

## Test input validation
%!error nthroot ()
%!error nthroot (1)
%!error nthroot (1,2,3)
%!error <X must not contain complex values> nthroot (1+j, 2)
%!error <N must be a real nonzero scalar> nthroot (1, i)
%!error <N must be a real nonzero scalar> nthroot (1, [1 2])
%!error <N must be a real nonzero scalar> nthroot (1, 0)
%!error <N must be an odd integer> nthroot (-1, 2)

