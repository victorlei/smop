## Copyright (C) 1995-2015 Kurt Hornik
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
## @deftypefn {Function File} {} detrend (@var{x}, @var{p})
## If @var{x} is a vector, @code{detrend (@var{x}, @var{p})} removes the
## best fit of a polynomial of order @var{p} from the data @var{x}.
##
## If @var{x} is a matrix, @code{detrend (@var{x}, @var{p})} does the same
## for each column in @var{x}.
##
## The second argument @var{p} is optional.  If it is not specified, a value of
## 1 is assumed.  This corresponds to removing a linear trend.
##
## The order of the polynomial can also be given as a string, in which case
## @var{p} must be either @qcode{"constant"} (corresponds to @code{@var{p}=0})
## or @qcode{"linear"} (corresponds to @code{@var{p}=1}).
## @seealso{polyfit}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 11 October 1994
## Adapted-By: jwe

function y = detrend (x, p = 1)
  ## Check input
  if (nargin > 0 && isreal (x) && ndims (x) <= 2)
    ## Check p
    if (ischar (p) && strcmpi (p, "constant"))
      p = 0;
    elseif (ischar (p) && strcmpi (p, "linear"))
      p = 1;
    elseif (! isscalar (p) || p < 0 || p != fix (p))
      error ("detrend: second input argument must be 'constant', 'linear' or a positive integer");
    endif
  else
    error ("detrend: first input argument must be a real vector or matrix");
  endif

  [m, n] = size (x);
  if (m == 1)
    x = x';
  endif

  r = rows (x);
  b = ((1 : r)' * ones (1, p + 1)) .^ (ones (r, 1) * (0 : p));
  y = x - b * (b \ x);

  if (m == 1)
    y = y';
  endif

endfunction


%!test
%! N = 32;
%! x = (0:1:N-1)/N + 2;
%! y = detrend (x);
%! assert (abs (y(:)) < 20*eps);

%!test
%! N = 32;
%! t = (0:1:N-1)/N;
%! x = t .* t + 2;
%! y = detrend (x,2);
%! assert (abs (y(:)) < 30*eps);

%!test
%! N = 32;
%! t = (0:1:N-1)/N;
%! x = [t;4*t-3]';
%! y = detrend (x);
%! assert (abs (y(:)) < 20*eps);

