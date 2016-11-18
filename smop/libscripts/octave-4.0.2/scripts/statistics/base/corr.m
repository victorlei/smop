## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn  {Function File} {} corr (@var{x})
## @deftypefnx {Function File} {} corr (@var{x}, @var{y})
## Compute matrix of correlation coefficients.
##
## If each row of @var{x} and @var{y} is an observation and each column is
## a variable, then the @w{(@var{i}, @var{j})-th} entry of
## @code{corr (@var{x}, @var{y})} is the correlation between the
## @var{i}-th variable in @var{x} and the @var{j}-th variable in @var{y}.
## @tex
## $$
## {\rm corr}(x,y) = {{\rm cov}(x,y) \over {\rm std}(x) {\rm std}(y)}
## $$
## @end tex
## @ifnottex
##
## @example
## corr (x,y) = cov (x,y) / (std (x) * std (y))
## @end example
##
## @end ifnottex
## If called with one argument, compute @code{corr (@var{x}, @var{x})},
## the correlation between the columns of @var{x}.
## @seealso{cov}
## @end deftypefn

## Author: Kurt Hornik <hornik@wu-wien.ac.at>
## Created: March 1993
## Adapted-By: jwe

function retval = corr (x, y = [])

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  ## Input validation is done by cov.m.  Don't repeat tests here

  ## Special case, scalar is always 100% correlated with itself
  if (isscalar (x))
    if (isa (x, "single"))
      retval = single (1);
    else
      retval = 1;
    endif
    return;
  endif

  ## No check for division by zero error, which happens only when
  ## there is a constant vector and should be rare.
  if (nargin == 2)
    c = cov (x, y);
    s = std (x)' * std (y);
    retval = c ./ s;
  else
    c = cov (x);
    s = sqrt (diag (c));
    retval = c ./ (s * s');
  endif

endfunction


%!test
%! x = rand (10);
%! cc1 = corr (x);
%! cc2 = corr (x, x);
%! assert (size (cc1) == [10, 10] && size (cc2) == [10, 10]);
%! assert (cc1, cc2, sqrt (eps));

%!test
%! x = [1:3]';
%! y = [3:-1:1]';
%! assert (corr (x, y), -1, 5*eps);
%! assert (corr (x, flipud (y)), 1, 5*eps);
%! assert (corr ([x, y]), [1 -1; -1 1], 5*eps);

%!test
%! x = single ([1:3]');
%! y = single ([3:-1:1]');
%! assert (corr (x, y), single (-1), 5*eps);
%! assert (corr (x, flipud (y)), single (1), 5*eps);
%! assert (corr ([x, y]), single ([1 -1; -1 1]), 5*eps);

%!assert (corr (5), 1)
%!assert (corr (single (5)), single (1))

## Test input validation
%!error corr ()
%!error corr (1, 2, 3)
%!error corr ([1; 2], ["A", "B"])
%!error corr (ones (2,2,2))
%!error corr (ones (2,2), ones (2,2,2))

