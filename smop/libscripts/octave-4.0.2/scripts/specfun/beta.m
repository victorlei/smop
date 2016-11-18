## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn {Mapping Function} {} beta (@var{a}, @var{b})
## Compute the Beta function for real inputs @var{a} and @var{b}.
##
## The Beta function definition is
## @tex
## $$
##  B (a, b) = {\Gamma (a) \Gamma (b) \over \Gamma (a + b)}.
## $$
## @end tex
## @ifnottex
##
## @example
## beta (a, b) = gamma (a) * gamma (b) / gamma (a + b).
## @end example
##
## @end ifnottex
##
## The Beta function can grow quite large and it is often more useful to work
## with the logarithm of the output rather than the function directly.
## @xref{XREFbetaln,,betaln}, for computing the logarithm of the Beta function
## in an efficient manner.
## @seealso{betaln, betainc, betaincinv}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 13 June 1993
## Adapted-By: jwe

function retval = beta (a, b)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isreal (a) || ! isreal (b))
    error ("beta: A and B must be real");
  elseif (! size_equal (a, b) && numel (a) != 1 && numel (b) != 1)
    error ("beta: A and B must have consistent sizes");
  endif

  retval = real (exp (gammaln (a) + gammaln (b) - gammaln (a+b)));

endfunction


%!test
%! a = [1, 1.5, 2, 3];
%! b = [4, 3, 2, 1];
%! v1 = beta (a, b);
%! v2 = beta (b, a);
%! v3 = gamma (a).*gamma (b) ./ gamma (a+b);
%! assert (v1, v2, sqrt (eps));
%! assert (v2, v3, sqrt (eps));

%!assert (beta (1, 1), 1)

%!test
%! a = 2:10;
%! tol = 10 * max (a) * eps;
%! assert (-a, beta (-1./a, 1), tol);
%! assert (-a, beta (1, -1./a), tol);

%!test
%! a = 0.25 + (0:5) * 0.5;
%! tol = 10 * max (a) * eps;
%! assert (zeros (size (a)), beta (a, -a), tol);
%! assert (zeros (size (a)), beta (-a, a), tol);

%!error beta ()
%!error beta (1)
%!error beta (1,2,3)
%!error <A and B must be real> beta (1i, 2)
%!error <A and B must be real> beta (2, 1i)
%!error <A and B must have consistent sizes> beta ([1 2], [1 2 3])
%!error <A and B must have consistent sizes> beta ([1 2 3], [1 2])
%!error <A and B must have consistent sizes> beta ([1 2 3], [1 2 3]')

