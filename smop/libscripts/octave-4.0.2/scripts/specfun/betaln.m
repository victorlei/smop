## Copyright (C) 1998-2015 Nicol N. Schraudolph
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
## @deftypefn {Mapping Function} {} betaln (@var{a}, @var{b})
## Compute the natural logarithm of the Beta function for real inputs @var{a}
## and @var{b}.
##
## @code{betaln} is defined as
## @tex
## $$
##  {\rm betaln} (a, b) = \ln (B (a,b)) \equiv \ln ({\Gamma (a) \Gamma (b) \over \Gamma (a + b)}).
## $$
## @end tex
## @ifnottex
##
## @example
## betaln (a, b) = log (beta (a, b))
## @end example
##
## @end ifnottex
## and is calculated in a way to reduce the occurrence of underflow.
##
## The Beta function can grow quite large and it is often more useful to work
## with the logarithm of the output rather than the function directly.
## @seealso{beta, betainc, betaincinv, gammaln}
## @end deftypefn

## Author:   Nicol N. Schraudolph <nic@idsia.ch>
## Created:  06 Aug 1998
## Keywords: log beta special function

function retval = betaln (a, b)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isreal (a) || ! isreal (b))
    error ("betaln: A and B must be real");
  elseif (! size_equal (a, b) && numel (a) != 1 && numel (b) != 1)
    error ("betaln: A and B must have consistent sizes");
  endif

  retval = gammaln (a) + gammaln (b) - gammaln (a + b);

endfunction


%!assert (betaln (3,4), log (beta (3,4)), eps)

## Test input validation
%!error betaln ()
%!error betaln (1)
%!error betaln (1,2,3)
%!error <A and B must be real> betaln (1i, 2)
%!error <A and B must be real> betaln (2, 1i)
%!error <A and B must have consistent sizes> betaln ([1 2], [1 2 3])
%!error <A and B must have consistent sizes> betaln ([1 2 3], [1 2])
%!error <A and B must have consistent sizes> betaln ([1 2 3], [1 2 3]')

