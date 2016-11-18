## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn  {Function File} {} list_primes ()
## @deftypefnx {Function File} {} list_primes (@var{n})
## List the first @var{n} primes.
##
## If @var{n} is unspecified, the first 25 primes are listed.
## @seealso{primes, isprime}
## @end deftypefn

## Author: jwe

function retval = list_primes (n = 25)

  if (nargin > 1)
    print_usage ();
  elseif (! isreal (n) || ! isscalar (n))
    error ("list_primes: N must be a real scalar");
  endif

  n = floor (n);

  if (n < 1)
    retval = [];
    return;
  elseif (n == 1)
    retval = 2;
    return;
  endif

  list = primes (n * log (5 * n));
  if (numel (list) < n)
    ## Algorithm tested up to n=10,000 without failure.
    error ("list_primes: Algorithm failed.  Try primes (n*log (6*n))(1:n)");
  endif

  retval = list(1:n);

endfunction


%!assert (list_primes (), [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, ...
%!                         43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97])
%!assert (list_primes (5), [2, 3, 5, 7, 11])

%!assert (list_primes (0), [])
%!assert (list_primes (1), [2])

## Test input validation
%!error list_primes (1, 2)
%!error <N must be a real scalar> list_primes (i)
%!error <N must be a real scalar> list_primes ([1 2])

