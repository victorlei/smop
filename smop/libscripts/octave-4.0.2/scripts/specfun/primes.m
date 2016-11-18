## Copyright (C) 2000-2015 Paul Kienzle
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
## @deftypefn {Function File} {} primes (@var{n})
## Return all primes up to @var{n}.
##
## The output data class (double, single, uint32, etc.) is the same as the
## input class of @var{n}.  The algorithm used is the Sieve of Eratosthenes.
##
## Notes: If you need a specific number of primes you can use the fact that the
## distance from one prime to the next is, on average, proportional to the
## logarithm of the prime.  Integrating, one finds that there are about
## @math{k} primes less than
## @tex
## $k \log (5 k)$.
## @end tex
## @ifnottex
## k*log (5*k).
## @end ifnottex
##
## See also @code{list_primes} if you need a specific number @var{n} of primes.
## @seealso{list_primes, isprime}
## @end deftypefn

## Author: Paul Kienzle
## Author: Francesco Potortì
## Author: Dirk Laurie

function p = primes (n)

  if (nargin != 1)
    print_usage ();
  endif

  if (! isscalar (n))
    error ("primes: N must be a scalar");
  endif

  if (n > 100e3)
    ## Optimization: 1/6 less memory, and much faster (asymptotically)
    ## 100K happens to be the cross-over point for Paul's machine;
    ## below this the more direct code below is faster.  At the limit
    ## of memory in Paul's machine, this saves .7 seconds out of 7 for
    ## n = 3e6.  Hardly worthwhile, but Dirk reports better numbers.
    lenm = floor ((n+1)/6);       # length of the 6n-1 sieve
    lenp = floor ((n-1)/6);       # length of the 6n+1 sieve
    sievem = true (1, lenm);      # assume every number of form 6n-1 is prime
    sievep = true (1, lenp);      # assume every number of form 6n+1 is prime

    for i = 1:(sqrt (n)+1)/6      # check up to sqrt (n)
      if (sievem(i))              # if i is prime, eliminate multiples of i
        sievem(7*i-1:6*i-1:lenm) = false;
        sievep(5*i-1:6*i-1:lenp) = false;
      endif                       # if i is prime, eliminate multiples of i
      if (sievep(i))
        sievep(7*i+1:6*i+1:lenp) = false;
        sievem(5*i+1:6*i+1:lenm) = false;
      endif
    endfor
    p = sort ([2, 3, 6*find(sievem)-1, 6*find(sievep)+1]);
  elseif (n > 352)                # nothing magical about 352; must be > 2
    len = floor ((n-1)/2);        # length of the sieve
    sieve = true (1, len);        # assume every odd number is prime
    for i = 1:(sqrt (n)-1)/2      # check up to sqrt (n)
      if (sieve(i))               # if i is prime, eliminate multiples of i
        sieve(3*i+1:2*i+1:len) = false; # do it
      endif
    endfor
    p = [2, 1+2*find(sieve)];     # primes remaining after sieve
  else
    a = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, ...
         53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, ...
         109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, ...
         173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, ...
         233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, ...
         293, 307, 311, 313, 317, 331, 337, 347, 349];
    p = a(a <= n);
  endif

  if (! isa (n, "double"))
    cast (p, class (n));
  endif

endfunction


%!assert (size (primes (350)), [1, 70])
%!assert (primes (357)(end), 353)

%!error primes ()
%!error primes (1, 2)
%!error <N must be a scalar> primes (ones (2,2))

