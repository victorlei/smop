## Copyright (C) 2000-2015 Paul Kienzle
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

## -*- texinfo -*-
## @deftypefn {Function File} {} isprime (@var{x})
## Return a logical array which is true where the elements of @var{x} are prime
## numbers and false where they are not.
##
## A prime number is conventionally defined as a positive integer greater than
## 1 (e.g., 2, 3, @dots{}) which is divisible only by itself and 1.  Octave
## extends this definition to include both negative integers and complex
## values.  A negative integer is prime if its positive counterpart is prime.
## This is equivalent to @code{isprime (abs (x))}.
##
## If @code{class (@var{x})} is complex, then primality is tested in the domain
## of Gaussian integers (@url{http://en.wikipedia.org/wiki/Gaussian_integer}).
## Some non-complex integers are prime in the ordinary sense, but not in the
## domain of Gaussian integers.  For example, @math{5 = (1+2i)*(1-2i)} shows
## that 5 is not prime because it has a factor other than itself and 1.
## Exercise caution when testing complex and real values together in the same
## matrix.
##
## Examples:
##
## @example
## @group
## isprime (1:6)
##     @result{} [0, 1, 1, 0, 1, 0]
## @end group
## @end example
##
## @example
## @group
## isprime ([i, 2, 3, 5])
##     @result{} [0, 0, 1, 0]
## @end group
## @end example
##
## Programming Note: @code{isprime} is appropriate if the maximum value in
## @var{x} is not too large (< 1e15).  For larger values special purpose
## factorization code should be used.
##
## Compatibility Note: @var{matlab} does not extend the definition of prime
## numbers and will produce an error if given negative or complex inputs.
## @seealso{primes, factor, gcd, lcm}
## @end deftypefn

function t = isprime (x)

  if (nargin != 1)
    print_usage ();
  elseif (any (fix (x) != x))
    error ("isprime: X contains non-integer entries");
  endif

  if (isempty (x))
    t = x;
    return;
  endif

  if (iscomplex (x))
    t = isgaussianprime (x);
    return;
  endif

  ## Code strategy is to build a table with the list of possible primes
  ## and then quickly compare entries in x with the table of primes using
  ## lookup().  The table size is limited to save memory and computation
  ## time during its creation.  All entries larger than the maximum in the
  ## table are checked by straightforward division.

  x = abs (x);              # handle negative entries
  maxn = max (x(:));
  ## generate prime table of suitable length.
  ## 1e7 threshold requires ~0.15 seconds of computation, 1e8 requires 1.8.
  maxp = min (maxn, max (sqrt (maxn), 1e7));
  pr = primes (maxp);
  t = lookup (pr, x, "b");  # quick search for table matches.

  ## process any remaining large entries
  m = x(x > maxp);
  if (! isempty (m))
    if (maxn <= intmax ("uint32"))
      m = uint32 (m);
    elseif (maxn <= intmax ("uint64"))
      m = uint64 (m);
    else
      warning ("isprime: X contains integers too large to be tested");
    endif

    ## Start by dividing through by the small primes until the remaining
    ## list of entries is small (and most likely prime themselves).
    pr = cast (pr(pr <= sqrt (maxn)), class (m));
    for p = pr
      m = m(rem (m, p) != 0);
      if (numel (m) < numel (pr) / 10)
        break;
      endif
    endfor

    ## Check the remaining list of possible primes against the
    ## remaining prime factors which were not tested in the for loop.
    ## This is just an optimization to use arrayfun over for loo
    pr = pr(pr > p);
    mm = arrayfun (@(x) all (rem (x, pr)), m);
    m = m(mm);

    ## Add any remaining entries, which are truly prime, to the results.
    if (! isempty (m))
      m = cast (sort (m), class (x));
      t |= lookup (m, x, "b");
    endif
  endif

endfunction

function t = isgaussianprime (z)
  ## Assume prime unless proven otherwise
  t = true (size (z));

  x = real (z);
  y = imag (z);

  ## If purely real or purely imaginary, ordinary prime test for
  ## that complex part if that part is 3 mod 4.
  xidx = y==0 & mod (x, 4) == 3;
  yidx = x==0 & mod (y, 4) == 3;

  t(xidx) &= isprime (x(xidx));
  t(yidx) &= isprime (y(yidx));

  ## Otherwise, prime if x^2 + y^2 is prime
  zidx = ! (xidx | yidx);          # Skip entries that were already evaluated
  zabs = x(zidx).^2 + y(zidx).^2;
  t(zidx) &= isprime (zabs);
endfunction


%!assert (isprime (3), true)
%!assert (isprime (4), false)
%!assert (isprime (5i), false)
%!assert (isprime (7i), true)
%!assert (isprime ([1+2i, (2+3i)*(-1+2i)]), [true, false])
%!assert (isprime (-2), true)
%!assert (isprime (complex (-2)), false)
%!assert (isprime (2i), false)
%!assert (isprime ([i, 2, 3, 5]), [false, false, true, false])
%!assert (isprime (0), false)
%!assert (isprime (magic (3)), logical ([0, 0, 0; 1, 1, 1; 0, 0, 1]))

## Test input validation
%!error isprime ()
%!error isprime (1, 2)
%!error <X contains non-integer entries> isprime (0.5i)
%!error <X contains non-integer entries> isprime (0.5)

