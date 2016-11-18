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
## @deftypefn  {Function File} {@var{pf} =} factor (@var{q})
## @deftypefnx {Function File} {[@var{pf}, @var{n}] =} factor (@var{q})
## Return the prime factorization of @var{q}.
##
## The prime factorization is defined as @code{prod (@var{pf}) == @var{q}}
## where every element of @var{pf} is a prime number.  If @code{@var{q} == 1},
## return 1.
##
## With two output arguments, return the unique prime factors @var{pf} and
## their multiplicities.  That is, @code{prod (@var{pf} .^ @var{n}) == @var{q}}.
##
## Implementation Note: The input @var{q} must be less than
## @code{bitmax} (9.0072e+15) in order to factor correctly.
## @seealso{gcd, lcm, isprime, primes}
## @end deftypefn

## Author: Paul Kienzle

## 2002-01-28 Paul Kienzle
## * remove recursion; only check existing primes for multiplicity > 1
## * return multiplicity as suggested by Dirk Laurie
## * add error handling

function [pf, n] = factor (q)

  if (nargin != 1)
    print_usage ();
  endif

  if (! isreal (q) || ! isscalar (q) || q != fix (q))
    error ("factor: Q must be a real integer");
  endif

  ## Special case of no primes less than sqrt(q).
  if (q < 4)
    pf = q;
    n = 1;
    return;
  endif

  q = double (q);  # For the time being, calcs rely on double precision var.
  qorig = q;
  pf = [];
  ## There is at most one prime greater than sqrt(q), and if it exists,
  ## it has multiplicity 1, so no need to consider any factors greater
  ## than sqrt(q) directly.  [If there were two factors p1, p2 > sqrt(q),
  ## then q >= p1*p2 > sqrt(q)*sqrt(q) == q. Contradiction.]
  p = primes (sqrt (q));
  while (q > 1)
    ## Find prime factors in remaining q.
    p = p(rem (q, p) == 0);
    if (isempty (p))
      ## Can't be reduced further, so q must itself be a prime.
      p = q;
    endif
    pf = [pf, p];
    ## Reduce q.
    q /= prod (p);
  endwhile
  pf = sort (pf);

  ## Verify algorithm was succesful
  q = prod (pf);
  if (q != qorig)
    error ("factor: Q too large to factor");
  elseif (q > bitmax)
    warning ("factor: Q too large.  Answer is unreliable");
  endif

  ## Determine muliplicity.
  if (nargout > 1)
    idx = find ([0, pf] != [pf, 0]);
    pf = pf(idx(1:length (idx)-1));
    n = diff (idx);
  endif

endfunction


%!assert (factor (1), 1)
%!test
%! for i = 2:20
%!   pf = factor (i);
%!   assert (prod (pf), i);
%!   assert (all (isprime (pf)));
%!   [pf, n] = factor (i);
%!   assert (prod (pf.^n), i);
%!   assert (all ([0,pf] != [pf,0]));
%! endfor

## Test input validation
%!error factor ()
%!error factor (1,2)
%!error <Q must be a real integer> factor (6i)
%!error <Q must be a real integer> factor ([1,2])
%!error <Q must be a real integer> factor (1.5)

