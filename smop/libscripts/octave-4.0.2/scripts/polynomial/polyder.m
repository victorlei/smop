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
## @deftypefn  {Function File} {} polyder (@var{p})
## @deftypefnx {Function File} {[@var{k}] =} polyder (@var{a}, @var{b})
## @deftypefnx {Function File} {[@var{q}, @var{d}] =} polyder (@var{b}, @var{a})
## Return the coefficients of the derivative of the polynomial whose
## coefficients are given by the vector @var{p}.
##
## If a pair of polynomials is given, return the derivative of the product
## @math{@var{a}*@var{b}}.
##
## If two inputs and two outputs are given, return the derivative of the
## polynomial quotient @math{@var{b}/@var{a}}.  The quotient numerator is
## in @var{q} and the denominator in @var{d}.
## @seealso{polyint, polyval, polyreduce}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function [q, d] = polyder (p, a)

  if (nargin == 1 || nargin == 2)
    if (! isvector (p))
      error ("polyder: argument must be a vector");
    endif
    if (nargin == 2)
      if (! isvector (a))
        error ("polyder: argument must be a vector");
      endif
      if (nargout == 1)
        ## derivative of p*a returns a single polynomial
        q = polyder (conv (p, a));
      else
        ## derivative of p/a returns numerator and denominator
        d = conv (a, a);
        if (numel (p) == 1)
          q = -p * polyder (a);
        elseif (numel (a) == 1)
          q = a * polyder (p);
        else
          q = conv (polyder (p), a) - conv (p, polyder (a));
          q = polyreduce (q);
        endif

        ## remove common factors from numerator and denominator
        x = polygcd (q, d);
        if (length (x) != 1)
          q = deconv (q, x);
          d = deconv (d, x);
        endif

        ## move all the gain into the numerator
        q = q/d(1);
        d = d/d(1);
      endif
    else
      lp = numel (p);
      if (lp == 1)
        q = 0;
        return;
      elseif (lp == 0)
        q = [];
        return;
      endif

      ## Force P to be a row vector.
      p = p(:).';

      q = p(1:(lp-1)) .* [(lp-1):-1:1];
    endif
  else
    print_usage ();
  endif

endfunction


%!assert (polyder ([1, 2, 3], [2, 2]))
%!assert (polyder (13), 0)

%!error polyder ([])
%!error polyder (1,2,3)
%!error <argument must be a vector> polyder ([1, 2; 3, 4])

