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
## @deftypefn  {Function File} {@var{q} =} polygcd (@var{b}, @var{a})
## @deftypefnx {Function File} {@var{q} =} polygcd (@var{b}, @var{a}, @var{tol})
##
## Find the greatest common divisor of two polynomials.
##
## This is equivalent to the polynomial found by multiplying together all the
## common roots.  Together with deconv, you can reduce a ratio of two
## polynomials.
##
## The tolerance @var{tol} defaults to @code{sqrt (eps)}.
##
## @strong{Caution:} This is a numerically unstable algorithm and should not
## be used on large polynomials.
##
## Example code:
##
## @example
## @group
## polygcd (poly (1:8), poly (3:12)) - poly (3:8)
## @result{} [ 0, 0, 0, 0, 0, 0, 0 ]
## deconv (poly (1:8), polygcd (poly (1:8), poly (3:12))) - poly (1:2)
## @result{} [ 0, 0, 0 ]
## @end group
## @end example
## @seealso{poly, roots, conv, deconv, residue}
## @end deftypefn

function x = polygcd (b, a, tol)

  if (nargin == 2 || nargin == 3)
    if (nargin == 2)
      if (isa (a, "single") || isa (b, "single"))
        tol = sqrt (eps ("single"));
      else
        tol = sqrt (eps);
      endif
    endif
    if (length (a) == 1 || length (b) == 1)
      if (a == 0)
        x = b;
      elseif (b == 0)
        x = a;
      else
        x = 1;
      endif
    else
      a /= a(1);
      while (1)
        [d, r] = deconv (b, a);
        nz = find (abs (r) > tol);
        if (isempty (nz))
          x = a;
          break;
        else
          r = r(nz(1):length(r));
        endif
        b = a;
        a = r / r(1);
      endwhile
    endif
  else
    print_usage ();
  endif

endfunction


%!test
%! poly1 = [1 6 11 6]; # (x+1)(x+2)(x+3);
%! poly2 = [1 3 2];    # (x+1)(x+2);
%! poly3 = polygcd (poly1, poly2);
%! assert (poly3, poly2, sqrt (eps));

%!assert (polygcd (poly (1:8), poly (3:12)), poly (3:8), sqrt (eps))
%!assert (deconv (poly (1:8), polygcd (poly (1:8), poly (3:12))), poly (1:2), sqrt (eps))

%!test
%! for ii=1:100
%!   ## Exhibits numerical problems for multipliers of ~4 and greater.
%!   p  = (unique (randn (10, 1)) * 3).';
%!   p1 = p(3:end);
%!   p2 = p(1:end-2);
%!   assert (polygcd (poly (-p1), poly (-p2)),
%!           poly (- intersect (p1, p2)), sqrt (eps));
%! endfor

