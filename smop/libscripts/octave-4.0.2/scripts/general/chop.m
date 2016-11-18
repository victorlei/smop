## Copyright (C) 2010-2015 John W. Eaton
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
## @deftypefn {Function File} {} chop (@var{x}, @var{ndigits}, @var{base})
## Truncate elements of @var{x} to a length of @var{ndigits} such that the
## resulting numbers are exactly divisible by @var{base}.
##
## If @var{base} is not specified it defaults to 10.
##
## @example
## @group
## chop (-pi, 5, 10)
##    @result{} -3.14200000000000
## chop (-pi, 5, 5)
##    @result{} -3.14150000000000
## @end group
## @end example
## @end deftypefn

function retval = chop (x, ndigits, base = 10)

  if (nargin == 2 || nargin == 3)
    tmp = abs (x);

    ## Avoid computing log (0).
    tmp(x == 0) = 1;

    ## Digits to the left of the decimal.
    tmp = floor (log10 (tmp) + 1);

    ## The expression
    ##
    ##   round (x .* inflate)
    ##
    ## produces an integer that contains the digits we want to keep.
    ## Multiplying by deflate puts the decimal back where it belngs.
    ##
    ## Further scaling and rounding with the base factor produces a
    ## value with ndigits exactly divisible by base.  We skip that step
    ## unless base was explicitly provided.

    inflate = 10 .^ (ndigits - tmp);
    deflate = 1 ./ inflate;
    if (nargin == 2)
      retval = deflate .* round (x .* inflate);
    else
      retval = base .* deflate .* round (round (x .* inflate) ./ base);
    endif
  else
    print_usage ();
  endif

endfunction


%!assert (chop (e, 3), 2.72)
%!assert (chop (e, 4), 2.718)
%!assert (chop (e, 4, 5), 2.72)
%!assert (chop (e, 4, 7), 2.716)
%!assert (chop (-e, 3), -2.72)
%!assert (chop (-e, 4), -2.718)
%!assert (chop (-e, 4, 5), -2.72)
%!assert (chop (-e, 4, 7), -2.716)
%!assert (chop (hilb (3), 3), [1,.5,.333;.5,.333,.25;.333,.25,.2])
%!assert (chop (hilb (3), 2, 7), [.7,.49,.35;.49,.35,.28;.35,.28,.21], 2*eps)

