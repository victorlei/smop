## Copyright (C) 1995-2015 Kurt Hornik
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
## @deftypefn  {Function File} {} pow2 (@var{x})
## @deftypefnx {Function File} {} pow2 (@var{f}, @var{e})
## With one input argument, compute
## @tex
## $2^x$
## @end tex
## @ifnottex
## 2 .^ x
## @end ifnottex
## for each element of @var{x}.
##
## With two input arguments, return
## @tex
## $f \cdot 2^e$.
## @end tex
## @ifnottex
## f .* (2 .^ e).
## @end ifnottex
## @seealso{log2, nextpow2, power}
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 17 October 1994
## Adapted-By: jwe

function y = pow2 (f, e)

  if (nargin == 1)
    y = 2 .^ f;
  elseif (nargin == 2)
    y = f .* (2 .^ e);
  else
    print_usage ();
  endif

endfunction


%!test
%! x = [3, 0, -3];
%! v = [8, 1, .125];
%! assert (pow2 (x), v, sqrt (eps));

%!test
%! x = [3, 0, -3, 4, 0, -4, 5, 0, -5];
%! y = [-2, -2, -2, 1, 1, 1, 3, 3, 3];
%! z = x .* (2 .^ y);
%! assert (pow2 (x,y), z, sqrt (eps));

%!error pow2 ()
%!error pow2 (1,2,3)

