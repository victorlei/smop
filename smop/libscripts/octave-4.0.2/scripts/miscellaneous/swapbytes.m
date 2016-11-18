## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn {Function File} {} swapbytes (@var{x})
## Swap the byte order on values, converting from little endian to big endian
## and vice versa.
##
## For example:
##
## @example
## @group
## swapbytes (uint16 (1:4))
## @result{} [   256   512   768  1024]
## @end group
## @end example
##
## @seealso{typecast, cast}
## @end deftypefn

function y = swapbytes (x)

  if (nargin != 1)
    print_usage ();
  endif

  cls = class (x);
  if (strcmp (cls, "int8") || strcmp (cls, "uint8") || isempty (x))
    y = x;
  else
    if (strcmp (cls, "int16") || strcmp (cls, "uint16"))
      nb = 2;
    elseif (strcmp (cls, "single")
            || strcmp (cls, "int32") || strcmp (cls, "uint32"))
      nb = 4;
    elseif (strcmp (cls, "double")
            || strcmp (cls, "int64") || strcmp (cls, "uint64"))
      nb = 8;
    else
      error ("swapbytes: invalid object of class '%s'", cls);
    endif
    y = reshape (typecast (reshape (typecast (x(:), "uint8"), nb, numel (x))
                           ([nb : -1 : 1], :) (:), cls), size (x));
  endif

endfunction


%!assert (swapbytes (uint16 (1:4)), uint16 ([256 512 768 1024]))
%!test
%! assert (swapbytes (swapbytes (pi)), pi);
%! assert (swapbytes (swapbytes (single (pi))), single (pi));

## Test input validation
%!error swapbytes ()
%!error swapbytes (1, 2)
%!error <invalid object of class 'cell'> swapbytes ({1})

