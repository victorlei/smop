## Copyright (C) 1996-2015 Daniel Calvelo
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
## @deftypefn {Function File} {} bin2dec (@var{s})
## Return the decimal number corresponding to the binary number represented
## by the string @var{s}.
##
## For example:
##
## @example
## @group
## bin2dec ("1110")
##      @result{} 14
## @end group
## @end example
##
## Spaces are ignored during conversion and may be used to make the binary
## number more readable.
##
## @example
## @group
## bin2dec ("1000 0001")
##      @result{} 129
## @end group
## @end example
##
## If @var{s} is a string matrix, return a column vector with one converted
## number per row of @var{s}; Invalid rows evaluate to NaN@.
##
## If @var{s} is a cell array of strings, return a column vector with one
## converted number per cell element in @var{s}.
## @seealso{dec2bin, base2dec, hex2dec}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function d = bin2dec (s)

  if (nargin != 1)
    print_usage ();
  endif

  d = base2dec (s, 2);

endfunction


%!assert (bin2dec ("0000"), 0)
%!assert (bin2dec ("1110"), 14)
%!assert (bin2dec ("11111111111111111111111111111111111111111111111111111"), 2^53-1)
%!assert (bin2dec ({"1110", "1111"}), [14; 15])
%!assert (bin2dec ("1 0 1"), 5)
%!assert (bin2dec (char ("1 0 1", "   1111")), [5; 15])

## Test input validation
%!error bin2dec ()
%!error bin2dec (1)
%!error bin2dec ("1", 2)

