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
## @deftypefn {Function File} {} dec2bin (@var{d}, @var{len})
## Return a binary number corresponding to the non-negative integer @var{d},
## as a string of ones and zeros.
##
## For example:
##
## @example
## @group
## dec2bin (14)
##      @result{} "1110"
## @end group
## @end example
##
## If @var{d} is a matrix or cell array, return a string matrix with one row
## per element in @var{d}, padded with leading zeros to the width of the
## largest value.
##
## The optional second argument, @var{len}, specifies the minimum number of
## digits in the result.
## @seealso{bin2dec, dec2base, dec2hex}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function b = dec2bin (d, len)

  if (nargin == 1)
    b = dec2base (d, 2);
  elseif (nargin == 2)
    b = dec2base (d, 2, len);
  else
    print_usage ();
  endif

endfunction


%!assert (dec2bin (14), "1110")
%!assert (dec2bin (14, 6), "001110")
%!assert (dec2bin ({1, 2; 3, 4}), ["001"; "011"; "010"; "100"])

## Test input validation
%!error dec2bin ()
%!error dec2bin (1, 2, 3)

