## Copyright (C) 2000-2015 Daniel Calvelo
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
## @deftypefn  {Function File} {} dec2base (@var{d}, @var{base})
## @deftypefnx {Function File} {} dec2base (@var{d}, @var{base}, @var{len})
## Return a string of symbols in base @var{base} corresponding to the
## non-negative integer @var{d}.
##
## @example
## @group
## dec2base (123, 3)
##    @result{} "11120"
## @end group
## @end example
##
## If @var{d} is a matrix or cell array, return a string matrix with one row
## per element in @var{d}, padded with leading zeros to the width of the
## largest value.
##
## If @var{base} is a string then the characters of @var{base} are used as
## the symbols for the digits of @var{d}.  Space (' ') may not be used as a
## symbol.
##
## @example
## @group
## dec2base (123, "aei")
##    @result{} "eeeia"
## @end group
## @end example
##
## The optional third argument, @var{len}, specifies the minimum number of
## digits in the result.
## @seealso{base2dec, dec2bin, dec2hex}
## @end deftypefn

## Author: Daniel Calvelo <dcalvelo@yahoo.com>
## Adapted-by: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>

function retval = dec2base (d, base, len)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (iscell (d))
    d = cell2mat (d);
  endif

  ## Create column vector for algorithm
  if (! iscolumn (d))
    d = d(:);
  endif

  ## Treat logical as numeric for compatibility with ML
  if (islogical (d))
    d = double (d);
  elseif (! isnumeric (d) || iscomplex (d) || any (d < 0 | d != fix (d)))
    error ("dec2base: input must be real non-negative integers");
  endif

  symbols = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  if (ischar (base))
    symbols = base;
    base = length (symbols);
    if (length (unique (symbols)) != base)
      error ("dec2base: symbols representing digits must be unique");
    endif
    if (any (isspace (symbols)))
      error ("dec2base: whitespace characters are not valid symbols");
    endif
  elseif (! isscalar (base))
    error ("dec2base: cannot convert from several bases at once");
  elseif (base < 2 || base > length (symbols))
    error ("dec2base: BASE must be between 2 and 36, or a string of symbols");
  endif

  ## determine number of digits required to handle all numbers, can overflow
  ## by 1 digit
  max_len = round (log (max (max (d(:)), 1)) / log (base)) + 1;

  if (nargin == 3)
    max_len = max (max_len, len);
  endif

  ## determine digits for each number
  digits = zeros (length (d), max_len);
  for k = max_len:-1:1
    digits(:,k) = mod (d, base);
    d = round ((d - digits(:,k)) / base);
  endfor

  ## convert digits to symbols
  retval = reshape (symbols(digits+1), size (digits));

  ## Check if the first element is the zero symbol. It seems possible
  ## that LEN is provided, and is less than the computed MAX_LEN and
  ## MAX_LEN is computed to be one larger than necessary, so we would
  ## have a leading zero to remove.  But if LEN >= MAX_LEN, we should
  ## not remove any leading zeros.
  if ((nargin == 2 || (nargin == 3 && max_len > len))
      && length (retval) != 1 && ! any (retval(:,1) != symbols(1)))
    retval = retval(:,2:end);
  endif

endfunction


%!test
%! s0 = "";
%! for n = 1:13
%!   for b = 2:16
%!     pp = dec2base (b^n+1, b);
%!     assert (dec2base (b^n, b), ['1',s0,'0']);
%!     assert (dec2base (b^n+1, b), ['1',s0,'1']);
%!   endfor
%!   s0 = [s0,'0'];
%! endfor

%!test
%! digits = "0123456789ABCDEF";
%! for n = 1:13
%!   for b = 2:16
%!     pm = dec2base (b^n-1, b);
%!     assert (length (pm), n);
%!     assert (all (pm == digits(b)));
%!   endfor
%! endfor

%!test
%! for b = 2:16
%!   assert (dec2base (0, b), '0');
%! endfor

%!assert (dec2base (0, 2, 4), "0000")
%!assert (dec2base (2^51-1, 2), ...
%!        "111111111111111111111111111111111111111111111111111")
%!assert (dec2base (uint64 (2)^63-1, 16), "7FFFFFFFFFFFFFFF")
%!assert (dec2base ([1, 2; 3, 4], 2, 3), ["001"; "011"; "010"; "100"])
%!assert (dec2base ({1, 2; 3, 4}, 2, 3), ["001"; "011"; "010"; "100"])

%!test
%! a = 0:3;
%! assert (dec2base (!a, 2, 1), ["1"; "0"; "0"; "0"])

## Test input validation
%!error dec2base ()
%!error dec2base (1)
%!error dec2base (1, 2, 3, 4)
%!error dec2base ("A")
%!error dec2base (2i)
%!error dec2base (-1)
%!error dec2base (1.1)
%!error dec2base (1, "ABA")
%!error dec2base (1, "A B")
%!error dec2base (1, ones (2))
%!error dec2base (1, 1)
%!error dec2base (1, 37)

