## Copyright (C) 2004-2015 David Bateman
## Copyright (C) 2012 Jordi Gutiérrez Hermoso
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
## @deftypefn  {Function File} {@var{C} =} bitset (@var{A}, @var{n})
## @deftypefnx {Function File} {@var{C} =} bitset (@var{A}, @var{n}, @var{val})
## Set or reset bit(s) @var{n} of the unsigned integers in @var{A}.
##
## @var{val} = 0 resets and @var{val} = 1 sets the bits.
## The least significant bit is @var{n} = 1.  All variables must be the same
## size or scalars.
##
## @example
## @group
## dec2bin (bitset (10, 1))
##   @result{} 1011
## @end group
## @end example
## @seealso{bitand, bitor, bitxor, bitget, bitcmp, bitshift, bitmax}
## @end deftypefn

function C = bitset (A, n, val)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (any (A(:) < 0))
    error ("bitset: A must be >= 0");
  endif

  sz = size (A);

  if (nargin == 2)
    val = true (sz);
  endif

  cl = class (A);

  if (isfloat (A) && isreal (A))
    Bmax = bitmax (cl);
    Amax = ceil (log2 (Bmax));
  elseif (isinteger (A))
    Bmax = intmax (cl);
    Amax = ceil (log2 (Bmax));
  else
    error ("bitset: invalid class %s", cl);
  endif

  if (any ((n < 1)(:)) || any ((n > Amax)(:)))
    error ("bitset: N must be in the range [1,%d]", Amax);
  endif

  mask = bitshift (cast (1, cl), uint8 (n) - uint8 (1));

  on = logical (val);
  off = !on;

  if (isscalar (mask))
    onmask = mask;
    offmask = mask;
  else
    if (! size_equal (A, n))
      error ("bitset: N must be scalar or the same size as A");
    endif
    onmask = mask(on);
    offmask = mask(off);
  endif

  C = zeros (sz, cl);
  C(on) = bitor (A(on), onmask);
  C(off) = bitand (A(off), bitcmp (offmask));

endfunction


%!test
%! assert (bitset ([0, 10], [3, 3]), [4, 14]);
%! assert (bitset (single ([0, 10]), [3, 3]), single ([4, 14]));
%! pfx = {"", "u"};
%! for i = 1:2
%!   for prec = [8, 16, 32, 64]
%!     fcn = str2func (sprintf ("%sint%d", pfx{i}, prec));
%!     assert (bitset (fcn ([0, 10]), [3, 3]), fcn ([4, 14]));
%!   endfor
%! endfor

## Bug #36458
%!assert (bitset (uint8 ([1, 2;3 4]), 1, [0 1; 0 1]), uint8 ([0, 3; 2 5]))

%!error bitset (1)
%!error bitset (1, 2, 3, 4)
%!error <A must be .= 0> bitset (-1, 2)
%!error <invalid class char> bitset ("1", 2)
%!error <N must be in the range \[1,53\]> bitset (0, 0)
%!error <N must be in the range \[1,53\]> bitset (0, 55)
%!error <N must be in the range \[1,24\]> bitset (single (0), 0)
%!error <N must be in the range \[1,24\]> bitset (single (0), 26)
%!error <N must be in the range \[1,8\]> bitset (uint8 (0), 0)
%!error <N must be in the range \[1,8\]> bitset (uint8 (0), 9)
%!error <N must be in the range \[1,7\]> bitset (int8 (0), 9)
%!error <N must be in the range \[1,15\]> bitset (int16 (0), 17)
%!error <N must be in the range \[1,16\]> bitset (uint16 (0), 17)
%!error <N must be in the range \[1,31\]> bitset (int32 (0), 33)
%!error <N must be in the range \[1,32\]> bitset (uint32 (0), 33)
%!error <N must be in the range \[1,63\]> bitset (int64 (0), 65)
%!error <N must be in the range \[1,64\]> bitset (uint64 (0), 65)
%!error <N must be scalar or the same size as A> bitset (uint8 (1), [1 3])
%!error <N must be scalar or the same size as A> bitset (uint8 (1:3), [1 3])

