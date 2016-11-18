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
## @deftypefn  {Function File} {@var{z} =} xor (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{z} =} xor (@var{x1}, @var{x2}, @dots{})
## Return the @dfn{exclusive or} of @var{x} and @var{y}.
##
## For boolean expressions @var{x} and @var{y},
## @code{xor (@var{x}, @var{y})} is true if and only if one of @var{x} or
## @var{y} is true.  Otherwise, if @var{x} and @var{y} are both true or both
## false, @code{xor} returns false.
##
## The truth table for the xor operation is
##
## @multitable @columnfractions 0.44 .03 .05 .03 0.44
## @item @tab @var{x} @tab @var{y} @tab @var{z} @tab
## @item @tab - @tab - @tab - @tab
## @item @tab 0 @tab 0 @tab 0 @tab
## @item @tab 1 @tab 0 @tab 1 @tab
## @item @tab 0 @tab 1 @tab 1 @tab
## @item @tab 1 @tab 1 @tab 0 @tab
## @end multitable
##
## If more than two arguments are given the xor operation is applied
## cumulatively from left to right:
##
## @example
## (@dots{}((x1 XOR x2) XOR x3) XOR @dots{})
## @end example
##
## @seealso{and, or, not}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 16 September 1994
## Adapted-By: jwe

function z = xor (x, y, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  z = __xor__ (x, y);

  ## Slow expansion to multiple arguments.
  ## Probably okay number of elements ## will be small.
  if (! isempty (varargin))
    for i = 1:numel (varargin)
      z = __xor__ (z, varargin{i});
    endfor
  endif

endfunction

function z = __xor__ (x, y)

  if (isscalar (x) || isscalar (y) || size_equal (x, y))
    ## Typecast to logicals is necessary for other numeric types.
    z = logical (x) != logical (y);
  else
    try
      z = bsxfun (@xor, x, y);
    catch
      error ("xor: X and Y must be of compatible size or scalars");
    end_try_catch
  endif

endfunction


%!assert (xor ([1, 1, 0, 0], [0, 1, 0, 1]), logical ([1, 0, 0, 1]))
%!assert (xor ([i, i, 0, 0], [1, 0, 1, 0]), logical ([0, 1, 1, 0]))

%!assert (xor (eye (2), fliplr (eye (2))), logical (ones (2)))
%!assert (xor (speye (2), fliplr (speye (2))), sparse (logical (ones (2))))

## Test XOR reduction
%!assert (xor ([1 0], [1 1], [0 0]), logical ([0 1]))

## Test input validation
%!error xor ()
%!error xor (1)
%!error <X and Y must be of compatible size> xor (ones (3,2), ones (2,3))

