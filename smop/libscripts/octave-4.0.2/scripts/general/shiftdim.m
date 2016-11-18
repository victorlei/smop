## Copyright (C) 2004-2015 John Eaton and David Bateman
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
## @deftypefn  {Function File} {@var{y} =} shiftdim (@var{x}, @var{n})
## @deftypefnx {Function File} {[@var{y}, @var{ns}] =} shiftdim (@var{x})
## Shift the dimensions of @var{x} by @var{n}, where @var{n} must be
## an integer scalar.
##
## When @var{n} is positive, the dimensions of @var{x} are shifted to the left,
## with the leading dimensions circulated to the end.  If @var{n} is negative,
## then the dimensions of @var{x} are shifted to the right, with @var{n}
## leading singleton dimensions added.
##
## Called with a single argument, @code{shiftdim}, removes the leading
## singleton dimensions, returning the number of dimensions removed in the
## second output argument @var{ns}.
##
## For example:
##
## @example
## @group
## x = ones (1, 2, 3);
## size (shiftdim (x, -1))
##    @result{} [1, 1, 2, 3]
## size (shiftdim (x, 1))
##    @result{} [2, 3]
## [b, ns] = shiftdim (x)
##    @result{} b = [1, 1, 1; 1, 1, 1]
##    @result{} ns = 1
## @end group
## @end example
## @seealso{reshape, permute, ipermute, circshift, squeeze}
## @end deftypefn

function [y, ns]  = shiftdim (x, n)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  nd = ndims (x);
  orig_dims = size (x);

  if (nargin == 1)
    ## Find the first non-singleton dimension.
    (n = find (orig_dims != 1, 1) - 1) || (n = nd);
  elseif (! (isscalar (n) && n == fix (n)))
    error ("shiftdim: N must be a scalar integer");
  endif

  if (n >= nd)
    n = rem (n, nd);
  endif

  if (n < 0)
    singleton_dims = ones (1, -n);
    y = reshape (x, [singleton_dims, orig_dims]);
  elseif (n > 0)
    ## We need permute here instead of reshape to shift values in a
    ## compatible way.
    y = permute (x, [n+1:nd 1:n]);
  else
    y = x;
  endif

  ns = n;

endfunction


%!test
%! x = rand (1, 1, 4, 2);
%! [y, ns] = shiftdim (x);
%! assert (size (y), [4 2]);
%! assert (ns, 2);
%! assert (shiftdim (y, -2), x);
%! assert (size (shiftdim (x, 2)), [4 2]);

%!assert (size (shiftdim (rand (0, 1, 2))), [0 1 2])

## Test input validation
%!error (shiftdim ())
%!error (shiftdim (1,2,3))
%!error (shiftdim (1, ones (2)))
%!error (shiftdim (1, 1.5))

