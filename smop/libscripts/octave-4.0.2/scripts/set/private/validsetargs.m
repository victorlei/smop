## Copyright (C) 2000-2015 Paul Kienzle
## Copyright (C) 2009-2010 Jaroslav Hajek
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

## Validate arguments for binary set operation.

function [x, y] = validsetargs (caller, x, y, byrows_arg)

  isallowedarraytype = @(x) isnumeric (x) || ischar (x) || islogical (x);

  if (nargin == 3)
    icx = iscellstr (x);
    icy = iscellstr (y);
    if (icx || icy)
      if (icx && ischar (y))
        y = cellstr (y);
      elseif (icy && ischar (x))
        x = cellstr (x);
      elseif (! (icx && icy))
        error ("%s: cell array of strings cannot be combined with a nonstring value", caller);
      endif
    elseif (! (isallowedarraytype (x) && isallowedarraytype (y)))
      error ("%s: A and B must be arrays or cell arrays of strings", caller);
    endif
  elseif (nargin == 4)
    if (! strcmpi (byrows_arg, "rows"))
      error ("%s: invalid option: %s", caller, byrows_arg);
    endif

    if (iscell (x) || iscell (y))
      error ('%s: cells not supported with "rows"', caller);
    elseif (! (isallowedarraytype (x) && isallowedarraytype (y)))
      error ("%s: A and B must be arrays or cell arrays of strings", caller);
    else
      if (ndims (x) > 2 || ndims (y) > 2)
        error ('%s: A and B must be 2-dimensional matrices for "rows"', caller);
      elseif (columns (x) != columns (y) && ! (isempty (x) || isempty (y)))
        error ("%s: number of columns in A and B must match", caller);
      endif
    endif
  endif

endfunction


## %!tests for function are in union.m
