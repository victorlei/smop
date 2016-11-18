## Copyright (C) 2005-2015 Paul Kienzle
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
## @deftypefn  {Function File} {count =} runlength (@var{x})
## @deftypefnx {Function File} {[count, value] =} runlength (@var{x})
## Find the lengths of all sequences of common values.
##
## @var{count} is a vector with the lengths of each repeated value.
##
## The optional output @var{value} contains the value that was repeated in
## the sequence.
##
## @example
## @group
## runlength ([2, 2, 0, 4, 4, 4, 0, 1, 1, 1, 1])
## @result{}  [2, 1, 3, 1, 4]
## @end group
## @end example
## @seealso{run_count}
## @end deftypefn

function [count, value] = runlength (x)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)) || ! isvector (x))
    error ("runlength: X must be a numeric vector");
  endif

  if (iscolumn (x))
    x = x.';
  endif

  idx = [find(x(1:end-1) != x(2:end)), length(x)];
  count = diff ([0 idx]);
  if (nargout == 2)
    value = x(idx);
  endif

endfunction


%!assert (runlength ([2 2 0 4 4 4 0 1 1 1 1]), [2 1 3 1 4])
%!assert (runlength ([2 2 0 4 4 4 0 1 1 1 1]'), [2 1 3 1 4])
%!test
%! [c, v] = runlength ([2 2 0 4 4 4 0 1 1 1 1]);
%! assert (c, [2 1 3 1 4]);
%! assert (v, [2 0 4 0 1]);

## Test input validation
%!error runlength ()
%!error runlength (1, 2)
%!error runlength (['A'; 'B'])
%!error runlength (ones (2,2))

