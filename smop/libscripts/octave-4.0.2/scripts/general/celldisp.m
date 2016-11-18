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
## @deftypefn  {Function File} {} celldisp (@var{c})
## @deftypefnx {Function File} {} celldisp (@var{c}, @var{name})
## Recursively display the contents of a cell array.
##
## By default the values are displayed with the name of the variable @var{c}.
## However, this name can be replaced with the variable @var{name}.  For
## example:
##
## @example
## @group
## c = @{1, 2, @{31, 32@}@};
## celldisp (c, "b")
##    @result{}
##       b@{1@} =
##        1
##       b@{2@} =
##        2
##       b@{3@}@{1@} =
##        31
##       b@{3@}@{2@} =
##        32
## @end group
## @end example
##
## @seealso{disp}
## @end deftypefn

## This is ugly, but seems to be what matlab does..

function celldisp (c, name)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! iscell (c))
    error ("celldisp: C must be a cell array");
  endif

  if (nargin == 1)
    name = inputname (1);
  endif

  for i = 1: numel (c)
    if (iscell (c{i}))
      celldisp (c{i}, sprintf ("%s{%s}", name, indices (size (c), i)));
    else
      disp (sprintf ("%s{%s} = \n", name, indices (size (c), i)));
      disp (c{i});
      disp ("");
    endif
  endfor
endfunction

function s = indices (dv, i)
  if (sum (dv != 1) > 1)
    c = cell (size (dv));
    [c{:}] = ind2sub (dv, i);
    s = sprintf ("%i,", c{:});
    s(end) = [];
  else
    s = sprintf ("%i", i);
  endif
endfunction


%!demo
%! c = {1, 2, {31, 32}};
%! celldisp (c, "b")

## Test input validation
%!error celldisp ()
%!error celldisp ({}, "name", 1)
%!error <C must be a cell array> celldisp (1)

