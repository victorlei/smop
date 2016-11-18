## Copyright (C) 1996-2015 Kurt Hornik
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
## @deftypefn {Function File} {} deblank (@var{s})
## Remove trailing whitespace and nulls from @var{s}.
##
## If @var{s} is a matrix, @var{deblank} trims each row to the length of
## longest string.  If @var{s} is a cell array of strings, operate
## recursively on each string element.
##
## Examples:
##
## @example
## @group
## deblank ("    abc  ")
##      @result{}  "    abc"
##
## deblank ([" abc   "; "   def   "])
##      @result{}  [" abc  " ; "   def"]
## @end group
## @end example
## @seealso{strtrim}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

function s = deblank (s)

  if (nargin != 1)
    print_usage ();
  endif

  if (ischar (s))

    k = find (! isspace (s) & s != "\0");
    if (isempty (s) || isempty (k))
      s = "";
    else
      s = s(:,1:ceil (max (k) / rows (s)));
    endif

  elseif (iscell (s))

    char_idx = cellfun ("isclass", s, "char");
    cell_idx = cellfun ("isclass", s, "cell");
    if (! all (char_idx | cell_idx))
      error ("deblank: S argument must be a string or cellstring");
    endif

    ## Divide work load.  Recursive cellfun deblank call is slow
    ## and avoided where possible.
    s(char_idx) = regexprep (s(char_idx), "[\\s\v\\0]+$", '');
    s(cell_idx) = cellfun ("deblank", s(cell_idx), "UniformOutput", false);

  else
    error ("deblank: S argument must be a string or cellstring");
  endif

endfunction


%!assert (deblank (" f o o \0"), " f o o")
%!assert (deblank ('   '), '')
%!assert (deblank ("   "), "")
%!assert (deblank (""), "")
%!assert (deblank ({}), {})
%!assert (deblank ({" abc   ", {"   def   "}}), {" abc", {"   def"}})

%!error <Invalid call to deblank> deblank ()
%!error <Invalid call to deblank> deblank ("foo", "bar")
%!error <argument must be a string> deblank (1)
%!error <argument must be a string> deblank ({[]})

