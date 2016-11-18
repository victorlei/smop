## Copyright (C) 2014-2015 John W. Eaton
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
## @deftypefn {Built-in Function} {} fnmatch (@var{pattern}, @var{string})
##
## @code{fnmatch} is deprecated and will be removed in Octave version 4.4.
## Please use @code{glob} or @code{regexp} in all new code.
##
## Return true or false for each element of @var{string} that matches any of
## the elements of the string array @var{pattern}, using the rules of
## filename pattern matching.  For example:
##
## @example
## @group
## fnmatch (\"a*b\", @{\"ab\"; \"axyzb\"; \"xyzab\"@})
##      @result{} [ 1; 1; 0 ]
## @end group
## @end example
## @seealso{glob, regexp}
## @end deftypefn

## Deprecated in version 4.0

function retval = fnmatch (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "fnmatch is obsolete and will be removed from a future version of Octave, please use glob or regexp instead");
  endif

  retval = __fnmatch__ (varargin{:});

endfunction

