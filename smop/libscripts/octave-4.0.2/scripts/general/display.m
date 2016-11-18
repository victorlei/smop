## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn {Function File} {} display (@var{a})
## Display the contents of an object.
##
## If @var{a} is an object of the class @qcode{"myclass"}, then @code{display}
## is called in a case like
##
## @example
## myclass (@dots{})
## @end example
##
## @noindent
## where Octave is required to display the contents of a variable of the
## type @qcode{"myclass"}.
##
## @seealso{class, subsref, subsasgn}
## @end deftypefn

function idx = display (a)

  if (nargin != 1)
    print_usage ();
  endif

  ## Only reason we got here is that there was no overloaded display()
  ## function for object a.  This may mean it is a built-in.
  str = disp (a);
  if (isempty (strfind (str, "<class ")))
    disp (str);
  else
    error ('display: not defined for class "%s"', class (a));
  endif

endfunction

