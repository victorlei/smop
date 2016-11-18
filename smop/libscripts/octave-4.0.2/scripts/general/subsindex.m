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
## @deftypefn {Function File} {@var{idx} =} subsindex (@var{a})
## Convert an object to an index vector.
##
## When @var{a} is a class object defined with a class constructor, then
## @code{subsindex} is the overloading method that allows the conversion of
## this class object to a valid indexing vector.  It is important to note that
## @code{subsindex} must return a zero-based real integer vector of the class
## @qcode{"double"}.  For example, if the class constructor
##
## @example
## @group
## function b = myclass (a)
##   b = class (struct ("a", a), "myclass");
## endfunction
## @end group
## @end example
##
## @noindent
## then the @code{subsindex} function
##
## @example
## @group
## function idx = subsindex (a)
##   idx = double (a.a) - 1.0;
## endfunction
## @end group
## @end example
##
## @noindent
## can then be used as follows
##
## @example
## @group
## a = myclass (1:4);
## b = 1:10;
## b(a)
## @result{} 1  2  3  4
## @end group
## @end example
##
## @seealso{class, subsref, subsasgn}
## @end deftypefn

function idx = subsindex (a)
  error ("subsindex: not defined for class \"%s\"", class (a));
endfunction

