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
## @deftypefn {Function File} {@var{b} =} loadobj (@var{a})
## Method of a class to manipulate an object after loading it from a file.
##
## The function @code{loadobj} is called when the object @var{a} is loaded
## using the @code{load} function.  An example of the use of @code{saveobj}
## might be to add fields to an object that don't make sense to be saved.
## For example:
##
## @example
## @group
## function b = loadobj (a)
##   b = a;
##   b.addmissingfield = addfield (b);
## endfunction
## @end group
## @end example
##
## @seealso{saveobj, class}
## @end deftypefn

function b = loadobj (a)
  error ('loadobj: not defined for class "%s"', class (a));
endfunction

