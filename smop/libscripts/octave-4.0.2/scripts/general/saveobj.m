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
## @deftypefn {Function File} {@var{b} =} saveobj (@var{a})
## Method of a class to manipulate an object prior to saving it to a file.
##
## The function @code{saveobj} is called when the object @var{a} is saved
## using the @code{save} function.  An example of the use of @code{saveobj}
## might be to remove fields of the object that don't make sense to be saved
## or it might be used to ensure that certain fields of the object are
## initialized before the object is saved.  For example:
##
## @example
## @group
## function b = saveobj (a)
##   b = a;
##   if (isempty (b.field))
##      b.field = initfield (b);
##   endif
## endfunction
## @end group
## @end example
##
## @seealso{loadobj, class}
## @end deftypefn

function b = saveobj (a)
  error ('saveobj: not defined for class "%s"', class (a));
endfunction

