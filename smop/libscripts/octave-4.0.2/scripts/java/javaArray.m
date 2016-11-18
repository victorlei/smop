## Copyright (C) 2007, 2013 Michael Goffioul
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
## @deftypefn  {Function File} {@var{jary} =} javaArray (@var{classname}, @var{sz})
## @deftypefnx {Function File} {@var{jary} =} javaArray (@var{classname}, @var{m}, @var{n}, @dots{})
##
## Create a Java array of size @var{sz} with elements of class @var{classname}.
##
## @var{classname} may be a Java object representing a class or a string
## containing the fully qualified class name.  The size of the object may
## also be specified with individual integer arguments @var{m}, @var{n}, etc.
##
## The generated array is uninitialized.  All elements are set to null if
## @var{classname} is a reference type, or to a default value (usually 0) if
## @var{classname} is a primitive type.
##
## Sample code:
##
## @example
## @group
## jary = javaArray ("java.lang.String", 2, 2);
## jary(1,1) = "Hello";
## @end group
## @end example
## @seealso{javaObject}
## @end deftypefn

function retval = javaArray (classname, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  retval = javaMethod ("createArray", "org.octave.ClassHelper",
                        classname, [varargin{:}]);

endfunction

