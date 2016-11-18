## Copyright (C) 2012-2015 Rik Wehbring
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
## @deftypefn {Function File} {@var{obj} =} java_set (@var{obj}, @var{name}, @var{val})
## Set the value of the field @var{name} of the Java object @var{obj} to
## @var{val}.
##
## For static fields, @var{obj} can be a string representing the fully
## qualified named of the corresponding Java class.
##
## When @var{obj} is a regular Java object, structure-like indexing can be
## used as a shortcut syntax.  For instance, the following two statements are
## equivalent
##
## @example
## @group
##   java_set (x, "field1", val)
##   x.field1 = val
## @end group
## @end example
##
## @seealso{java_get, javaMethod, javaObject}
## @end deftypefn

function retval = java_set (obj, name, val)

  if (nargin != 3)
    print_usage ();
  endif

  if (isjava (obj))
    obj.(name) = val;
  elseif (ischar (obj))
    retval = __java_set__ (obj, name, val);
  else
    error ("java_set: OBJ must be a Java object");
  endif

endfunction

