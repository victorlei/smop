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
## @deftypefn  {Built-in Function} {@var{ret} =} java_invoke (@var{obj}, @var{methodname})
## @deftypefnx {Built-in Function} {@var{ret} =} java_invoke (@var{obj}, @var{methodname}, @var{arg1}, @dots{})
## Invoke the method @var{methodname} on the Java object @var{obj} with the
## arguments @var{arg1}, @dots{}  For static methods, @var{obj} can be a
## string representing the fully qualified name of the corresponding class.
## The function returns the result of the method invocation.
##
## When @var{obj} is a regular Java object, structure-like indexing can be
## used as a shortcut syntax.  For instance, the two following statements are
## equivalent
##
## @example
## @group
##   ret = java_invoke (x, "method1", 1.0, "a string")
##   ret = x.method1 (1.0, "a string")
## @end group
## @end example
##
## @seealso{javaMethod, javaObject}
## @end deftypefn

function retval = java_invoke (obj, methodname, varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "java_invoke is obsolete and will be removed from a future version of Octave, please use javaMethod instead");
  endif

  if (nargin < 2)
    print_usage ();
  endif

  retval = javaMethod (methodname, obj, varargin{:});

endfunction

