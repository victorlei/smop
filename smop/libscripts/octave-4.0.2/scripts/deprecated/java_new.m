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
## @deftypefn  {Loadable Function} {@var{obj} =} java_new (@var{name})
## @deftypefnx {Loadable Function} {@var{obj} =} java_new (@var{name}, @var{arg1}, @dots{})
## Create a Java object of class @var{name}, by calling the class constructor
## with the arguments @var{arg1}, @dots{}
##
## @example
## @group
##   x = java_new ("java.lang.StringBuffer")
##   x = java_new ("java.lang.StringBuffer", "Initial string")
## @end group
## @end example
##
## @seealso{javaObject, javaMethod}
## @end deftypefn

function retval = java_new (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "java_new is obsolete and will be removed from a future version of Octave; please use javaObject instead");
  endif

  if (nargin < 1)
    print_usage ();
  endif

  retval = javaObject (varargin{:});

endfunction

