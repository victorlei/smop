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
## @deftypefn  {Function File} {} javafields (@var{javaobj})
## @deftypefnx {Function File} {} javafields ("@var{classname}")
## @deftypefnx {Function File} {@var{fld_names} =} javafields (@dots{})
## Return the fields of a Java object or Java class in the form of a cell
## array of strings.  If no output is requested, print the result
## to the standard output.
## @seealso{fieldnames, methods, javaObject}
## @end deftypefn

function fld_names = javafields (javaobj)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "javafields is obsolete and will be removed from a future version of Octave, please use fieldnames instead");
  endif

  if (nargin != 1)
    print_usage ();
  endif

  c_methods = javaMethod ("getFields", "org.octave.ClassHelper", javaobj);
  method_list = ostrsplit (c_methods, ';');

  if (nargout == 0)
    if (! isempty (method_list))
      disp (method_list);
    endif
  else
    fld_names = cellstr (method_list);
  endif

endfunction

