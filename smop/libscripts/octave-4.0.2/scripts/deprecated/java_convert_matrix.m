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
## @deftypefn  {Built-in Function} {@var{val} =} java_convert_matrix ()
## @deftypefnx {Built-in Function} {@var{old_val} =} java_convert_matrix (@var{new_val})
## @deftypefnx {Built-in Function} {} java_convert_matrix (@var{new_val}, "local")
## Query or set the internal variable that controls whether Java arrays are
## automatically converted to Octave matrices.  The default value is false.
##
## When called from inside a function with the @qcode{"local"} option, the
## variable is changed locally for the function and any subroutines it calls.
##  The original variable value is restored when exiting the function.
## @seealso{java_matrix_autoconversion, java_unsigned_conversion, java_debug}
## @end deftypefn

function old_val = java_convert_matrix (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "java_convert_matrix is obsolete and will be removed from a future version of Octave; use java_matrix_autoconversion instead");
  endif

  if (nargin > 2)
    print_usage ();
  endif

  old_val = java_matrix_autoconversion (varargin{:});

endfunction

