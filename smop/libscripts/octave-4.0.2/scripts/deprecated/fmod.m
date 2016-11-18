## Copyright (C) 2014-2015 John W. Eaton
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
## @deftypefn {Mapping Function} {} fmod (@var{x}, @var{y})
##
## @code{fmod} is deprecated and will be removed in Octave version 4.4.
## Please use @code{rem} in all new code.
##
## Return the remainder of the division @code{@var{x} / @var{y}}, computed
## using the expression
##
## @example
## x - y .* fix (x ./ y)
## @end example
##
## An error message is printed if the dimensions of the arguments do not
## agree, or if either of the arguments is complex.
## @seealso{rem, mod}
## @end deftypefn

## Deprecated in version 4.0

function retval = fmod (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "fmod is obsolete and will be removed from a future version of Octave, please use rem instead");
  endif

  retval = rem (varargin{:});

endfunction

