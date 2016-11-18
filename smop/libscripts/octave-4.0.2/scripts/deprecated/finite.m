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
## @deftypefn {Mapping Function} {} finite (@var{x})
##
## @code{finite} is deprecated and will be removed in Octave version 4.4.
## Please use @code{isfinite} in all new code.
##
## Return a logical array which is true where the elements of @var{x} are
## finite values and false where they are not.
## For example:
##
## @example
## @group
## finite ([13, Inf, NA, NaN])
##      @result{} [ 1, 0, 0, 0 ]
## @end group
## @end example
## @seealso{isfinite, isinf, isnan, isna}
## @end deftypefn

## Deprecated in version 4.0

function retval = finite (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "finite is obsolete and will be removed from a future version of Octave, please use isfinite instead");
  endif

  retval = isfinite (varargin{:});

endfunction

