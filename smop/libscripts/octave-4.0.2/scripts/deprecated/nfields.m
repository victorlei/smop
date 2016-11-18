## Copyright (C) 2014-2015 Rik Wehbring
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
## @deftypefn {Function File} {} nfields (@var{s})
## Return the number of fields of the structure @var{s}.
##
## @strong{Warning:} @code{nfields} is scheduled for removal in version 4.4.
## Use @code{numfields} instead.
## @seealso{numfields, fieldnames}
## @end deftypefn

## Deprecated in 4.0

function retval = nfields (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "nfields is obsolete and will be removed from a future version of Octave; please use numfields instead");
  endif

  if (nargin < 1)
    print_usage ();
  endif

  retval = numfields (varargin{:});

endfunction

