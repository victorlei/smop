## Copyright (C) 2012-2015 John W. Eaton
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
## @deftypefn  {Function File} {@var{current_state} =} recycle ()
## @deftypefnx {Function File} {@var{old_state} =} recycle (@var{new_state})
## Query or set the preference for recycling deleted files.
##
## When recycling is enabled, commands which would permanently erase files
## instead move them to a temporary location (such as the directory labeled
## Trash).
##
## Programming Note: This function is provided for @sc{matlab} compatibility,
## but recycling is not implemented in Octave.  To help avoid accidental data
## loss an error will be raised if an attempt is made to enable file recycling.
## @seealso{delete, rmdir}
## @end deftypefn

## Author: jwe

function retval = recycle (state)

  persistent current_state = "off";

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0 || nargout > 0)
    retval = current_state;
  endif

  if (nargin == 1)
    if (! ischar (state))
      error ("recycle: STATE must be a character string");
    endif

    if (strcmpi (state, "on"))
      error ("recycle: recycling files is not implemented");
    elseif (strcmpi (state, "off"))
      current_state = "off";
    else
      error ("recycle: invalid value of STATE = '%s'", state);
    endif
  endif

endfunction


%!test
%! recycle ("off");
%! assert (recycle ("off"), "off");

%!error recycle ("on", "and I mean it")
%!error <STATE must be a character string> recycle (1)
%!error <recycling files is not implemented> recycle ("on")
%!error <invalid value of STATE = 'foobar'> recycle ("foobar")

