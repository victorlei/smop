## Copyright (C) 2013-2015 Vytautas Jančauskas
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
## @deftypefn {Function File} {@var{value} =} subsref (@var{recorder}, @var{idx})
## Perform subscripted selection on the audio recorder object @var{recorder}.
##
## Return the recorder property value named by @var{idx}.
## @end deftypefn

function value = subsref (recorder, idx)

  if (nargin != 2)
    print_usage ();
  endif

  if (isempty (idx))
    error ("@audiorecorder/subsref: missing index");
  endif

  if (strcmp (idx(1).type, "."))
    field = idx.subs;
    value = get (recorder, field);
  else
    error ("@audiorecorder/subsref: invalid subscript type")
  endif

endfunction
