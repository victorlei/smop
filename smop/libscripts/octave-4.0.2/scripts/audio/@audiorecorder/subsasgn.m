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
## @deftypefn {Function File} {@var{value} =} subsasgn (@var{recorder}, @var{idx}, @var{rhs})
## Perform subscripted assignment on the audio recorder object @var{recorder}.
##
## Assign the value of @var{rhs} to the recorder property named by @var{idx}.
## @end deftypefn

function value = subsasgn (recorder, idx, rhs)
  if (nargin != 3)
    print_usage ();
  endif

  if (isempty (idx))
    error ("@audiorecorder/subsasgn: missing index");
  endif

  if (strcmp (idx(1).type, "."))
    field = idx.subs;
    set (recorder, field, rhs);
    value = recorder;
  else
    error ("@audiorecorder/subsasgn: invalid subscript type");
  endif

endfunction
