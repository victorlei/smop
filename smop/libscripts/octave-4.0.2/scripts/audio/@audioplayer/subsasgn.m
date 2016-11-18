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
## @deftypefn {Function File} {@var{value} =} subsasgn (@var{player}, @var{idx}, @var{rhs})
## Perform subscripted assignment on the audio player object @var{player}.
##
## Assign the value of @var{rhs} to the player property named by @var{idx}.
## @end deftypefn

function value = subsasgn (player, idx, rhs)

  if (isempty (idx))
    error ("audioplayer: missing index");
  endif

  if (strcmp (idx(1).type, "."))
    field = idx.subs;
    set (player, field, rhs);
    value = player;
  else
    error ("@audioplayer/subsasgn: invalid subscript type");
  endif

endfunction
