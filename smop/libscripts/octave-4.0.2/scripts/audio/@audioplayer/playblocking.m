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
## @deftypefn  {Function File} {} playblocking (@var{player})
## @deftypefnx {Function File} {} playblocking (@var{player}, @var{start})
## @deftypefnx {Function File} {} playblocking (@var{player}, @var{limits})
## Play audio stored in the audioplayer object @var{player} with blocking.
##
## Given optional argument start, begin playing at @var{start} seconds in the
## recording.  Given a two-element vector @var{limits}, begin and end playing
## at the number of seconds specified by the elements of the vector.
## @end deftypefn

function playblocking (varargin)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  __player_playblocking__ (struct (varargin{1}).player, varargin{2:end});

endfunction
