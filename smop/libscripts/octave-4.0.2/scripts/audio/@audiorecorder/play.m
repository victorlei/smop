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
## @deftypefn  {Function File} {@var{player} =} play (@var{recorder})
## @deftypefnx {Function File} {@var{player} =} play (@var{recorder}, @var{start})
## @deftypefnx {Function File} {@var{player} =} play (@var{recorder}, [@var{start}, @var{end}])
## Play the audio recorded in @var{recorder} and return a corresponding
## audioplayer object.
##
## If the optional argument @var{start} is provided, begin playing
## @var{start} seconds in to the recording.
##
## If the optional argument @var{end} is provided, stop playing at
## @var{end} seconds in the recording.
## @end deftypefn

function player = play (varargin)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  recorder = varargin{1};
  data = getaudiodata (recorder);
  player = audioplayer (data, get (recorder, "SampleRate"),
                        get (recorder, "BitsPerSample"));
  if (nargin == 1)
    play (player);
  else
    play (player, varargin{2});
  endif

endfunction
