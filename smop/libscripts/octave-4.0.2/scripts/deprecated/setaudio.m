## Copyright (C) 1995-2015 John W. Eaton
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
## @deftypefn  {Function File} {} setaudio ()
## @deftypefnx {Function File} {} setaudio (@var{w_type})
## @deftypefnx {Function File} {} setaudio (@var{w_type}, @var{value})
##
## @code{setaudio} is deprecated and will be removed in Octave version 4.4.
## Please scale the audio signal in all new code or use the operating system's
## native tools to adjust audio input and output levels.
##
## Execute the shell command @samp{mixer}, possibly with optional
## arguments @var{w_type} and @var{value}.
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 5 October 1994
## Adapted-By: jwe

function setaudio (w_type, value)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "setaudio is obsolete and will be removed from a future version of Octave, please scale the audio signal instead");
  endif

  if (nargin == 0)
    system ("mixer");
  elseif (nargin == 1)
    system (sprintf ("mixer %s", w_type));
  elseif (nargin == 2)
    system (sprintf ("mixer %s %d", w_type, value));
  else
    print_usage ();
  endif

endfunction

