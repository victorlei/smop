## Copyright (C) 2015 Mike Miller
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
## @deftypefn  {Function File} {} record (@var{sec})
## @deftypefnx {Function File} {} record (@var{sec}, @var{fs})
## Record @var{sec} seconds of audio from the system's default audio input at
## a sampling rate of 8000 samples per second.
##
## If the optional argument @var{fs} is given, it specifies the sampling rate
## for recording.
##
## For more control over audio recording, use the @code{audiorecorder} class.
## @seealso{sound, soundsc}
## @end deftypefn

function x = record (sec, fs)

  if (nargin == 1)
    fs = 8000;
  elseif (nargin != 2)
    print_usage ();
  endif

  if (! (isscalar (sec) && (sec >= 0)))
    error ("record: recording duration SEC must be a non-negative number");
  endif

  if (! (isscalar (fs) && (fs > 0)))
    error ("record: sample rate FS must be a positive number");
  endif

  x = [];

  if (sec > 0)

    rec = audiorecorder (fs, 16, 1);

    recordblocking (rec, sec);

    x = getaudiodata (rec);

  endif

endfunction


## Tests of record must not actually record anything.

%!assert (isempty (record (0)))

## Test input validation
%!error record ()
%!error record (1,2,3)
%!error record (-1)
%!error record (1, -1)

