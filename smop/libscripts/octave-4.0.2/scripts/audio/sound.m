## Copyright (C) 2015 Mike Miller
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
## @deftypefn  {Function File} {} sound (@var{y})
## @deftypefnx {Function File} {} sound (@var{y}, @var{fs})
## @deftypefnx {Function File} {} sound (@var{y}, @var{fs}, @var{nbits})
## Play audio data @var{y} at sample rate @var{fs} to the default audio
## device.
##
## The audio signal @var{y} can be a vector or a two-column array, representing
## mono or stereo audio, respectively.
##
## If @var{fs} is not given, a default sample rate of 8000 samples per second
## is used.
##
## The optional argument @var{nbits} specifies the bit depth to play to the
## audio device and defaults to 8 bits.
##
## For more control over audio playback, use the @code{audioplayer} class.
## @seealso{soundsc, record}
## @end deftypefn

function sound (y, fs, nbits)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin < 2 || isempty (fs))
    fs = 8000;
  elseif (! (isscalar (fs) && (fs > 0)))
    error ("sound: sample rate FS must be a positive number");
  endif

  if (nargin < 3 || isempty (nbits))
    nbits = 8;
  elseif (! (isscalar (nbits) && (nbits == 8 || nbits == 16 || nbits == 24)))
    error ("sound: bit depth NBITS must be 8, 16, or 24");
  endif

  play = audioplayer (y, fs, nbits);

  playblocking (play);

endfunction


## Tests of sound must not actually play anything.

## Test input validation
%!error sound ()
%!error sound (1,2,3,4)
%!error sound (1, -1)
%!error sound (1, [], 2)

