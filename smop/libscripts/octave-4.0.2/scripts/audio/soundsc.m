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
## @deftypefn  {Function File} {} soundsc (@var{y})
## @deftypefnx {Function File} {} soundsc (@var{y}, @var{fs})
## @deftypefnx {Function File} {} soundsc (@var{y}, @var{fs}, @var{nbits})
## @deftypefnx {Function File} {} soundsc (@dots{}, [@var{ymin}, @var{ymax}])
## Scale the audio data @var{y} and play it at sample rate @var{fs} to the
## default audio device.
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
## By default, @var{y} is automatically normalized to the range [-1, 1].  If the
## range [@var{ymin}, @var{ymax}] is given, then elements of @var{y} that fall
## within the range @var{ymin} @leq{} @var{y} @leq{} @var{ymax} are scaled to
## the range [-1, 1] instead.
##
## For more control over audio playback, use the @code{audioplayer} class.
## @seealso{sound, record}
## @end deftypefn

function soundsc (y, fs, nbits, yrange)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  if (nargin < 4)
    yrange = [];
  endif

  if (nargin < 2 || isempty (fs))
    fs = 8000;
  elseif (nargin == 2 && numel (fs) > 1)
    yrange = fs;
    fs = 8000;
  elseif (! (isscalar (fs) && (fs > 0)))
    error ("soundsc: sample rate FS must be a positive number");
  endif

  if (nargin < 3 || isempty (nbits))
    nbits = 8;
  elseif (nargin == 3 && numel (nbits) > 1)
    yrange = nbits;
    nbits = 8;
  elseif (! (isscalar (nbits) && (nbits == 8 || nbits == 16 || nbits == 24)))
    error ("soundsc: bit depth NBITS must be 8, 16, or 24");
  endif

  if (isreal (yrange) && (numel (yrange) == 2) && (yrange(1) <= yrange(2)))
    ymin = yrange(1);
    ymax = yrange(2);
  elseif (isempty (yrange))
    ymin = min (y(:));
    ymax = max (y(:));
  else
    error ("soundsc: audio range must be a 2-element [YMIN, YMAX] vector");
  endif

  ymin = double (ymin);
  ymax = double (ymax);
  ymedian = (ymax + ymin) / 2;
  yscale = 2 / (ymax - ymin);

  y = (double (y) - ymedian) .* yscale;

  play = audioplayer (y, fs, nbits);

  playblocking (play);

endfunction


## Tests of soundsc must not actually play anything.

## Test input validation
%!error soundsc ()
%!error soundsc (1,2,3,4,5)
%!error soundsc (1, -1)
%!error soundsc (1, [], 2)
%!error soundsc (1, [2 1])
%!error soundsc (1, [1 2 3])
%!error soundsc (1, 8000, [2 1])
%!error soundsc (1, 8000, [1 2 3])
%!error soundsc (1, 8000, 8, [2 1])
%!error soundsc (1, 8000, 8, [1 2 3])

