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
## @deftypefn {Function File} {} loadaudio (@var{name}, @var{ext}, @var{bps})
##
## @code{loadaudio} is deprecated and will be removed in Octave version 4.4.
## Please use @code{audioread} in all new code.
##
## Load audio data from the file @file{@var{name}.@var{ext}} into the
## vector @var{x}.
##
## The extension @var{ext} determines how the data in the audio file is
## interpreted; the extensions @file{lin} (default) and @file{raw}
## correspond to linear, the extensions @file{au}, @file{mu}, or @file{snd}
## to mu-law encoding.
##
## The argument @var{bps} can be either 8 (default) or 16, and specifies
## the number of bits per sample used in the audio file.
## @seealso{lin2mu, mu2lin, saveaudio, playaudio, setaudio, record}
## @end deftypefn


## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 10 April 1994
## Adapted-By: jwe

function X = loadaudio (name, ext, bps)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "loadaudio is obsolete and will be removed from a future version of Octave, please use audioread instead");
  endif

  if (nargin == 0 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 1)
    ext = "lin";
  endif

  if (nargin < 3)
    bps = 8;
  elseif (bps != 8 && bps != 16)
    error ("loadaudio: BPS must be either 8 or 16");
  endif

  name = [name, ".", ext];
  num = fopen (name, "rb");

  if (strcmp (ext, "lin") || strcmp (ext, "raw") || strcmp (ext, "pcm"))
    if (bps == 8)
      [Y, c] = fread (num, inf, "uchar");
      X = Y - 127;
    else
      [X, c] = fread (num, inf, "short");
    endif
  elseif (strcmp (ext, "mu") || strcmp (ext, "au")
          || strcmp (ext, "snd") || strcmp (ext, "ul"))
    [Y, c] = fread (num, inf, "uchar");
    ## remove file header
    m = find (Y(1:64) == 0, 1, "last");
    if (! isempty (m))
      Y(1:m) = [];
    endif
    X = mu2lin (Y, bps);
  else
    fclose (num);
    error ("loadaudio: unsupported extension");
  endif

  fclose (num);

endfunction

