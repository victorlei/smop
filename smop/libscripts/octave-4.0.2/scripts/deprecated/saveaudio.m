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
## @deftypefn {Function File} {} saveaudio (@var{name}, @var{x}, @var{ext}, @var{bps})
##
## @code{saveaudio} is deprecated and will be removed in Octave version 4.4.
## Please use @code{audiowrite} in all new code.
##
## Save a vector @var{x} of audio data to the file
## @file{@var{name}.@var{ext}}.  The optional parameters @var{ext} and
## @var{bps} determine the encoding and the number of bits per sample used
## in the audio file (see @code{loadaudio}); defaults are @file{lin} and
## 8, respectively.
## @seealso{lin2mu, mu2lin, loadaudio, playaudio, setaudio, record}
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 5 September 1994
## Adapted-By: jwe

function saveaudio (name, x, ext, bps)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "saveaudio is obsolete and will be removed from a future version of Octave, please use audiowrite instead");
  endif

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  if (nargin == 2)
    ext = "lin";
  endif

  if (nargin < 4)
    bps = 8;
  elseif (bps != 8 && bps != 16)
    error ("saveaudio: BPS must be either 8 or 16");
  endif

  [nr, nc] = size (x);
  if (nc != 1)
    if (nr == 1)
      x = x';
      nr = nc;
    else
      error ("saveaudio: X must be a vector");
    endif
  endif

  num = fopen ([name, ".", ext], "wb");

  if (strcmp (ext, "lin") || strcmp (ext, "raw"))
    if (bps == 8)
      ld = max (abs (x));
      if (ld > 127)   # convert 16 to 8 bit
        if (ld < 16384)
          sc = 64 / ld;
        else
          sc = 1 / 256;
        endif
        x = fix (x * sc);
      endif
      x = x + 127;
      c = fwrite (num, x, "uchar");
    else
      c = fwrite (num, x, "short");
    endif
  elseif (strcmp (ext, "mu") || strcmp (ext, "au")
          || strcmp (ext, "snd") || strcmp (ext, "ul"))
    y = lin2mu (x);
    c = fwrite (num, y, "uchar");
  else
    fclose (num);
    error ("saveaudio: unsupported extension");
  endif

  fclose (num);

endfunction

