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
## @deftypefn  {Function File} {} playaudio (@var{name}, @var{ext})
## @deftypefnx {Function File} {} playaudio (@var{x})
##
## @code{playaudio} is deprecated and will be removed in Octave version 4.4.
## Please use @code{audioplayer} in all new code.
##
## Play the audio file @file{@var{name}.@var{ext}} or the audio data
## stored in the vector @var{x}.
## @seealso{lin2mu, mu2lin, loadaudio, saveaudio, setaudio, record}
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 11 April 1994
## Adapted-By: jwe

function playaudio (name, ext)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "playaudio is obsolete and will be removed from a future version of Octave, please use audioplayer instead");
  endif

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1 && isnumeric (name))
    ## play a vector
    if (! isvector (name))
      error ("playaudio: X must be a vector");
    endif
    X = name(:) + 127;
    unwind_protect
      file = tempname ();
      fid = fopen (file, "wb");
      fwrite (fid, X, "uchar");
      fclose (fid);
      [status, out] = system (sprintf ('cat "%s" > /dev/dsp', file));
      if (status != 0)
        system (sprintf ("paplay --raw \"%s\"", file));
      endif
    unwind_protect_cleanup
      unlink (file);
    end_unwind_protect
  elseif (nargin >= 1 && ischar (name))
    ## play a file
    if (nargin == 1)
      name = [name ".lin"];
    elseif (nargin == 2)
      name = [name "." ext];
    endif
    if (any (strcmp (ext, {"lin", "raw"})))
      [status, out] = system (sprintf ('cat "%s" > /dev/dsp', name));
      if (status != 0)
        system (sprintf ('paplay --raw "%s"', name));
      endif
    elseif (any (strcmp (ext, {"mu", "au" "snd", "ul"})))
      [status, out] = system (sprintf ('cat "%s" > /dev/audio', name));
      if (status != 0)
        system (sprintf ('paplay "%s"', name));
      endif
    else
      error ("playaudio: unsupported extension '%s'", ext);
    endif
  else
    print_usage ();
  endif

endfunction


## Test input validation
%!error playaudio ()
%!error playaudio (1,2,3)
%!error <X must be a vector> playaudio (magic (3))
%!error <unsupported extension> playaudio ("file", "abc")
%!error playaudio ({"abc"})

