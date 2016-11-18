## Copyright (C) 2006-2015 Daniel Sebald
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
## @deftypefn {Function File} {@var{version} =} __gnuplot_version__ ()
## Undocumented internal function.
## @end deftypefn

## Return the version of gnuplot we are using.  Note that we do not
## attempt to handle the case of the user switching to different
## versions of gnuplot during the same session.

function version = __gnuplot_version__ ()

  persistent __version__ = "";

  if (isempty (__version__))
    [status, output] = system (sprintf ('"%s" --version', gnuplot_binary ()));
    if (status != 0)
      ## This message ends in a newline so that the traceback messages
      ## are skipped and people might actually see the message, read it,
      ## comprehend it, take the advice it gives, and stop asking us
      ## why plotting fails when gnuplot is not found.
      error ("you must have gnuplot installed to display graphics; if you have gnuplot installed in a non-standard location, see the 'gnuplot_binary' function\n");
    endif
    output = strrep (output, "gnuplot", "");
    output = strrep (output, "patchlevel", ".");
    output = strrep (output, "\n", "");
    output = strrep (output, "\r", "");
    __version__ = strrep (output, " ", "");
  endif

  version = __version__;

endfunction

