## Copyright (C) 2004-2015 Petr Mikulik
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
## @deftypefn {Function File} {[@var{x}, @var{y}, @var{buttons}] =} __gnuplot_ginput__ (@var{f}, @var{n})
## Undocumented internal function.
## @end deftypefn

## This is ginput.m implementation for gnuplot and X11.
## It requires gnuplot 4.1 and later.

## This file initially bore the copyright statement
## Petr Mikulik
## History: June 2006; August 2005; June 2004; April 2004
## License: public domain

function [x, y, button] = __gnuplot_ginput__ (f, n)

  if (compare_versions (__gnuplot_version__ (), "4.0", "<="))
    error ("ginput: version %s of gnuplot not supported", gnuplot_version ());
  endif

  ostream = get (f, "__plot_stream__");
  if (numel (ostream) < 1)
    error ("ginput: stream to gnuplot not open");
  elseif (ispc ())
    if (numel (ostream) == 1)
      error ("ginput: Need mkfifo that is not implemented under Windows");
    endif
    use_mkfifo = false;
    istream = ostream(2);
    ostream = ostream(1);
  else
    use_mkfifo = true;
    ostream = ostream(1);
  endif

  if (nargin == 1)
    x = zeros (100, 1);
    y = zeros (100, 1);
    button = zeros (100, 1);
  else
    x = zeros (n, 1);
    y = zeros (n, 1);
    button = zeros (n, 1);
  endif

  if (use_mkfifo)
    gpin_name = tempname ();

    ##Mode: 6*8*8 ==  0600
    [err, msg] = mkfifo (gpin_name, 6*8*8);

    if (err)
      error ("ginput: Can not open fifo (%s)", msg);
    endif
  endif

  unwind_protect

    k = 0;
    while (true)
      k++;

      ## Notes: MOUSE_* can be undefined if user closes gnuplot by "q"
      ## or Alt-F4. Further, this abrupt close also requires the leading
      ## "\n" on the next line.
      if (use_mkfifo)
        fprintf (ostream, "set print \"%s\";\n", gpin_name);
        fflush (ostream);
        [gpin, err] = fopen (gpin_name, "r");
        if (err)
          error ("ginput: Can not open FIFO (%s)", msg);
        endif
        fputs (ostream, "pause mouse any;\n\n");
        fputs (ostream, "\nif (exists(\"MOUSE_KEY\") && exists(\"MOUSE_X\")) print MOUSE_X, MOUSE_Y, MOUSE_KEY; else print \"0 0 -1\"\n");

        ## Close output file, to force it to be flushed
        fputs (ostream, "set print;\n");
        fflush (ostream);

        ## Now read from fifo.
        [x(k), y(k), button(k), count] = fscanf (gpin, "%f %f %d", "C");
        fclose (gpin);
      else
        fputs (ostream, "set print \"-\";\n");
        fflush (ostream);
        fputs (ostream, "pause mouse any;\n\n");
        fputs (ostream, "\nif (exists(\"MOUSE_KEY\") && exists(\"MOUSE_X\")) key = (MOUSE_KEY==1063 ? 1 : MOUSE_KEY); print \"OCTAVE: \", MOUSE_X, MOUSE_Y, key; else print \"0 0 -1\"\n");

        ## Close output file, to force it to be flushed
        fputs (ostream, "set print;\n");
        fflush (ostream);

        str = {};
        while (isempty (str))
          str = fread (istream, "*char")';
          if (isempty (str))
            sleep (0.05);
          else
            str = regexp (str, 'OCTAVE:\s+[-+.\d]+\s+[-+.\d]+\s+\d*', 'match');
          endif
          fclear (istream);
        endwhile
        [x(k), y(k), button(k), count] = ...
          sscanf (str{end}(8:end), "%f %f %d", "C");
      endif

      if ([x(k), y(k), button(k)] == [0, 0, -1])
        ## Mousing not active (no plot yet).
        break;
      endif

      if (button(k) == 0x0D || button(k) == 0x0A)
        ## Stop when hitting a RETURN or ENTER key.
        x(k:end) = [];
        y(k:end) = [];
        button(k:end) = [];
        break;
      endif
      if (nargin > 1 && k == n)
        ## Input argument n was given, stop when k == n.
        break;
      endif

    endwhile

  unwind_protect_cleanup
    if (use_mkfifo)
      unlink (gpin_name);
    endif
  end_unwind_protect

endfunction

