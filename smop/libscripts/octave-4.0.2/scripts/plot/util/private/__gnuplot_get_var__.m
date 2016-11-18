## Copyright (C) 2009-2015 Ben Abbott
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
## @deftypefn {Function File} {@var{value} =} __gnuplot_get_var__ (@var{h}, @var{name}, @var{fmt})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2009-02-07

function gp_var_value = __gnuplot_get_var__ (h, gp_var_name, fmt = "")

  if (numel (h) == 1 && isfigure (h))
    if (isempty (get (h, "__plot_stream__")))
      ostream = __gnuplot_open_stream__ (2, h);
    else
      ostream = get (h, "__plot_stream__");
    endif
  else
    ostream = h;
  endif
  if (numel (ostream) < 1)
    error ("__gnuplot_get_var__: stream to gnuplot not open");
  elseif (ispc ())
    if (numel (ostream) == 1)
      error ("__gnuplot_get_var__: Need mkfifo that is not implemented under Windows");
    endif
    use_mkfifo = false;
    istream = ostream(2);
    ostream = ostream(1);
  else
    use_mkfifo = true;
    ostream = ostream(1);
  endif

  if (use_mkfifo)
    gpin_name = tempname ();

    ## Mode: 0600 == 6*8*8
    [err, msg] = mkfifo (gpin_name, 6*8*8);

    if (err)
      error ("__gnuplot_get_var__: Can not make FIFO (%s)", msg);
    endif
  endif

  gp_var_name = strtrim (gp_var_name);
  n = min (strfind (gp_var_name, " "), strfind (gp_var_name, ",")) - 1;
  if (isempty (n))
    n = numel (gp_var_name);
  endif

  unwind_protect

    ## Notes: Variables may be undefined if user closes gnuplot by "q"
    ## or Alt-F4.  Further, this abrupt close also requires the leading
    ## "\n" on the next line.
    if (use_mkfifo)
      fprintf (ostream, "\nset print \"%s\";\n", gpin_name);
      fflush (ostream);
      [gpin, err] = fopen (gpin_name, "r");
      if (err)
        ## Try a second time, and then give an error.
        [gpin, err] = fopen (gpin_name, "r");
      endif
      if (err)
        error ("__gnuplot_get_var__: can not open FIFO");
      endif
      gp_cmd = sprintf ("\nif (exists(\"%s\")) print %s; else print NaN\n",
                        gp_var_name(1:n), gp_var_name);
      fputs (ostream, gp_cmd);

      ## Close output file, to force it to be flushed
      fputs (ostream, "set print;\n");
      fflush (ostream);

      ## Now read from fifo.
      reading = true;
      str = {};
      while (reading)
        str{end+1} = fgets (gpin);
        if (isnumeric (str{end}) && (str{end} == -1))
          reading = false;
          str = str(1:(end-1));
        endif
      endwhile
      str = strcat (str{:});
      fclose (gpin);
    else
      ## Direct gnuplot to print to <STDOUT>
      fprintf (ostream, "set print \"-\";\n");
      fflush (ostream);
      gp_cmd = sprintf ("\nif (exists(\"%s\")) print \"OCTAVE: \", %s; else print NaN\n",
                        gp_var_name(1:n), gp_var_name);
      fputs (ostream, gp_cmd);
      fflush (ostream);
      ## Direct gnuplot to print to <STDERR>
      fputs (ostream, "set print;\n");
      fflush (ostream);

      str = {};
      while (isempty (str))
        str = fread (istream, "*char")';
        if (isempty (str))
          sleep (0.05);
        else
          str = regexp (str, 'OCTAVE:.*', "match");
          str = str{end}(8:end);
        endif
        fclear (istream);
      endwhile
    endif

    ## Strip out EOLs and the continuation character "|"
    str(str=="\n" | str=="\r") = "";
    n_continue = strfind (str, " \\ ");
    if (! isempty (n_continue))
      str(n_continue+1) = "";
    endif

    if (isempty (fmt))
      gp_var_value = strtrim (str);
    else
      gp_var_value = sscanf (str, fmt);
    endif

  unwind_protect_cleanup
    if (use_mkfifo)
      unlink (gpin_name);
    endif
  end_unwind_protect

endfunction

