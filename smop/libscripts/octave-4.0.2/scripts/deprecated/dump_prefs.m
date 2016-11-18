## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn  {Function File} {} dump_prefs ()
## @deftypefnx {Function File} {} dump_prefs (@var{fid})
##
## @code{dump_prefs} is deprecated and will be removed in Octave version 4.4.
## Please use individual preference get/set routines in all new code.
##
## Dump the current settings of all user preferences to stdout in a format that
## can be parsed by Octave later.
##
## If the optional argument @var{fid} is given then the results are written to
## the file specified by file descriptor @var{fid}.
## @seealso{octave_config_info}
## @end deftypefn

## Author: jwe

## Deprecated in 4.0

function dump_prefs (fid)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "dump_prefs is obsolete and will be removed from a future version of Octave, recode using individual preference get/set routines");
  endif

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0)
    fid = stdout;
  endif

  ## FIXME: It would be nice to be able to get the list of built-in variables
  ## directly from Octave so that we wouldn't have to remember to update it
  ## each time the list of preference variables changes

  ## FIXME: Update this list for 4.2.0 release
  ##        Example, 'gnuplot_command_end' is no longer valid.

  pref_list = {"EDITOR"
              "EXEC_PATH"
              "IMAGE_PATH"
              "PAGER"
              "PS1"
              "PS2"
              "PS4"
              "beep_on_error"
              "completion_append_char"
              "crash_dumps_octave_core"
              "echo_executing_commands"
              "fixed_point_format"
              "gnuplot_binary"
              "gnuplot_command_end"
              "gnuplot_command_plot"
              "gnuplot_command_replot"
              "gnuplot_command_splot"
              "gnuplot_command_title"
              "gnuplot_command_using"
              "gnuplot_command_with"
              "history_file"
              "history_size"
              "ignore_function_time_stamp"
              "info_file"
              "info_program"
              "makeinfo_program"
              "max_recursion_depth"
              "output_max_field_width"
              "output_precision"
              "page_output_immediately"
              "page_screen_output"
              "print_answer_id_name"
              "print_empty_dimensions"
              "save_precision"
              "saving_history"
              "sighup_dumps_octave_core"
              "sigterm_dumps_octave_core"
              "silent_functions"
              "split_long_rows"
              "string_fill_char"
              "struct_levels_to_print"
              "suppress_verbose_help_message"};

  for i = 1:rows (pref_list)
    pref = pref_list{i};
    try
      val = feval (pref);
      if (isnumeric (val))
        val = sprintf ("%g", val);
      endif
      fprintf (fid, "  %s = %s\n", pref, val);
    catch
      fprintf (fid, "# %s = <no value or error in displaying it>\n", pref);
    end_try_catch
  endfor

endfunction


%!error dump_prefs (1,2)

