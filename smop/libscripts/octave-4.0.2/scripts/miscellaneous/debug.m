## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn {Function File} {} debug ()
## Summary of debugging commands.
##
## For more information on each command and available options use
## @code{help CMD}.
##
## The debugging commands available in Octave are
##
## @table @code
## @item dbstop
## Add a breakpoint.
##
## @item dbclear
## Remove a breakpoint.
##
## @item dbstatus
## List all breakpoints.
##
## @item dbwhere
## Report the current file and line number where execution is stopped.
##
## @item dbtype
## Display the code of the function being debugged, enumerating
## the line numbers.
##
## @item dblist
## List 10 lines of code centered around the line number where execution is
## stopped.
##
## @item  dbstep
## @itemx dbnext
## Execute (step) one or more lines, follow execution into (step into) a
## function call, or execute until the end of a function (step out), and
## re-enter debug mode.
##
## @item dbcont
## Continue normal code execution from the debug prompt.
##
## @item dbquit
## Quit debugging mode immediately and return to the main prompt.
##
## @item dbstack
## Print a backtrace of the execution stack.
##
## @item dbup
## Move up the execution stack.
##
## @item dbdown
## Move down the execution stack.
##
## @item keyboard
## Force entry into debug mode from an m-file.
##
## @item debug_on_error
## Configure whether Octave enters debug mode when it encounters an error.
##
## @item debug_on_warning
## Configure whether Octave enters debug mode when it encounters a warning.
##
## @item debug_on_interrupt
## Configure whether Octave enters debug mode when it encounters an interrupt.
##
## @item isdebugmode
## Return true if in debug mode.
## @end table
##
## @noindent
## When Octave encounters a breakpoint, or other reason to enter debug mode,
## the prompt changes to @qcode{"debug>"}.  The workspace of the function
## where the breakpoint was encountered becomes available and any Octave
## command that is valid in that workspace context may be executed.
##
## @seealso{dbstop, dbclear, dbstatus, dbwhere, dbtype, dbcont, dbquit,
## dbstack, dbup, dbdown, keyboard, debug_on_error, debug_on_warning,
## debug_on_interrupt, isdebugmode}
## @end deftypefn

function debug ()
  help ("debug");
endfunction


## Mark file as being tested.  No real test needed for a documentation .m file
%!assert (1)

