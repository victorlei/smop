## Copyright (C) 2005-2015 Søren Hauberg
## Copyright (C) 2010 VZLU Prague, a.s.
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

## Executes a shell command. In the end it calls system() but in case of
## windows will first check if sh.exe works.
##
## If VERBOSE is true, it will prints the output to STDOUT in real time and
## the second output argument will be an empty string. Otherwise, it will
## contain the output of the execeuted command.

function [status, output] = shell (cmd, verbose)
  persistent have_sh;

  cmd = strrep (cmd, "\\", "/");
  if (ispc () && ! isunix ())
    if (isempty (have_sh))
      if (system ('sh.exe -c "exit"'))
        have_sh = false;
      else
        have_sh = true;
      endif
    endif
    if (have_sh)
      cmd = ['sh.exe -c "' cmd '"'];
    else
      error ("pkg: unable to find the command shell.");
    endif
  endif
  ## if verbose, we want to display the output in real time. To do this, we
  ## must call system with 1 output argument. But then the variable `output'
  ## won't exist. So we initialize it empty. If an error does occur, and we
  ## are verbose we will return an empty string but it's all fine since
  ## the error message has already been displayed.
  output = "";
  if (verbose)
    [status] = system (cmd);
  else
    [status, output] = system (cmd);
  endif
endfunction

