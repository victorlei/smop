## Copyright (C) 2009-2015 Søren Hauberg
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
## @deftypefn {Function File} {} __additional_help_message__ ()
## Undocumented internal function.
## @end deftypefn

function msg = __additional_help_message__ ()

  if (suppress_verbose_help_message ())
    msg = "";
  else
    msg = "\n\
Additional help for built-in functions and operators is\n\
available in the online version of the manual.  Use the command\n\
'doc <topic>' to search the manual index.\n\
\n\
Help and information about Octave is also available on the WWW\n\
at http://www.octave.org and via the help@octave.org\n\
mailing list.\n";
  endif

endfunction

