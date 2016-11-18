## Copyright (C) 2013-2015 John Donoghue
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
## @deftypefn  {Command} {} prefdir
## @deftypefnx {Command} {@var{dir} =} prefdir
## Return the directory that contains the preferences for Octave.
##
## Examples:
##
## Display the preferences directory
##
## @example
## prefdir
## @end example
##
## Change to the preferences folder
##
## @example
## cd (prefdir)
## @end example
## @seealso{getpref, setpref, addpref, rmpref, ispref}
## @end deftypefn

## Author: John Donoghue
## Version: 0.01

function folder = prefdir ()

  folder = get_home_directory ();

endfunction

