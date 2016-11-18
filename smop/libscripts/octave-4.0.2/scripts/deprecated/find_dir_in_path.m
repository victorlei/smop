## Copyright (C) 2013-2015 John W. Eaton
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
## @deftypefn  {Built-in Function} {} find_dir_in_path (@var{dir})
## @deftypefnx {Built-in Function} {} find_dir_in_path (@var{dir}, "all")
## This function has been deprecated.  Use @code{dir_in_loadpath} instead.
## @seealso{dir_in_loadpath}
## @end deftypefn

## Deprecated in version 4.0

function retval = find_dir_in_path (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "find_dir_in_path is obsolete and will be removed from a future version of Octave, please use dir_in_loadpath instead");
  endif

  retval = dir_in_loadpath (varargin{:});

endfunction

