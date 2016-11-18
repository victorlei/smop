## Copyright (C) 2013-2015 Rik Wehbring
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
## @deftypefn {Built-in Function} {} read_readline_init_file (@var{file})
## This function has been deprecated.  Use
## @code{@file{readline_read_init_file}} instead.
## @seealso{readline_read_init_file}
## @end deftypefn

## Deprecated in 3.8

function read_readline_init_file (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "read_readline_init_file is obsolete and will be removed from a future version of Octave, please use readline_read_init_file instead");
  endif

  readline_read_init_file (varargin{:});

endfunction

