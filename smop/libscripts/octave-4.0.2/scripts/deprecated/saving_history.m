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
## @deftypefn  {Built-in Function} {@var{val} =} saving_history ()
## @deftypefnx {Built-in Function} {@var{old_val} =} saving_history (@var{new_val})
## @deftypefnx {Built-in Function} {} saving_history (@var{new_val}, "local")
## This function has been deprecated.  Use @code{@file{history_save}} instead.
## @seealso{history_save}
## @end deftypefn

## Deprecated in 3.8

function retval = saving_history (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "saving_history is obsolete and will be removed from a future version of Octave, please use history_save instead");
  endif

  retval = save_default_options (varargin{:});

endfunction

