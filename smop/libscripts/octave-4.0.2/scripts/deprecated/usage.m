## Copyright (C) 2014-2015 John W. Eaton
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
## @deftypefn {Built-in Function} {} usage (@var{msg})
##
## @code{usage} is deprecated and will be removed in Octave version 4.4.
## Please use @code{print_usage} in all new code.
##
## Print the message @var{msg}, prefixed by the string @samp{usage: }, and
## set Octave's internal error state such that control will return to the
## top level without evaluating any more commands.  This is useful for
## aborting from functions.
##
## After @code{usage} is evaluated, Octave will print a traceback of all
## the function calls leading to the usage message.
##
## You should use this function for reporting problems errors that result
## from an improper call to a function, such as calling a function with an
## incorrect number of arguments, or with arguments of the wrong type.  For
## example, most functions distributed with Octave begin with code like
## this
##
## @example
## @group
## if (nargin != 2)
##   usage (\"foo (a, b)\");
## endif
## @end group
## @end example
##
## @noindent
## to check for the proper number of arguments.
## @seealso{print_usage}
## @end deftypefn

## Deprecated in version 4.0

function retval = usage (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "usage is obsolete and will be removed from a future version of Octave, please use print_usage instead");
  endif

  retval = __usage__ (varargin{:});

endfunction

