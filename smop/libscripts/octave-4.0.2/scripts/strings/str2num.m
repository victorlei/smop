## Copyright (C) 1996-2015 Kurt Hornik
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
## @deftypefn  {Function File} {@var{x} =} str2num (@var{s})
## @deftypefnx {Function File} {[@var{x}, @var{state}] =} str2num (@var{s})
## Convert the string (or character array) @var{s} to a number (or an array).
##
## Examples:
##
## @example
## @group
## str2num ("3.141596")
##       @result{} 3.141596
##
## str2num (["1, 2, 3"; "4, 5, 6"])
##       @result{} 1  2  3
##          4  5  6
## @end group
## @end example
##
## The optional second output, @var{state}, is logically true when the
## conversion is successful.  If the conversion fails the numeric output,
## @var{x}, is empty and @var{state} is false.
##
## @strong{Caution:} As @code{str2num} uses the @code{eval} function to do the
## conversion, @code{str2num} will execute any code contained in the string
## @var{s}.  Use @code{str2double} for a safer and faster conversion.
##
## For cell array of strings use @code{str2double}.
## @seealso{str2double, eval}
## @end deftypefn

## Author: jwe

function [m, state] = str2num (s)

  if (nargin != 1)
    print_usage ();
  elseif (! ischar (s))
    error ("str2num: S must be a string or string array");
  endif

  s(:, end+1) = ";";
  s = sprintf ("m = [%s];", reshape (s', 1, numel (s)));
  state = true;
  eval (s, "m = []; state = false;");
  if (ischar (m))
    m = [];
    state = false;
  endif

endfunction


%!assert (str2num ("-1.3e2"), -130)
%!assert (str2num ("[1, 2; 3, 4]"), [1, 2; 3, 4])

%!test
%! [x, state] = str2num ("pi");
%! assert (state);
%! [x, state] = str2num ("Hello World");
%! assert (! state);

## Test input validation
%!error str2num ()
%!error str2num ("string", 1)
%!error <S must be a string> str2num ({"string"})

