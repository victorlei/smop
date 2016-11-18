## Copyright (C) 2008-2015 John W. Eaton
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
## @deftypefn {Function File} {} isstrprop (@var{str}, @var{prop})
## Test character string properties.
##
## For example:
##
## @example
## @group
## isstrprop ("abc123", "alpha")
## @result{} [1, 1, 1, 0, 0, 0]
## @end group
## @end example
##
## If @var{str} is a cell array, @code{isstrpop} is applied recursively to
## each element of the cell array.
##
## Numeric arrays are converted to character strings.
##
## The second argument @var{prop} must be one of
##
## @table @asis
## @item @qcode{"alpha"}
## True for characters that are alphabetic (letters).
##
## @item  @qcode{"alnum"}
## @itemx @qcode{"alphanum"}
## True for characters that are alphabetic or digits.
##
## @item @qcode{"lower"}
## True for lowercase letters.
##
## @item @qcode{"upper"}
## True for uppercase letters.
##
## @item @qcode{"digit"}
## True for decimal digits (0-9).
##
## @item @qcode{"xdigit"}
## True for hexadecimal digits (@nospell{a-fA-F0-9}).
##
## @item  @qcode{"space"}
## @itemx @qcode{"wspace"}
## True for whitespace characters (space, formfeed, newline, carriage return,
## tab, vertical tab).
##
## @item @qcode{"punct"}
## True for punctuation characters (printing characters except space or
## letter or digit).
##
## @item @qcode{"cntrl"}
## True for control characters.
##
## @item  @qcode{"graph"}
## @itemx @qcode{"graphic"}
## True for printing characters except space.
##
## @item @qcode{"print"}
## True for printing characters including space.
##
## @item @qcode{"ascii"}
## True for characters that are in the range of ASCII encoding.
##
## @end table
##
## @seealso{isalpha, isalnum, islower, isupper, isdigit, isxdigit,
## isspace, ispunct, iscntrl, isgraph, isprint, isascii}
## @end deftypefn

function retval = isstrprop (str, prop)

  if (nargin != 2)
    print_usage ();
  endif

  switch (prop)
    case "alpha"
      retval = isalpha (str);
    case {"alnum", "alphanum"}
      retval = isalnum (str);
    case "ascii"
      retval = isascii (str);
    case "cntrl"
      retval = iscntrl (str);
    case "digit"
      retval = isdigit (str);
    case {"graph", "graphic"}
      retval = isgraph (str);
    case "lower"
      retval = islower (str);
    case "print"
      retval = isprint (str);
    case "punct"
      retval = ispunct (str);
    case {"space", "wspace"}
      retval = isspace (str);
    case "upper"
      retval = isupper (str);
    case "xdigit"
      retval = isxdigit (str);
    otherwise
      error ("isstrprop: invalid string property");
  endswitch

endfunction


%!assert (isstrprop ("abc123", "alpha"), logical ([1, 1, 1, 0, 0, 0]))
%!assert (isstrprop ("abc123", "digit"), logical ([0, 0, 0, 1, 1, 1]))
%!assert (isstrprop ("Hello World", "wspace"), isspace ("Hello World"))
%!assert (isstrprop ("Hello World", "graphic"), isgraph ("Hello World"))
%!assert (isstrprop (char ("AbC", "123"), "upper"), logical ([1 0 1; 0 0 0]))
%!assert (isstrprop ({"AbC", "123"}, "lower"), {logical([0 1 0]), logical([0 0 0])})

## Test input validation
%!error isstrprop ()
%!error isstrprop ("abc123")
%!error isstrprop ("abc123", "alpha", "alpha")
%!error <invalid string property> isstrprop ("abc123", "foo")

