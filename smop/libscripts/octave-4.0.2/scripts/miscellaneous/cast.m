## Copyright (C) 2007-2015 John W. Eaton
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
## @deftypefn {Function File} {} cast (@var{val}, "@var{type}")
## Convert @var{val} to data type @var{type}.
##
## @var{val} must be one of the numeric classes:
##
## @example
## @group
## "double"
## "single"
## "logical"
## "char"
## "int8"
## "int16"
## "int32"
## "int64"
## "uint8"
## "uint16"
## "uint32"
## "uint64"
## @end group
## @end example
##
## The value @var{val} may be modified to fit within the range of the new type.
##
## Examples:
##
## @example
## @group
## cast (-5, "uint8")
##    @result{} 0
## cast (300, "int8")
##    @result{} 127
## @end group
## @end example
##
## @seealso{typecast, int8, uint8, int16, uint16, int32, uint32, int64, uint64, double, single, logical, char, class, typeinfo}
## @end deftypefn

## Author: jwe

function retval = cast (val, type)

  if (nargin != 2)
    print_usage ();
  endif

  if (! ischar (type))
    error ("cast: TYPE must be a string");
  elseif (! any (strcmp (type, {"int8"; "uint8"; "int16"; "uint16";
                                "int32"; "uint32"; "int64"; "uint64";
                                "double"; "single"; "logical"; "char"})))
    error ("cast: TYPE '%s' is not a built-in type", type);
  endif

  retval = feval (type, val);

endfunction


%!assert (cast (single (2.5), "double"), 2.5)
%!assert (cast (2.5, "single"), single (2.5))
%!assert (cast ([5 0 -5], "logical"), [true false true])
%!assert (cast ([65 66 67], "char"), "ABC")
%!assert (cast ([-2.5 1.1 2.5], "int8"), int8 ([-3 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "uint8"), uint8 ([0 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "int16"), int16 ([-3 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "uint16"), uint16 ([0 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "int32"), int32 ([-3 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "uint32"), uint32 ([0 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "int64"), int64 ([-3 1 3]))
%!assert (cast ([-2.5 1.1 2.5], "uint64"), uint64 ([0 1 3]))

## Test input validation
%!error cast ()
%!error cast (1)
%!error cast (1,2,3)
%!error <TYPE 'foobar' is not a built-in type> cast (1, "foobar")
%!error <TYPE must be a string> cast (1, {"foobar"})

