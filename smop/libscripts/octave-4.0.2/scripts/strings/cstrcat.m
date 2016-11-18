## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn {Function File} {} cstrcat (@var{s1}, @var{s2}, @dots{})
## Return a string containing all the arguments concatenated horizontally
## with trailing white space preserved.
##
## For example:
##
## @example
## @group
## cstrcat ("ab   ", "cd")
##       @result{} "ab   cd"
## @end group
## @end example
##
## @example
## @group
## s = [ "ab"; "cde" ];
## cstrcat (s, s, s)
##       @result{} "ab ab ab "
##          "cdecdecde"
## @end group
## @end example
## @seealso{strcat, char, strvcat}
## @end deftypefn

## Author: jwe

function st = cstrcat (varargin)

  if (nargin < 1)
    print_usage ();
  elseif (! iscellstr (varargin))
    error ("cstrcat: expecting arguments to character strings");
  endif

  st = [varargin{:}];

endfunction


## Test the dimensionality
## 1d
%!assert (cstrcat ("ab ", "ab "), "ab ab ")
## 2d
%!assert (cstrcat (["ab ";"cde"], ["ab ";"cde"]), ["ab ab ";"cdecde"])

%!assert (cstrcat ("foo", "bar"), "foobar")
%!assert (cstrcat (["a"; "bb"], ["foo"; "bar"]), ["a foo"; "bbbar"])

## Test input validation
%!error cstrcat ()
%!error cstrcat (1, 2)

