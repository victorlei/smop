## Copyright (C) 2007 Muthiah Annamalai
## Copyright (C) 2013-2015 Ben Abbott
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
## @deftypefn  {Function File} {@var{str} =} strjoin (@var{cstr})
## @deftypefnx {Function File} {@var{str} =} strjoin (@var{cstr}, @var{delimiter})
## Join the elements of the cell string array, @var{cstr}, into a single
## string.
##
## If no @var{delimiter} is specified, the elements of @var{cstr} are
## separated by a space.
##
## If @var{delimiter} is specified as a string, the cell string array is
## joined using the string.  Escape sequences are supported.
##
## If @var{delimiter} is a cell string array whose length is one less than
## @var{cstr}, then the elements of @var{cstr} are joined by interleaving the
## cell string elements of @var{delimiter}.  Escape sequences are not
## supported.
##
## @example
## @group
## strjoin (@{'Octave','Scilab','Lush','Yorick'@}, '*')
##       @result{} 'Octave*Scilab*Lush*Yorick'
## @end group
## @end example
## @seealso{strsplit}
## @end deftypefn

## Author: Muthiah Annamalai <muthiah.annamalai@uta.edu>
## Author: Ben Abbott <bpabbott@mac.com>

function rval = strjoin (cstr, delimiter)

  if (nargin == 1)
    delimiter = " ";
  elseif (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (! (iscellstr (cstr) && (ischar (delimiter) || iscellstr (delimiter))))
    print_usage ();
  endif

  if (numel (cstr) == 1)
    rval = cstr{1};
    return;
  endif

  if (ischar (delimiter))
    delimiter = do_string_escapes (delimiter);
    delimiter = {delimiter};
  endif

  num = numel (cstr);
  if (numel (delimiter) == 1 && num > 1)
    delimiter = repmat (delimiter, 1, num);
    delimiter(end) = {""};
  elseif (num > 0 && numel (delimiter) != num - 1)
    error ("strjoin:cellstring_delimiter_mismatch",
      "strjoin: the number of delimiters does not match the number of strings")
  else
    delimiter(end+1) = {""};
  endif

  if (num == 0)
    rval = "";
  else
    rval = [[cstr(:).'; delimiter(:).']{:}];
  endif

endfunction


%!assert (strjoin ({"hello"}, "-"), "hello")
%!assert (strjoin ({"hello", "world"}), "hello world")
%!assert (strjoin ({"Octave", "Scilab", "Lush", "Yorick"}, "*"),
%!  "Octave*Scilab*Lush*Yorick")
%!assert (strjoin ({"space", "comma", "dash", "semicolon", "done"},
%!  {" ", ",", "-", ";"}), "space comma,dash-semicolon;done")
%!assert (strjoin ({'Octave','Scilab'},'\n'), "Octave\nScilab")
%!assert (strjoin ({'Octave','Scilab'},{'\n'}), "Octave\\nScilab")
%!assert (strjoin ({},'foo'), "")

