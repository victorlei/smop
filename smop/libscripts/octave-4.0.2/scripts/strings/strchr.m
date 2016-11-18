## Copyright (C) 2008-2015 Jaroslav Hajek
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
## @deftypefn  {Function File} {@var{idx} =} strchr (@var{str}, @var{chars})
## @deftypefnx {Function File} {@var{idx} =} strchr (@var{str}, @var{chars}, @var{n})
## @deftypefnx {Function File} {@var{idx} =} strchr (@var{str}, @var{chars}, @var{n}, @var{direction})
## @deftypefnx {Function File} {[@var{i}, @var{j}] =} strchr (@dots{})
## Search for the string @var{str} for occurrences of characters from
## the set @var{chars}.
##
## The return value(s), as well as the @var{n} and @var{direction} arguments
## behave identically as in @code{find}.
##
## This will be faster than using regexp in most cases.
##
## @seealso{find}
## @end deftypefn

function varargout = strchr (str, chars, varargin)

  if (nargin < 2)
    print_usage ();
  elseif (! ischar (str))
    error ("strchr: STR argument must be a string or string array");
  elseif (! ischar (chars))
    error ("strchr: CHARS argument must be a string");
  endif

  if (isempty (chars))
    mask = false (size (str));
  elseif (length (chars) <= 4)
    ## With a few characters, it pays off to build the mask incrementally.
    ## We do it via a for loop to save memory.
    mask = str == chars(1);
    for i = 2:length (chars)
      mask |= str == chars(i);
    endfor
  else
    ## Index the str into a mask of valid values.
    ## This is slower than it could be because of the +1 issue.
    f = false (256, 1);
    f(uint8 (chars) + 1) = true;
    ## Default goes via double -- unnecessarily long.
    si = uint32 (str);
    ## in-place is faster than str+1
    ++si;
    mask = reshape (f(si), size (str));
  endif

  varargout = cell (1, nargout);
  varargout{1} = [];
  [varargout{:}] = find (mask, varargin{:});

endfunction


%!assert (strchr ("Octave is the best software", ""), zeros (1,0))
%!assert (strchr ("Octave is the best software", "best"), [3, 6, 9, 11, 13, 15, 16, 17, 18, 20, 23, 27])
%!assert (strchr ("Octave is the best software", "software"), [3, 4, 6, 9, 11, 13, 16, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27])

## Test input validation
%!error strchr ()
%!error strchr (1)
%!error <STR argument must be a string> strchr (1, "aeiou")
%!error <CHARS argument must be a string> strchr ("aeiou", 1)

