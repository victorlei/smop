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
## @deftypefn  {Function File} {} substr (@var{s}, @var{offset})
## @deftypefnx {Function File} {} substr (@var{s}, @var{offset}, @var{len})
## Return the substring of @var{s} which starts at character number
## @var{offset} and is @var{len} characters long.
##
## Position numbering for offsets begins with 1.  If @var{offset} is negative,
## extraction starts that far from the end of the string.
##
## If @var{len} is omitted, the substring extends to the end of @var{s}.  A
## negative value for @var{len} extracts to within @var{len} characters of
## the end of the string
##
## Examples:
##
## @example
## @group
## substr ("This is a test string", 6, 9)
##      @result{} "is a test"
## substr ("This is a test string", -11)
##      @result{} "test string"
## substr ("This is a test string", -11, -7)
##      @result{} "test"
## @end group
## @end example
##
## This function is patterned after the equivalent function in Perl.
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

function t = substr (s, offset, len)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! ischar (s))
    error ("substr: S must be a string or string array");
  elseif (! isscalar (offset) || (nargin == 3 && ! isscalar (len)))
    error ("substr: OFFSET and LEN must be scalar integers");
  endif

  offset = fix (offset);
  nc = columns (s);
  if (abs (offset) > nc || offset == 0)
    error ("substr: OFFSET = %d out of range", offset);
  endif

  if (offset <= 0)
    offset += nc + 1;
  endif

  if (nargin == 2)
    eos = nc;
  else
    len = fix (len);
    if (len < 0)
      eos = nc + len;
    else
      eos = offset + len - 1;
    endif
  endif

  if (eos > nc)
    error ("substr: length LEN = %d out of range", len);
  elseif (offset > eos && len != 0)
    error ("substr: No overlap with chosen values of OFFSET and LEN");
  endif

  t = s(:, offset:eos);

endfunction


%!assert (substr ("This is a test string", 6, 9), "is a test")
%!assert (substr ("This is a test string", -11), "test string")
%!assert (substr ("This is a test string", -11, 4), "test")
%!assert (substr ("This is a test string", -11, -7), "test")
%!assert (substr ("This is a test string", 1, -7), "This is a test")
%!assert (isempty (substr ("This is a test string", 1, 0)))

## Test input validation
%!error substr ()
%!error substr ("foo", 2, 3, 4)
%!error substr (ones (5, 1), 1, 1)
%!error substr ("foo", ones (2,2))
%!error substr ("foo", 1, ones (2,2))
%!error substr ("foo", 0)
%!error substr ("foo", 5)
%!error substr ("foo", 1, 5)
%!error substr ("foo", -1, 5)
%!error substr ("foo", 2, -5)

