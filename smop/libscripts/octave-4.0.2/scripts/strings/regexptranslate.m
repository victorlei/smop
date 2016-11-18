## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn {Function File} {} regexptranslate (@var{op}, @var{s})
## Translate a string for use in a regular expression.
##
## This may include either wildcard replacement or special character escaping.
##
## The behavior is controlled by @var{op} which can take the following
## values
##
## @table @asis
## @item @qcode{"wildcard"}
## The wildcard characters @code{.}, @code{*}, and @code{?} are replaced with
## wildcards that are appropriate for a regular expression.  For example:
##
## @example
## @group
## regexptranslate ("wildcard", "*.m")
##      @result{} ".*\.m"
## @end group
## @end example
##
## @item @qcode{"escape"}
## The characters @code{$.?[]}, that have special meaning for regular
## expressions are escaped so that they are treated literally.  For example:
##
## @example
## @group
## regexptranslate ("escape", "12.5")
##      @result{} "12\.5"
## @end group
## @end example
##
## @end table
## @seealso{regexp, regexpi, regexprep}
## @end deftypefn

function y = regexptranslate (op, s)

  if (nargin != 2)
    print_usage ();
  endif

  if (! ischar (op))
    error ("regexptranslate: operation OP must be a string");
  endif

  op = tolower (op);
  if (strcmp ("wildcard", op))
    y = strrep (strrep (strrep (s, '.', '\.'),
                                   '*', '.*'),
                                   '?', '.');
  elseif (strcmp ("escape", op))
    y = regexprep (s, '([][(){}.*+?^$|\\])', '\\$1');
  else
    error ("regexptranslate: invalid operation OP");
  endif

endfunction


%!assert (regexptranslate ("wildcard", "/a*b?c."), "/a.*b.c\\.")
%!assert (regexptranslate ("escape", '^.?[abc]$'), '\^\.\?\[abc\]\$')

## Test input validation
%!error <Invalid call to regexptranslate> regexptranslate ()
%!error <Invalid call to regexptranslate> regexptranslate ("wildcard")
%!error <Invalid call to regexptranslate> regexptranslate ("a", "b", "c")
%!error <invalid operation> regexptranslate ("foo", "abc")
%!error <operation OP must be a string> regexptranslate (10, "abc")

