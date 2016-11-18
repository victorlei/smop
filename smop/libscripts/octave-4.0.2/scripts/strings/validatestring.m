## Copyright (C) 2008-2015 Bill Denney
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
## @deftypefn  {Function File} {@var{validstr} =} validatestring (@var{str}, @var{strarray})
## @deftypefnx {Function File} {@var{validstr} =} validatestring (@var{str}, @var{strarray}, @var{funcname})
## @deftypefnx {Function File} {@var{validstr} =} validatestring (@var{str}, @var{strarray}, @var{funcname}, @var{varname})
## @deftypefnx {Function File} {@var{validstr} =} validatestring (@dots{}, @var{position})
## Verify that @var{str} is an element, or substring of an element, in
## @var{strarray}.
##
## When @var{str} is a character string to be tested, and @var{strarray} is a
## cellstr of valid values, then @var{validstr} will be the validated form
## of @var{str} where validation is defined as @var{str} being a member
## or substring of @var{validstr}.  This is useful for both verifying
## and expanding short options, such as @qcode{"r"}, to their longer forms,
## such as @qcode{"red"}.  If @var{str} is a substring of @var{validstr}, and
## there are multiple matches, the shortest match will be returned if all
## matches are substrings of each other.  Otherwise, an error will be raised
## because the expansion of @var{str} is ambiguous.  All comparisons are case
## insensitive.
##
## The additional inputs @var{funcname}, @var{varname}, and @var{position}
## are optional and will make any generated validation error message more
## specific.
##
## Examples:
## @c Set example in small font to prevent overfull line
##
## @smallexample
## @group
## validatestring ("r", @{"red", "green", "blue"@})
## @result{} "red"
##
## validatestring ("b", @{"red", "green", "blue", "black"@})
## @result{} error: validatestring: multiple unique matches were found for 'b':
##    blue, black
## @end group
## @end smallexample
##
## @seealso{strcmp, strcmpi, validateattributes, inputParser}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>

function str = validatestring (str, strarray, varargin)

  if (nargin < 2 || nargin > 5)
    print_usage ();
  endif

  position = 0;
  ## Process input arguments
  if (! isempty (varargin) && isnumeric (varargin{end}))
    position = fix (varargin{end});
    varargin(end) = [];
  endif

  funcname = varname = "";
  char_idx = cellfun ("isclass", varargin, "char");
  n_chararg = sum (char_idx);
  if (n_chararg > 2)
    error ("validatestring: invalid number of character inputs (3)");
  elseif (n_chararg == 2)
    [funcname, varname] = deal (varargin{char_idx});
  elseif (n_chararg == 1)
    funcname = varargin{char_idx};
  endif

  ## Check the inputs
  if (! ischar (str))
    error ("validatestring: STR must be a character string");
  elseif (! isrow (str))
    error ("validatestring: STR must be a single row vector");
  elseif (! iscellstr (strarray))
    error ("validatestring: STRARRAY must be a cellstr");
  elseif (! isempty (funcname) && ! isrow (funcname))
    error ("validatestring: FUNCNAME must be a single row vector");
  elseif (! isempty (varname) && ! isrow (varname))
    error ("validatestring: VARNAME must be a single row vector");
  elseif (position < 0)
    error ("validatestring: POSITION must be >= 0");
  endif

  ## Make static part of error string that uses funcname, varname, and position
  errstr = "";
  if (! isempty (funcname))
    errstr = [funcname ": "];
  endif
  if (! isempty (varname))
    errstr = [errstr varname " "];
  else
    errstr = sprintf ("%s'%s' ", errstr, str);
  endif
  if (position > 0)
    errstr = sprintf ("%s(argument #%i) ", errstr, position);
  endif

  matches = strncmpi (str, strarray(:), length (str));
  nmatches = sum (matches);
  if (nmatches == 0)
    error ("%sdoes not match any of\n%s", errstr,
           sprintf ("%s, ", strarray{:})(1:end-2));
  elseif (nmatches == 1)
    str = strarray{matches};
  else
    ## Are the matches substrings of each other?
    ## If true, choose the shortest.  If not, raise an error.
    match_idx = find (matches);
    match_len = cellfun ("length", strarray(match_idx));
    [min_len, min_idx] = min (match_len);
    short_str = strarray{match_idx(min_idx)};
    submatch = strncmpi (short_str, strarray(match_idx), min_len);
    if (all (submatch))
      str = short_str;
    else
      error ("%sallows multiple unique matches:\n%s",
             errstr, sprintf ("%s, ", strarray{match_idx})(1:end-2));
    endif
  endif

endfunction


%!shared strarray
%! strarray = {"octave" "Oct" "octopus" "octaves"};
%!assert (validatestring ("octave", strarray), "octave")
%!assert (validatestring ("oct", strarray), "Oct")
%!assert (validatestring ("octa", strarray), "octave")
%! strarray = {"abc1" "def" "abc2"};
%!assert (validatestring ("d", strarray), "def")

%!error <'xyz' does not match any> validatestring ("xyz", strarray)
%!error <DUMMY_TEST: 'xyz' does not> validatestring ("xyz", strarray, "DUMMY_TEST")
%!error <DUMMY_TEST: DUMMY_VAR does> validatestring ("xyz", strarray, "DUMMY_TEST", "DUMMY_VAR")
%!error <DUMMY_TEST: DUMMY_VAR \(argument #5\) does> validatestring ("xyz", strarray, "DUMMY_TEST", "DUMMY_VAR", 5)
%!error <'abc' allows multiple unique matches> validatestring ("abc", strarray)

## Test input validation
%!error validatestring ("xyz")
%!error validatestring ("xyz", {"xyz"}, "3", "4", 5, 6)
%!error <invalid number of character inputs> validatestring ("xyz", {"xyz"}, "3", "4", "5")
%!error <STR must be a character string> validatestring (1, {"xyz"}, "3", "4", 5)
%!error <STR must be a single row vector> validatestring ("xyz".', {"xyz"}, "3", "4", 5)
%!error <STRARRAY must be a cellstr> validatestring ("xyz", "xyz", "3", "4", 5)
%!error <FUNCNAME must be a single row vector> validatestring ("xyz", {"xyz"}, "33".', "4", 5)
%!error <VARNAME must be a single row vector> validatestring ("xyz", {"xyz"}, "3", "44".', 5)
%!error <POSITION must be> validatestring ("xyz", {"xyz"}, "3", "4", -5)

