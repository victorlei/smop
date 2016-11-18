## Copyright (C) 2008-2015 Bill Denney, Robert Platt
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
## @deftypefn  {Function File} {@var{varname} =} genvarname (@var{str})
## @deftypefnx {Function File} {@var{varname} =} genvarname (@var{str}, @var{exclusions})
## Create valid unique variable name(s) from @var{str}.
##
## If @var{str} is a cellstr, then a unique variable is created for each cell
## in @var{str}.
##
## @example
## @group
## genvarname (@{"foo", "foo"@})
##   @result{}
##      @{
##        [1,1] = foo
##        [1,2] = foo1
##      @}
## @end group
## @end example
##
## If @var{exclusions} is given, then the variable(s) will be unique to each
## other and to @var{exclusions} (@var{exclusions} may be either a string or a
## cellstr).
##
## @example
## @group
## x = 3.141;
## genvarname ("x", who ())
##   @result{} x1
## @end group
## @end example
##
## Note that the result is a char array or cell array of strings, not the
## variables themselves.  To define a variable, @code{eval()} can be used.
## The following trivial example sets @code{x} to @code{42}.
##
## @example
## @group
## name = genvarname ("x");
## eval ([name " = 42"]);
##   @result{} x =  42
## @end group
## @end example
##
## This can be useful for creating unique struct field names.
##
## @example
## @group
## x = struct ();
## for i = 1:3
##   x.(genvarname ("a", fieldnames (x))) = i;
## endfor
##   @result{} x =
##      @{
##        a =  1
##        a1 =  2
##        a2 =  3
##      @}
## @end group
## @end example
##
## Since variable names may only contain letters, digits, and underscores,
## @code{genvarname} will replace any sequence of disallowed characters with
## an underscore.  Also, variables may not begin with a digit; in this case
## an @samp{x} is added before the variable name.
##
## Variable names beginning and ending with two underscores @qcode{"__"} are
## valid, but they are used internally by Octave and should generally be
## avoided; therefore, @code{genvarname} will not generate such names.
##
## @code{genvarname} will also ensure that returned names do not clash with
## keywords such as @qcode{"for"} and @qcode{"if"}.  A number will be
## appended if necessary.  Note, however, that this does @strong{not} include
## function names such as @qcode{"sin"}.  Such names should be included in
## @var{exclusions} if necessary.
## @seealso{isvarname, iskeyword, exist, who, tempname, eval}
## @end deftypefn

## Authors: Rob Platt <robert.platt@postgrad.manchester.ac.uk>
##          Bill Denney <bill@denney.ws>

function varname = genvarname (str, exclusions = {})

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  strinput = ischar (str);
  ## Process the inputs
  if (strinput)
    if (rows (str) != 1)
      error ("genvarname: if more than one STR is given, it must be a cellstr");
    endif
    str = {str};
  elseif (! iscellstr (str))
    error ("genvarname: STR must be a string or cellstr");
  endif

  if (ischar (exclusions))
    if (rows (exclusions) != 1)
      error ("genvarname: if more than one exclusion is given, it must be a cellstr");
    endif
    exclusions = {exclusions};
  elseif (! iscellstr (exclusions))
    error ("genvarname: EXCLUSIONS must be a string or cellstr");
  else
    exclusions = exclusions(:);
  endif

  varname = cell (size (str));
  for i = 1:numel (str)
    ## Perform any modifications to the varname to make sure that it is
    ## a valid variable name.

    ## remove invalid characters
    str{i}(! (isalnum (str{i}) | str{i} == "_")) = "_";
    ## do not use keywords
    if (iskeyword (str{i}))
      firstcharacter = toupper (str{i}(1));
      str{i} = ["x", firstcharacter, str{i}(2:end)];
    endif
    ## The variable cannot be empty
    if (isempty (str{i}))
      str{i} = "x";
    endif
    ## Leading underscores are not Matlab compatible
    if (str{i}(1) == "_")
      str{i} = ["x", str{i}];
    endif
    ## it cannot start with a number
    if (isdigit (str{i}(1)))
      str{i} = ["x", str{i}];
    endif

    ## make sure that the variable is unique relative to other variables
    ## and the exclusions list
    excluded = any (strcmp (str{i}, exclusions));
    if (excluded && isdigit (str{i}(end)))
      ## if it is not unique and ends with a digit, add an underscore to
      ## make the variable name more readable ("x1_1" instead of "x11")
      str{i} = [str{i}, "_"];
    endif
    varname(i) = str(i);
    idx = 0;
    while (excluded)
      idx++;
      varname{i} = sprintf ("%s%d", str{i}, idx);
      excluded = any (strcmp (varname{i}, exclusions));
    endwhile
    exclusions(end+1) = varname(i);
  endfor

  if (strinput)
    varname = varname{1};
  endif

endfunction


## a single argument
%!assert (genvarname ("a"), "a")
## a single argument with a non-conflicting exception
%!assert (genvarname ("a", "b"), "a")
## a single argument with a conflicting exception
%!assert (genvarname ("a", "a"), "a1")
## a single argument as a cell
%!assert (genvarname ({"a"}), {"a"})
%!assert (genvarname ({"a"}, "b"), {"a"})
%!assert (genvarname ({"a"}, {"b"}), {"a"})
%!assert (genvarname ({"a"}, "a"), {"a1"})
%!assert (genvarname ({"a"}, {"a"}), {"a1"})
## Test different arguments
## orientation
%!assert (genvarname ({"a" "b"}), {"a" "b"})
%!assert (genvarname ({"a";"b"}), {"a";"b"})
%!assert (genvarname ({"a" "a"}), {"a" "a1"})
%!assert (genvarname ({"a" "b";"c" "d"}), {"a" "b";"c" "d"})
%!assert (genvarname ({"a" "a" "a";"a" "a" "a"}), {"a" "a2" "a4";"a1" "a3" "a5"})
## more than one repetition
%!assert (genvarname ({"a" "a" "a"}), {"a" "a1" "a2"})
%!assert (genvarname ({"a" "a" "a"}, {"a" "a1" "a2"}), {"a3" "a4" "a5"})
## more than one repetition not in order
%!assert (genvarname ({"a" "b" "a" "b" "a"}), {"a" "b" "a1" "b1" "a2"})
## Variable name munging
%!assert (genvarname ("__x__"), "x__x__")
%!assert (genvarname ("123456789"), "x123456789")
%!assert (genvarname ("_$1__"), "x__1__")
%!assert (genvarname ("__foo__", "x__foo__"), "x__foo__1")
%!assert (genvarname ("1million_and1", "x1million_and1"), "x1million_and1_1")
%!assert (genvarname ({"", "", ""}), {"x", "x1", "x2"})
%!assert (genvarname ("if"), "xIf")
%!assert (genvarname ({"if", "if", "if"}), {"xIf", "xIf1", "xIf2"})
## Exclusions in odd format
%!assert (genvarname ("x", {"a", "b"; "x", "d"}), "x1")

## Test input validation
%!error genvarname ()
%!error genvarname (1,2,3)
%!error <more than one STR is given, it must be a cellstr> genvarname (char ("a", "b", "c"))
%!error <STR must be a string or cellstr> genvarname (1)
%!error <more than one exclusion is given, it must be a cellstr> genvarname ("x", char ("a", "b", "c"))
%!error <EXCLUSIONS must be a string or cellstr> genvarname ("x", 1)

