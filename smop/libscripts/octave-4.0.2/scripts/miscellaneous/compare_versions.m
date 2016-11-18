## Copyright (C) 2006-2015 Bill Denney
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
## @deftypefn {Function File} {} compare_versions (@var{v1}, @var{v2}, @var{operator})
## Compare two version strings using the given @var{operator}.
##
## This function assumes that versions @var{v1} and @var{v2} are arbitrarily
## long strings made of numeric and period characters possibly followed by an
## arbitrary string (e.g., @qcode{"1.2.3"}, @qcode{"0.3"}, @qcode{"0.1.2+"},
## or @qcode{"1.2.3.4-test1"}).
##
## The version is first split into numeric and character portions and then
## the parts are padded to be the same length (i.e., @qcode{"1.1"} would be
## padded to be @qcode{"1.1.0"} when being compared with @qcode{"1.1.1"}, and
## separately, the character parts of the strings are padded with nulls).
##
## The operator can be any logical operator from the set
##
## @itemize @bullet
## @item
## @qcode{"=="}
## equal
##
## @item
## @qcode{"<"}
## less than
##
## @item
## @qcode{"<="}
## less than or equal to
##
## @item
## @qcode{">"}
## greater than
##
## @item
## @qcode{">="}
## greater than or equal to
##
## @item
## @qcode{"!="}
## not equal
##
## @item
## @qcode{"~="}
## not equal
## @end itemize
##
## Note that version @qcode{"1.1-test2"} will compare as greater than
## @qcode{"1.1-test10"}.  Also, since the numeric part is compared first,
## @qcode{"a"} compares less than @qcode{"1a"} because the second string
## starts with a numeric part even though @code{double ("a")} is greater than
## @code{double ("1").}
## @end deftypefn

## Author: Bill Denney <denney@seas.upenn.edu>

function out = compare_versions (v1, v2, operator)

  if (nargin != 3)
    print_usage ();
  endif

  ## Make sure that the version numbers are valid.
  if (! (ischar (v1) && ischar (v2)))
    error ("compare_versions: version numbers V1 and V2 must be strings");
  elseif (rows (v1) != 1 || rows (v2) != 1)
    error ("compare_versions: version numbers V1 and V2 must be a single row");
  endif

  ## check and make sure that the operator is valid
  if (! ischar (operator))
    error ("compare_versions: OPERATOR must be a string");
  elseif (numel (operator) > 2)
    error ("compare_versions: OPERATOR must be 1 or 2 characters long");
  endif

  ## trim off any character data that is not part of a normal version number
  v1firstchar = find (! (isdigit (v1) | v1 == "."), 1);
  v2firstchar = find (! (isdigit (v2) | v2 == "."), 1);

  if (isempty (v1firstchar))
    v1c = "";
    v1nochar = v1;
  else
    v1c = v1(v1firstchar:end);
    v1nochar = v1(1:v1firstchar-1);
  endif
  if (isempty (v2firstchar))
    v2c = "";
    v2nochar = v2;
  else
    v2c = v2(v2firstchar:end);
    v2nochar = v2(1:v2firstchar-1);
  endif

  v1n = str2double (ostrsplit (v1nochar, ".")');
  if (isnan (v1n))
    v1n = [];
  endif
  v2n = str2double (ostrsplit (v2nochar, ".")');
  if (isnan (v2n))
    v2n = [];
  endif

  if (isempty (v1n) && isempty (v1c))
    error ("compare_versions: version string V1 is not valid: %s", v1);
  elseif (isempty (v2n) && isempty (v2c))
    error ("compare_versions: version string V2 is not valid: %s", v2);
  endif

  ## Assume any additional elements would be 0 if one is longer than the other.
  maxnumlen = max ([length(v1n) length(v2n)]);
  v1n(end+1:maxnumlen) = 0;
  v2n(end+1:maxnumlen) = 0;

  ## Assume any additional character elements would be 0,
  ## if one is longer than the other.
  maxcharlen = max ([length(v1c), length(v2c)]);
  v1c(end+1:maxcharlen) = "\0";
  v2c(end+1:maxcharlen) = "\0";

  ## Determine the operator.
  equal_op = any (operator == "=");
  not_op = any (operator == "!" | operator == "~");
  lt_op = any (operator == "<");
  gt_op = any (operator == ">");

  ## Make sure that we don't have conflicting operators.
  if (gt_op && lt_op)
    error ("compare_versions: OPERATOR cannot contain both greater and less than symbols");
  elseif ((gt_op || lt_op) && not_op)
    error ("compare_versions: OPERATOR cannot contain not and greater than or less than symbols");
  elseif (strcmp (operator, "="))
    error ('compare_versions: equality OPERATOR is "==", not "="');
  elseif (! (equal_op || not_op || lt_op || gt_op))
    error ("compare_versions: no valid OPERATOR specified");
  endif

  ## Compare the versions (making sure that they're the same shape)
  vcmp = v1n(:) - v2n(:);
  vcmp = [vcmp; (v1c - v2c)(:)];
  if (lt_op)
    ## so that we only need to check for the output being greater than 1
    vcmp = -vcmp;
  endif
  firstdiff = find (vcmp, 1);

  if (isempty (firstdiff))
    ## They're equal.
    out = equal_op;
  elseif (lt_op || gt_op)
    ## They're correctly less than or greater than.
    out = (vcmp(firstdiff) > 0);
  else
    ## They're not correctly less than or greater than, and they're not equal.
    out = false;
  endif

  ## Reverse the output if not is given.
  if (not_op)
    out = !out;
  endif

endfunction


## tests
## test both equality symbols
## test arbitrarily long equality
%!assert (compare_versions ("1.1.0.0.0", "1.1", "=="), true)
%!assert (compare_versions ("1", "1.1", "<"), true)
%!assert (compare_versions ("1.1", "1.1", "<="), true)
%!assert (compare_versions ("1.1", "1.1.1", "<="), true)
%!assert (compare_versions ("1.23", "1.24", "=<"), true)
## test different length numbers
%!assert (compare_versions ("23.2000", "23.1", ">"), true)
%!assert (compare_versions ("0.0.2", "0.0.1", ">="), true)
%!assert (compare_versions ("0.2", "0.0.100", "=>"), true)
%!assert (compare_versions ("0.1", "0.2", "!="), true)
%!assert (compare_versions ("0.1", "0.2", "~="), true)

## test alphanumeric strings
%!assert (compare_versions ("1a", "1b", "<"), true)
%!assert (compare_versions ("a", "1", "<"), true)
%!assert (compare_versions ("1a", "1b", ">"), false)
%!assert (compare_versions ("a", "1", ">"), false)
%!assert (compare_versions ("1.1.0a", "1.1.0b", "=="), false)
%!assert (compare_versions ("1.1.0a", "1.1.0b", "!="), true)
%!assert (compare_versions ("1.1.0test", "1.1.0b", "=="), false)
%!assert (compare_versions ("1.1.0test", "1.1.0test", "=="), true)

## make sure that it won't just give true output
%!assert (compare_versions ("1", "0", "=="), false)
## test arbitrarily long equality
%!assert (compare_versions ("1.1.1.0.0", "1.1", "=="), false)
%!assert (compare_versions ("1.1", "1", "<"), false)
%!assert (compare_versions ("2", "1.1", "<="), false)
%!assert (compare_versions ("1.1.1", "1.1", "<="), false)
%!assert (compare_versions ("1.25", "1.24", "=<"), false)
## test different length numbers
%!assert (compare_versions ("23.2", "23.100", ">"), false)
%!assert (compare_versions ("0.0.0.2", "0.0.1", ">="), false)
%!assert (compare_versions ("0.0.20", "0.10.2", "=>"), false)
%!assert (compare_versions ("0.1", "0.1", "!="), false)
%!assert (compare_versions ("0.1", "0.1", "~="), false)

## Test input validation
%!error compare_versions ()
%!error compare_versions (1)
%!error compare_versions (1,2)
%!error compare_versions (1,2,3,4)
%!error <V1 and V2 must be strings> compare_versions (0.1, "0.1", "==")
%!error <V1 and V2 must be strings> compare_versions ("0.1", 0.1, "==")
%!error <V1 and V2 must be a single row> compare_versions (["0";".";"1"], "0.1", "==")
%!error <V1 and V2 must be a single row> compare_versions ("0.1", ["0";".";"1"], "==")
%!error <OPERATOR must be a string> compare_versions ("0.1", "0.1", 1)
%!error <OPERATOR must be 1 or 2> compare_versions ("0.1", "0.1", "==>")
%!error <V1 is not valid> compare_versions (".", "0.1", "==")
%!error <V2 is not valid> compare_versions ("0.1", ".", "==")

%!error <cannot contain both greater and less than> compare_versions ("0.1", "0.1", "<>")
%!error <cannot contain not and greater than> compare_versions ("0.1", "0.1", "!>")
%!error <cannot contain not and greater than> compare_versions ("0.1", "0.1", "!<")
%!error <equality OPERATOR is "=="> compare_versions ("0.1", "0.1", "=")
%!error <no valid OPERATOR> compare_versions ("0.1", "0.1", "aa")

