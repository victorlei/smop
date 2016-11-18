## Copyright (C) 2013-2015 Carnë Draug
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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} validateattributes (@var{A}, @var{classes}, @var{attributes})
## @deftypefnx {Function File} {} validateattributes (@var{A}, @var{classes}, @var{attributes}, @var{arg_idx})
## @deftypefnx {Function File} {} validateattributes (@var{A}, @var{classes}, @var{attributes}, @var{func_name})
## @deftypefnx {Function File} {} validateattributes (@var{A}, @var{classes}, @var{attributes}, @var{func_name}, @var{arg_name})
## @deftypefnx {Function File} {} validateattributes (@var{A}, @var{classes}, @var{attributes}, @var{func_name}, @var{arg_name}, @var{arg_idx})
## Check validity of input argument.
##
## Confirms that the argument @var{A} is valid by belonging to one of
## @var{classes}, and holding all of the @var{attributes}.  If it does not,
## an error is thrown, with a message formatted accordingly.  The error
## message can be made further complete by the function name @var{fun_name},
## the argument name @var{arg_name}, and its position in the input
## @var{arg_idx}.
##
## @var{classes} must be a cell array of strings (an empty cell array is
## allowed) with the name of classes (remember that a class name is case
## sensitive).  In addition to the class name, the following categories
## names are also valid:
##
## @table @asis
## @item @qcode{"float"}
## Floating point value comprising classes @qcode{"double"} and
## @qcode{"single"}.
##
## @item @qcode{"integer"}
## Integer value comprising classes (u)int8, (u)int16, (u)int32, (u)int64.
##
## @item @qcode{"numeric"}
## Numeric value comprising either a floating point or integer value.
##
## @end table
##
## @var{attributes} must be a cell array with names of checks for @var{A}.
## Some of them require an additional value to be supplied right after the
## name (see details for each below).
##
## @table @asis
## @item @qcode{"<="}
## All values are less than or equal to the following value in @var{attributes}.
##
## @item @qcode{"<"}
## All values are less than the following value in @var{attributes}.
##
## @item @qcode{">="}
## All values are greater than or equal to the following value in
## @var{attributes}.
##
## @item @qcode{">"}
## All values are greater than the following value in @var{attributes}.
##
## @item @qcode{"2d"}
## A 2-dimensional matrix.  Note that vectors and empty matrices have
## 2 dimensions, one of them being of length 1, or both length 0.
##
## @item @qcode{"3d"}
## Has no more than 3 dimensions.  A 2-dimensional matrix is a 3-D matrix
## whose 3rd dimension is of length 1.
##
## @item @qcode{"binary"}
## All values are either 1 or 0.
##
## @item @qcode{"column"}
## Values are arranged in a single column.
##
## @item @qcode{"decreasing"}
## No value is @var{NaN}, and each is less than the preceding one.
##
## @item @qcode{"even"}
## All values are even numbers.
##
## @item @qcode{"finite"}
## All values are finite.
##
## @item @qcode{"increasing"}
## No value is @var{NaN}, and each is greater than the preceding one.
##
## @item @qcode{"integer"}
## All values are integer.  This is different than using @code{isinteger}
## which only checks its an integer type.  This checks that each value in
## @var{A} is an integer value, i.e., it has no decimal part.
##
## @item @qcode{"ncols"}
## Has exactly as many columns as the next value in @var{attributes}.
##
## @item @qcode{"ndims"}
## Has exactly as many dimensions as the next value in @var{attributes}.
##
## @item @qcode{"nondecreasing"}
## No value is @var{NaN}, and each is greater than or equal to the preceding
## one.
##
## @item @qcode{"nonempty"}
## It is not empty.
##
## @item @qcode{"nonincreasing"}
## No value is @var{NaN}, and each is less than or equal to the preceding one.
##
## @item @qcode{"nonnan"}
## No value is a @code{NaN}.
##
## @item @qcode{"nonnegative"}
## All values are non negative.
##
## @item @qcode{"nonsparse"}
## It is not a sparse matrix.
##
## @item @qcode{"nonzero"}
## No value is zero.
##
## @item @qcode{"nrows"}
## Has exactly as many rows as the next value in @var{attributes}.
##
## @item @qcode{"numel"}
## Has exactly as many elements as the next value in @var{attributes}.
##
## @item @qcode{"odd"}
## All values are odd numbers.
##
## @item @qcode{"positive"}
## All values are positive.
##
## @item @qcode{"real"}
## It is a non-complex matrix.
##
## @item @qcode{"row"}
## Values are arranged in a single row.
##
## @item @qcode{"scalar"}
## It is a scalar.
##
## @item @qcode{"size"}
## Its size has length equal to the values of the next in @var{attributes}.
## The next value must is an array with the length for each dimension.  To
## ignore the check for a certain dimension, the value of @code{NaN} can be
## used.
##
## @item @qcode{"square"}
## Is a square matrix.
##
## @item @qcode{"vector"}
## Values are arranged in a single vector (column or vector).
##
## @end table
##
## @seealso{isa, validatestring, inputParser}
## @end deftypefn

function validateattributes (A, cls, attr, varargin)
  if (nargin < 3 || nargin > 6)
    print_usage ();
  elseif (! iscellstr (cls))
    error ("validateattributes: CLASSES must be a cell array of strings");
  elseif (! iscell (attr))
    error ("validateattributes: ATTRIBUTES must be a cell array");
  endif

  ## Built start of error message from the extra optional arguments
  func_name = "";
  var_name  = "input";
  if (nargin > 3)
    fourth = varargin{1};
    if (ischar (fourth))
      func_name = [fourth ": "];
    elseif (nargin == 4 && valid_arg_idx (fourth))
      var_name = sprintf ("input %d", fourth);
    else
      error ("validateattributes: 4th input argument must be ARG_IDX or FUNC_NAME");
    endif

    if (nargin > 4)
      var_name = varargin{2};
      if (! ischar (var_name))
        error ("validateattributes: VAR_NAME must be a string");
      endif

      if (nargin > 5)
        arg_idx = varargin{3};
        if (! valid_arg_idx (arg_idx))
          error ("validateattributes: ARG_IDX must be a positive integer");
        endif
        var_name = sprintf ("%s (argument #%i)", var_name, arg_idx);
      endif
    endif
  endif
  err_ini = [func_name var_name];

  check_cl = isa (A, cls);
  if (! isempty (check_cl) && ! any (check_cl))
    ## Allowing for an empty list of classes is Matlab incompatible but
    ## that should count as a just a Matlab bug, not an incompatibility.

    ## Replace the category names with the classes that belong to it.
    integer = { "int8"  "int16"  "int32"  "int64" ...
               "uint8" "uint16" "uint32" "uint64"};
    float   = {"single" "double"};
    numeric = {integer{:} float{:}};
    cls = replace_cl_group (cls, "integer", integer);
    cls = replace_cl_group (cls, "float",   float  );
    cls = replace_cl_group (cls, "numeric", numeric);
    cls = unique (cls);

    classes = sprintf (" %s", cls{:});
    error ("%s must be of class:\n\n %s\n\nbut was of class %s",
           err_ini, classes, class (A));
  endif

  ## We use a while loop because some attributes require the following value
  ## in the cell array. Also, we can't just get the boolean value for the
  ## test and check at the end the error message since some of the tests
  ## require some more complex error message.

  ## It may look like that we don't perform enough input check in this
  ## function (e.g., we don't check if there's a value after the size
  ## attribute). The reasoning is that this will be a function mostly used
  ## by developers with fairly static input so any problem would be caught
  ## immediately during that functino development, it's no dependent on the
  ## final user input. In addition, it can be called so many times at the
  ## start of every function, we want it to run specially fast.
  idx = 1;
  problem = false; # becomes true when one of the tests fails
  while (idx <= numel (attr))
    ## TODO: once we use this in Octave core, it might be worthy to find
    ## which attributes are checked more often, and place them in that
    ## order inside the switch block.
    switch (tolower (attr{idx++}))
      case "2d",            problem = ndims (A) != 2;
      case "3d",            problem = ndims (A) > 3;
      case "column",        problem = ! iscolumn (A);
      case "row",           problem = ! isrow (A);
      case "scalar",        problem = ! isscalar (A);
      case "vector",        problem = ! isvector (A);
      case "square",        problem = ! issquare (A);
      case "nonempty",      problem = isempty (A);
      case "nonsparse",     problem = issparse (A);
      case "binary",        problem = ! islogical (A) && ...
                                      any ((A(:) != 1) & (A(:) != 0));
      case "even",          problem = any (rem (A(:), 2) != 0);
      case "odd",           problem = any (mod (A(:), 2) != 1);
      case "integer",       problem = ! isinteger (A) && ...
                                      any (ceil (A(:)) != A(:));
      case "real",          problem = ! isreal (A);
      case "finite",        problem = ! isinteger (A) && ...
                                      ! all (isfinite (A(:)));
      case "nonnan",        problem = ! isinteger (A) && ...
                                      any (isnan (A(:)));
      case "nonnegative",   problem = any (A(:) < 0);
      case "nonzero",       problem = any (A(:) == 0);
      case "positive",      problem = any (A(:) <= 0);
      case "decreasing",    problem = (any (isnan (A(:)))
                                       || any (diff (A(:)) >= 0));
      case "increasing",    problem = (any (isnan (A(:)))
                                       || any (diff (A(:)) <= 0));
      case "nondecreasing", problem = (any (isnan (A(:)))
                                       || any (diff (A(:)) <  0));
      case "nonincreasing", problem = (any (isnan (A(:)))
                                       || any (diff (A(:)) >  0));
      case "size",
        A_size = size (A);
        w_size = attr{idx++};
        A_size(isnan (w_size)) = NaN;
        if (! isequaln (A_size, w_size))
          A_size_str = sprintf ("%dx", size (A))(1:end-1);
          w_size_str = sprintf ("%ix", w_size)(1:end-1);
          w_size_str = strrep (w_size_str, "NaN", "N");
          error ("%s must be of size %s but was %s", err_ini, w_size_str, A_size_str);
        endif
      case "numel",
        if (numel (A) != attr{idx++})
          error ("%s must have %d elements", err_ini, attr{idx-1});
        endif
      case "ncols",
        if (columns (A) != attr{idx++})
          error ("%s must have %d columns", err_ini, attr{idx-1});
        endif
      case "nrows",
        if (rows (A) != attr{idx++})
          error ("%s must have %d rows", err_ini, attr{idx-1});
        endif
      case "ndims",
        ## Note that a [4 5 1] matrix is not considered to have ndims == 3
        ## but is ok for "3d". This is not a bug.
        if (ndims (A) != attr{idx++})
          error ("%s must have %d dimensions", err_ini, attr{idx-1});
        endif
      case ">"
        if (! all (A(:) > attr{idx++}))
          error ("%s must be greater than %f", err_ini, attr{idx-1});
        endif
      case ">="
        if (! all (A(:) >= attr{idx++}))
          error ("%s must be greater than or equal to %f", err_ini, attr{idx-1});
        endif
      case "<"
        if (! all (A(:) < attr{idx++}))
          error ("%s must be less than %f", err_ini, attr{idx-1});
        endif
      case "<="
        if (! all (A(:) <= attr{idx++}))
          error ("%s must be less than or equal to %f", err_ini, attr{idx-1});
        endif
      otherwise
        error ("validateattributes: unknown ATTRIBUTE %s", attr{idx-1});
    endswitch
    if (problem)
      error ("%s must be %s", err_ini, attr{idx-1});
    endif
  endwhile
endfunction

function retval = valid_arg_idx (arg)
  retval = isnumeric (arg) && isscalar (arg) && arg > 0 && arg == fix (arg);
endfunction

function cls = replace_cl_group (cls, name, group)
  num_pos = strcmpi (cls, name);
  if (any (num_pos))
    cls(num_pos) = [];
    cls(end+1:end+numel(group)) = group;
  endif
endfunction


%!error <double> validateattributes (rand (5), {"uint8"}, {})
%!error <single> validateattributes (uint8 (rand (5)), {"float"}, {})
%!error <2d> validateattributes (rand (5, 5, 5), {}, {"2d"})
%!error <3d> validateattributes (rand (5, 5, 5, 7), {}, {"3d"})
%!error <column> validateattributes (rand (5, 5), {}, {"column"})
%!error <column> validateattributes (rand (1, 5), {}, {"column"})
%!error <row> validateattributes (rand (5, 5), {}, {"row"})
%!error <row> validateattributes (rand (5, 1), {}, {"row"})
%!error <scalar> validateattributes (rand (1, 5), {}, {"scalar"})
%!error <vector> validateattributes (rand (5), {}, {"vector"})
%!error <square> validateattributes (rand (5, 6), {}, {"square"})
%!error <nonempty> validateattributes ([], {}, {"nonempty"})
%!error <nonsparse> validateattributes (sparse(rand(5)), {}, {"nonsparse"})
%!error <binary> validateattributes ("text", {}, {"binary"})
%!error <binary> validateattributes ([0 1 0 3 0], {}, {"binary"})
%!error <even> validateattributes ([2 3 6 8], {}, {"even"})
%!error <even> validateattributes ([2 NaN], {}, {"even"})
%!error <odd> validateattributes ([3 4 7 5], {}, {"odd"})
%!error <odd> validateattributes ([5 NaN], {}, {"odd"})
%!error <integer> validateattributes ([5 5.2 5.7], {}, {"integer"})
%!error <real> validateattributes ([5i 8 9], {}, {"real"})
%!error <finite> validateattributes ([5i Inf 8], {}, {"finite"})
%!error <nonnan> validateattributes ([NaN Inf 8], {}, {"nonnan"})
%!error <nonnegative> validateattributes ([7 8 -9], {}, {"nonnegative"})
%!error <nonzero> validateattributes ([7 8 0], {}, {"nonzero"})
%!error <positive> validateattributes ([7 0 8], {}, {"positive"})
%!error <decreasing> validateattributes ([7 8 4 3 -5], {}, {"decreasing"})
%!error <decreasing> validateattributes ([7 NaN 4 3 -5], {}, {"decreasing"})
%!error <increasing> validateattributes ([7 8 4 9 20], {}, {"increasing"})
%!error <increasing> validateattributes ([7 8 NaN 9 20], {}, {"increasing"})
%!error <nonincreasing> validateattributes ([7 8 4 9 20], {}, {"nonincreasing"})
%!error <nonincreasing> validateattributes ([7 8 NaN 9 20], {}, {"nonincreasing"})
%!error <nondecreasing> validateattributes ([7 8 4 3 -5], {}, {"nondecreasing"})
%!error <nondecreasing> validateattributes ([7 NaN 4 3 -5], {}, {"nondecreasing"})
%!error <size> validateattributes (ones (5, 3, 6), {}, {"size", [5 4 7]})
%!error <size> validateattributes (ones (5, 3, 6), {}, {"size", [5 NaN 7]})
%!error <size> validateattributes (ones (5, 3, 6), {}, {"size", [5 3 6 2]})
%!error <elements> validateattributes (ones (6, 3), {}, {"numel", 12})
%!error <columns> validateattributes (ones (6, 2), {}, {"ncols", 3})
%!error <rows> validateattributes (ones (6, 2), {}, {"nrows", 3})
%!error <dimensions> validateattributes (ones (6, 2, 6, 3), {}, {"ndims", 3})
%!error <greater than> validateattributes ([6 7 8 5], {}, {">", 5})
%!error <greater than> validateattributes ([6 7 8 5], {}, {">=", 6})
%!error <less than> validateattributes ([6 7 8 5], {}, {"<", 8})
%!error <less than> validateattributes ([6 7 8 5], {}, {"<=", 7})

%!test
%! validateattributes (rand (5), {"numeric"}, {})
%! validateattributes (rand (5), {"float"}, {})
%! validateattributes (rand (5), {"double"}, {})
%! validateattributes ("text", {"char"}, {})
%! validateattributes (rand (5), {}, {"2d"})
%! validateattributes (rand (5), {}, {"3d"})
%! validateattributes (rand (5, 5, 5), {}, {"3d"})
%! validateattributes (rand (5, 1), {}, {"column"})
%! validateattributes (rand (1, 5), {}, {"row"})
%! validateattributes ("a", {}, {"scalar"})
%! validateattributes (5, {}, {"scalar"})
%! validateattributes (rand (1, 5), {}, {"vector"})
%! validateattributes (rand (5, 1), {}, {"vector"})
%! validateattributes (rand (5), {}, {"square"})
%! validateattributes (rand (5), {}, {"nonempty"})
%! validateattributes (rand (5), {}, {"nonsparse"})
%! validateattributes ([0 1 0 1 0], {}, {"binary"})
%! validateattributes (rand (5) > 0.5, {}, {"binary"})
%! validateattributes ([8 4 0 6], {}, {"even"})
%! validateattributes ([-1 3 5], {}, {"odd"})
%! validateattributes ([8 4 0 6], {}, {"real"})
%! validateattributes ([8 4i 0 6], {}, {"finite"})
%! validateattributes (uint8 ([8 4]), {}, {"finite"})
%! validateattributes ([8 Inf], {}, {"nonnan"})
%! validateattributes ([0 7 4], {}, {"nonnegative"})
%! validateattributes ([-8 7 4], {}, {"nonzero"})
%! validateattributes ([8 7 4], {}, {"positive"})
%! validateattributes ([8 7 4 -5], {}, {"decreasing"})
%! validateattributes ([-8 -7 4 5], {}, {"increasing"})
%! validateattributes ([8 4 4 -5], {}, {"nonincreasing"})
%! validateattributes ([-8 -8 4 5], {}, {"nondecreasing"})
%! validateattributes (rand (4, 6, 7, 2), {}, {"size", [4 6 7 2]})
%! validateattributes (rand (4, 6, 7, 2), {}, {"size", [4 NaN 7 2]})
%! validateattributes (rand (4, 6, 7, 2), {}, {"size", [4 6 NaN 2 NaN]})
%! validateattributes (rand (6, 2), {}, {"numel", 12})
%! validateattributes (rand (6, 2), {}, {"ncols", 2})
%! validateattributes (rand (6, 2), {}, {"nrows", 6})
%! validateattributes (rand (6, 2, 4, 5), {}, {"ndims", 4})
%! validateattributes ([4 5 6 7], {}, {">", 3})
%! validateattributes ([4 5 6 7], {}, {">=", 4})
%! validateattributes ([4 5 6 7], {}, {"<", 8})
%! validateattributes ([4 5 6 7], {}, {"<=", 7})

%!test
%! validateattributes ([0 1 0 1], {"double", "uint8"}, {"binary", "size", [NaN 4], "nonnan"})
