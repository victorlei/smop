## Copyright (C) 2000-2015 Paul Kienzle
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
## @deftypefn  {Function File} {} assert (@var{cond})
## @deftypefnx {Function File} {} assert (@var{cond}, @var{errmsg})
## @deftypefnx {Function File} {} assert (@var{cond}, @var{errmsg}, @dots{})
## @deftypefnx {Function File} {} assert (@var{cond}, @var{msg_id}, @var{errmsg}, @dots{})
## @deftypefnx {Function File} {} assert (@var{observed}, @var{expected})
## @deftypefnx {Function File} {} assert (@var{observed}, @var{expected}, @var{tol})
##
## Produce an error if the specified condition is not met.
##
## @code{assert} can be called in three different ways.
##
## @table @code
## @item  assert (@var{cond})
## @itemx assert (@var{cond}, @var{errmsg})
## @itemx assert (@var{cond}, @var{errmsg}, @dots{})
## @itemx assert (@var{cond}, @var{msg_id}, @var{errmsg}, @dots{})
## Called with a single argument @var{cond}, @code{assert} produces an error if
## @var{cond} is false (numeric zero).
##
## Any additional arguments are passed to the @code{error} function for
## processing.
##
## @item assert (@var{observed}, @var{expected})
## Produce an error if observed is not the same as expected.
##
## Note that @var{observed} and @var{expected} can be scalars, vectors,
## matrices, strings, cell arrays, or structures.
##
## @item assert (@var{observed}, @var{expected}, @var{tol})
## Produce an error if observed is not the same as expected but equality
## comparison for numeric data uses a tolerance @var{tol}.
##
## If @var{tol} is positive then it is an absolute tolerance which will produce
## an error if @code{abs (@var{observed} - @var{expected}) > abs (@var{tol})}.
##
## If @var{tol} is negative then it is a relative tolerance which will produce
## an error if @code{abs (@var{observed} - @var{expected}) >
## abs (@var{tol} * @var{expected})}.
##
## If @var{expected} is zero @var{tol} will always be interpreted as an
## absolute tolerance.
##
## If @var{tol} is not scalar its dimensions must agree with those of
## @var{observed} and @var{expected} and tests are performed on an
## element-by-element basis.
## @end table
## @seealso{fail, test, error, isequal}
## @end deftypefn

function assert (cond, varargin)

  if (nargin == 0)
    print_usage ();
  endif

  persistent call_depth = -1;
  persistent errmsg;

  unwind_protect

    call_depth++;

    if (call_depth == 0)
      errmsg = "";
    endif

    if (nargin == 1 || (nargin > 1 && islogical (cond) && ischar (varargin{1})))
      if ((! isnumeric (cond) && ! islogical (cond))
          || isempty (cond) || ! all (cond(:)))
        if (nargin == 1)
          ## Perhaps, say which elements failed?
          argin = ["(" strjoin(cellstr (argn), ",") ")"];
          error ("assert %s failed", argin);
        else
          error (varargin{:});
        endif
      endif
    else
      expected = varargin{1};
      if (nargin < 3)
        tol = 0;
      elseif (nargin == 3)
        tol = varargin{2};
      else
        print_usage ();
      endif

      ## Add to list as the errors accumulate.  If empty at end then no errors.
      err.index = {};
      err.observed = {};
      err.expected = {};
      err.reason = {};

      if (ischar (expected))
        if (! ischar (cond))
          err.index{end+1} = ".";
          err.expected{end+1} = expected;
          if (isnumeric (cond))
            err.observed{end+1} = num2str (cond);
            err.reason{end+1} = "Expected string, but observed number";
          else
            err.observed{end+1} = "O";
            err.reason{end+1} = ["Expected string, but observed " class(cond)];
          endif
        elseif (! strcmp (cond, expected))
          err.index{end+1} = "[]";
          err.observed{end+1} = cond;
          err.expected{end+1} = expected;
          err.reason{end+1} = "Strings don't match";
        endif

      elseif (iscell (expected))
        if (! iscell (cond))
          err.index{end+1} = ".";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          err.reason{end+1} = ["Expected cell, but observed " class(cond)];
        elseif (ndims (cond) != ndims (expected)
                || any (size (cond) != size (expected)))
          err.index{end+1} = ".";
          err.observed{end+1} = ["O(" sprintf("%dx", size(cond))(1:end-1) ")"];
          err.expected{end+1} = ["E(" sprintf("%dx", size(expected))(1:end-1) ")"];
          err.reason{end+1} = "Dimensions don't match";
        else
          try
            ## Recursively compare cell arrays
            for i = 1:length (expected(:))
              assert (cond{i}, expected{i}, tol);
            endfor
          catch
            err.index{end+1} = "{}";
            err.observed{end+1} = "O";
            err.expected{end+1} = "E";
            err.reason{end+1} = "Cell configuration error";
          end_try_catch
        endif

      elseif (is_function_handle (expected))
        if (! is_function_handle (cond))
          err.index{end+1} = "@";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          err.reason{end+1} = ["Expected function handle, but observed " class(cond)];
        elseif (! isequal (cond, expected))
          err.index{end+1} = "@";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          err.reason{end+1} = "Function handles don't match";
        endif

      elseif (isstruct (expected))
        if (! isstruct (cond))
          err.index{end+1} = ".";
          err.observed{end+1} = "O";
          err.expected{end+1} = "E";
          err.reason{end+1} = ["Expected struct, but observed " class(cond)];
        elseif (ndims (cond) != ndims (expected)
                || any (size (cond) != size (expected))
                || numfields (cond) != numfields (expected))

          err.index{end+1} = ".";
          err.observed{end+1} = ["O(" sprintf("%dx", size(cond))(1:end-1) ")"];
          err.expected{end+1} = ["E(" sprintf("%dx", size(expected))(1:end-1) ")"];
          err.reason{end+1} = "Structure sizes don't match";
        else
          try
            empty = isempty (cond);
            normal = (numel (cond) == 1);
            for [v, k] = cond
              if (! isfield (expected, k))
                err.index{end+1} = ".";
                err.observed{end+1} = "O";
                err.expected{end+1} = "E";
                err.reason{end+1} = ["'" k "'" " is not an expected field"];
              endif
              if (empty)
                v = {};
              elseif (normal)
                v = {v};
              else
                v = v(:)';
              endif
              ## Recursively call assert for struct array values
              assert (v, {expected.(k)}, tol);
            endfor
          catch
            err.index{end+1} = ".";
            err.observed{end+1} = "O";
            err.expected{end+1} = "E";
            err.reason{end+1} = "Structure configuration error";
          end_try_catch
        endif

      elseif (ndims (cond) != ndims (expected)
              || any (size (cond) != size (expected)))
        err.index{end+1} = ".";
        err.observed{end+1} = ["O(" sprintf("%dx", size(cond))(1:end-1) ")"];
        err.expected{end+1} = ["E(" sprintf("%dx", size(expected))(1:end-1) ")"];
        err.reason{end+1} = "Dimensions don't match";

      else  # Numeric comparison
        if (nargin < 3)
          ## Without explicit tolerance, be more strict.
          if (! strcmp (class (cond), class (expected)))
            err.index{end+1} = "()";
            err.observed{end+1} = "O";
            err.expected{end+1} = "E";
            err.reason{end+1} = ["Class " class(cond) " != " class(expected)];
          elseif (isnumeric (cond))
            if (issparse (cond) != issparse (expected))
              err.index{end+1} = "()";
              err.observed{end+1} = "O";
              err.expected{end+1} = "E";
              if (issparse (cond))
                err.reason{end+1} = "sparse != non-sparse";
              else
                err.reason{end+1} = "non-sparse != sparse";
              endif
            elseif (iscomplex (cond) != iscomplex (expected))
              err.index{end+1} = "()";
              err.observed{end+1} = "O";
              err.expected{end+1} = "E";
             if (iscomplex (cond))
                err.reason{end+1} = "complex != real";
              else
                err.reason{end+1} = "real != complex";
              endif
            endif
          endif
        endif

        if (isempty (err.index))

          A = cond;
          B = expected;

          ## Check exceptional values.
          errvec = (  isna (real (A)) != isna (real (B))
                    | isna (imag (A)) != isna (imag (B)));
          erridx = find (errvec);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              repmat ({"'NA' mismatch"}, length (erridx), 1);
          endif
          errseen = errvec;

          errvec = (  isnan (real (A)) != isnan (real (B))
                    | isnan (imag (A)) != isnan (imag (B)));
          erridx = find (errvec & !errseen);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              repmat ({"'NaN' mismatch"}, length (erridx), 1);
          endif
          errseen |= errvec;

          errvec =   ((isinf (real (A)) | isinf (real (B))) ...
                      & (real (A) != real (B)))             ...
                   | ((isinf (imag (A)) | isinf (imag (B))) ...
                      & (imag (A) != imag (B)));
          erridx = find (errvec & !errseen);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              repmat ({"'Inf' mismatch"}, length (erridx), 1);
          endif
          errseen |= errvec;

          ## Check normal values.
          ## Replace exceptional values already checked above by zero.
          A_null_real = real (A);
          B_null_real = real (B);
          exclude = errseen ...
                    | ! isfinite (A_null_real) & ! isfinite (B_null_real);
          A_null_real(exclude) = 0;
          B_null_real(exclude) = 0;
          A_null_imag = imag (A);
          B_null_imag = imag (B);
          exclude = errseen ...
                    | ! isfinite (A_null_imag) & ! isfinite (B_null_imag);
          A_null_imag(exclude) = 0;
          B_null_imag(exclude) = 0;
          A_null = complex (A_null_real, A_null_imag);
          B_null = complex (B_null_real, B_null_imag);
          if (isscalar (tol))
            mtol = tol * ones (size (A));
          else
            mtol = tol;
          endif

          k = (mtol == 0);
          erridx = find ((A_null != B_null) & k);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              ostrsplit (deblank (sprintf ("Abs err %.5g exceeds tol %.5g\n",...
              [abs(A_null(erridx) - B_null(erridx))(:) mtol(erridx)(:)]')), "\n");
          endif

          k = (mtol > 0);
          erridx = find ((abs (A_null - B_null) > mtol) & k);
          if (! isempty (erridx))
            err.index(end+1:end+length (erridx)) = ...
              ind2tuple (size (A), erridx);
            err.observed(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (A(erridx) (:))));
            err.expected(end+1:end+length (erridx)) = ...
              strtrim (cellstr (num2str (B(erridx) (:))));
            err.reason(end+1:end+length (erridx)) = ...
              ostrsplit (deblank (sprintf ("Abs err %.5g exceeds tol %.5g\n",...
              [abs(A_null(erridx) - B_null(erridx))(:) mtol(erridx)(:)]')), "\n");
          endif

          k = (mtol < 0);
          if (any (k(:)))
            ## Test for absolute error where relative error can't be calculated.
            erridx = find ((B_null == 0) & abs (A_null) > abs (mtol) & k);
            if (! isempty (erridx))
              err.index(end+1:end+length (erridx)) = ...
                ind2tuple (size (A), erridx);
              err.observed(end+1:end+length (erridx)) = ...
                strtrim (cellstr (num2str (A(erridx) (:))));
              err.expected(end+1:end+length (erridx)) = ...
                strtrim (cellstr (num2str (B(erridx) (:))));
              err.reason(end+1:end+length (erridx)) = ...
                ostrsplit (deblank (sprintf ("Abs err %.5g exceeds tol %.5g\n",
                [abs(A_null(erridx) - B_null(erridx)) -mtol(erridx)]')), "\n");
            endif
            ## Test for relative error
            Bdiv = Inf (size (B_null));
            Bdiv(k & (B_null != 0)) = B_null(k & (B_null != 0));
            relerr = abs ((A_null - B_null) ./ abs (Bdiv));
            erridx = find ((relerr > abs (mtol)) & k);
            if (! isempty (erridx))
              err.index(end+1:end+length (erridx)) = ...
                ind2tuple (size (A), erridx);
              err.observed(end+1:end+length (erridx)) = ...
                strtrim (cellstr (num2str (A(erridx) (:))));
              err.expected(end+1:end+length (erridx)) = ...
                strtrim (cellstr (num2str (B(erridx) (:))));
              err.reason(end+1:end+length (erridx)) = ...
                ostrsplit (deblank (sprintf ("Rel err %.5g exceeds tol %.5g\n",
                [relerr(erridx)(:) -mtol(erridx)(:)]')), "\n");
            endif
          endif
        endif

      endif

      ## Print any errors
      if (! isempty (err.index))
        argin = ["(" strjoin(cellstr (argn), ",") ")"];
        if (! isempty (errmsg))
          errmsg = [errmsg "\n"];
        endif
        errmsg = [errmsg, pprint(argin, err)];
      endif

    endif

  unwind_protect_cleanup
    call_depth--;
  end_unwind_protect

  if (call_depth == -1)
    ## Last time through.  If there were any errors on any pass, raise a flag.
    if (! isempty (errmsg))
      error (errmsg);
    endif
  endif

endfunction


## empty input
%!error assert ([])
%!error assert ("")
%!error assert ({})
%!error assert (struct ([]))
%!assert (zeros (3,0), zeros (3,0))
%!error <O\(3x0\)\s+E\(0x2\)> assert (zeros (3,0), zeros (0,2))
%!error <Dimensions don't match> assert (zeros (3,0), [])
%!error <Dimensions don't match> assert (zeros (2,0,2), zeros (2,0))

## conditions
%!assert (isempty ([]))
%!assert (1)
%!error assert (0)
%!assert (ones (3,1))
%!assert (ones (1,3))
%!assert (ones (3,4))
%!error assert ([1,0,1])
%!error assert ([1;1;0])
%!error assert ([1,0;1,1])
%!error <2-part error> assert (false, "%s %s", "2-part", "error")
%!error <2-part error> assert (false, "TST:msg_id", "%s %s", "2-part", "error")

## scalars
%!error <Dimensions don't match> assert (3, [3,3])
%!error <Dimensions don't match> assert (3, [3,3; 3,3])
%!error <Dimensions don't match> assert ([3,3; 3,3], 3)
%!assert (3, 3)
%!error <Abs err 1 exceeds tol> assert (3, 4)
%!assert (3+eps, 3, eps)
%!assert (3, 3+eps, eps)
%!error <Abs err 4.4409e-0?16 exceeds tol> assert (3+2*eps, 3, eps)
%!error <Abs err 4.4409e-0?16 exceeds tol> assert (3, 3+2*eps, eps)

## vectors
%!assert ([1,2,3],[1,2,3]);
%!assert ([1;2;3],[1;2;3]);
%!error <Abs err 1 exceeds tol 0> assert ([2,2,3,3],[1,2,3,4]);
%!error <Abs err 1 exceeds tol 0.5> assert ([2,2,3,3],[1,2,3,4],0.5);
%!error <Rel err 1 exceeds tol 0.1> assert ([2,2,3,5],[1,2,3,4],-0.1);
%!error <Abs err 1 exceeds tol 0> assert ([6;6;7;7],[5;6;7;8]);
%!error <Abs err 1 exceeds tol 0.5> assert ([6;6;7;7],[5;6;7;8],0.5);
%!error <Rel err .* exceeds tol 0.1> assert ([6;6;7;7],[5;6;7;8],-0.1);
%!error <Dimensions don't match> assert ([1,2,3],[1;2;3]);
%!error <Dimensions don't match> assert ([1,2],[1,2,3]);
%!error <Dimensions don't match> assert ([1;2;3],[1;2]);

## matrices
%!assert ([1,2;3,4],[1,2;3,4]);
%!error <\(1,2\)\s+4\s+2> assert ([1,4;3,4],[1,2;3,4])
%!error <Dimensions don't match> assert ([1,3;2,4;3,5],[1,2;3,4])
%!test  # 2-D matrix
%! A = [1 2 3]'*[1,2];
%! assert (A, A);
%! fail ("assert (A.*(A!=2),A)");
%!test  # N-D matrix
%! X = zeros (2,2,3);
%! Y = X;
%! Y(1,2,3) = 1.5;
%! fail ("assert (X,Y)", "\(1,2,3\).*Abs err 1.5 exceeds tol 0");

## must give a small tolerance for floating point errors on relative
%!assert (100+100*eps, 100, -2*eps)
%!assert (100, 100+100*eps, -2*eps)
%!error <Rel err .* exceeds tol> assert (100+300*eps, 100, -2*eps)
%!error <Rel err .* exceeds tol> assert (100, 100+300*eps, -2*eps)

## test relative vs. absolute tolerances
%!test  assert (0.1+eps, 0.1,  2*eps);
%!error <Rel err 2.2204e-0?15 exceeds tol> assert (0.1+eps, 0.1, -2*eps);
%!test  assert (100+100*eps, 100, -2*eps);
%!error <Abs err 2.8422e-0?14 exceeds tol> assert (100+100*eps, 100,  2*eps);

## Corner case of relative tolerance with 0 divider
%!error <Abs err 2 exceeds tol 0.1> assert (2, 0, -0.1)

## Extra checking of inputs when tolerance unspecified.
%!error <Class single != double> assert (single (1), 1)
%!error <Class uint8 != uint16> assert (uint8 (1), uint16 (1))
%!error <sparse != non-sparse> assert (sparse([1]), [1])
%!error <non-sparse != sparse> assert ([1], sparse([1]))
%!error <complex != real> assert (1+i, 1)
%!error <real != complex> assert (1, 1+i)

## exceptional values
%!assert ([NaN, NA, Inf, -Inf, 1+eps, eps], [NaN, NA, Inf, -Inf, 1, 0], eps)

%!error <'NaN' mismatch> assert (NaN, 1)
%!error <'NaN' mismatch> assert ([NaN 1], [1 NaN])
%!test
%! try
%!   assert ([NaN 1], [1 NaN]);
%! catch
%!   errmsg = lasterr ();
%!   if (sum (errmsg () == "\n") != 4)
%!     error ("Too many errors reported for NaN assert");
%!   elseif (strfind (errmsg, "NA"))
%!     error ("NA reported for NaN assert");
%!   elseif (strfind (errmsg, "Abs err NaN exceeds tol 0"))
%!     error ("Abs err reported for NaN assert");
%!   endif
%! end_try_catch

%!error <'NA' mismatch> assert (NA, 1)
%!error assert ([NA 1]', [1 NA]')
%!test
%! try
%!   assert ([NA 1]', [1 NA]');
%! catch
%!   errmsg = lasterr ();
%!   if (sum (errmsg () == "\n") != 4)
%!     error ("Too many errors reported for NA assert");
%!   elseif (strfind (errmsg, "NaN"))
%!     error ("NaN reported for NA assert");
%!   elseif (strfind (errmsg, "Abs err NA exceeds tol 0"))
%!     error ("Abs err reported for NA assert");
%!   endif
%! end_try_catch
%!error assert ([(complex (NA, 1)) (complex (2, NA))], [(complex (NA, 2)) 2])

%!error <'Inf' mismatch> assert (-Inf, Inf)
%!error <'Inf' mismatch> assert ([-Inf Inf], [Inf -Inf])
%!test
%! try
%!   assert (complex (Inf, 0.2), complex (-Inf, 0.2 + 2*eps), eps);
%! catch
%!   errmsg = lasterr ();
%!   if (sum (errmsg () == "\n") != 3)
%!     error ("Too many errors reported for Inf assert");
%!   elseif (strfind (errmsg, "Abs err"))
%!     error ("Abs err reported for Inf assert");
%!   endif
%! end_try_catch
%!error <Abs err> assert (complex (Inf, 0.2), complex (Inf, 0.2 + 2*eps), eps)

## strings
%!assert ("dog", "dog")
%!error <Strings don't match> assert ("dog", "cat")
%!error <Expected string, but observed number> assert (3, "dog")
%!error <Class char != double> assert ("dog", [3 3 3])
%!error <Expected string, but observed cell> assert ({"dog"}, "dog")
%!error <Expected string, but observed struct> assert (struct ("dog", 3), "dog")

## cell arrays
%!error <Expected cell, but observed double> assert (1, {1})
%!error <Dimensions don't match> assert (cell (1,2,3), cell (3,2,1))
%!test
%! x = {{{1}}, 2};  # cell with multiple levels
%! y = x;
%! assert (x,y);
%! y{1}{1}{1} = 3;
%! fail ("assert (x,y)", "Abs err 2 exceeds tol 0");

## function handles
%!assert (@sin, @sin)
%!error <Function handles don't match> assert (@sin, @cos)
%!error <Expected function handle, but observed double> assert (pi, @cos)
%!error <Class function_handle != double> assert (@sin, pi)

%!test
%! x = {[3], [1,2,3]; 100+100*eps, "dog"};
%! y = x;
%! assert (x, y);
%! y = x; y(1,1) = [2];
%! fail ("assert (x, y)");
%! y = x; y(1,2) = [0, 2, 3];
%! fail ("assert (x, y)");
%! y = x; y(2,1) = 101;
%! fail ("assert (x, y)");
%! y = x; y(2,2) = "cat";
%! fail ("assert (x, y)");
%! y = x; y(1,1) = [2];  y(1,2) = [0, 2, 3]; y(2,1) = 101; y(2,2) = "cat";
%! fail ("assert (x, y)");

## structures
%!error <Expected struct, but observed double> assert (1, struct ("a", 1))
%!error <Structure sizes don't match>
%! x(1,2,3).a = 1;
%! y(1,2).a = 1;
%! assert (x,y);
%!error <Structure sizes don't match>
%! x(1,2,3).a = 1;
%! y(3,2,2).a = 1;
%! assert (x,y);
%!error <Structure sizes don't match>
%! x.a = 1;
%! x.b = 1;
%! y.a = 1;
%! assert (x,y);
%!error <'b' is not an expected field>
%! x.b = 1;
%! y.a = 1;
%! assert (x,y);

%!test
%! x.a = 1; x.b=[2, 2];
%! y.a = 1; y.b=[2, 2];
%! assert (x, y);
%! y.b=3;
%! fail ("assert (x, y)");
%! fail ("assert (3, x)");
%! fail ("assert (x, 3)");
%! ## Empty structures
%! x = resize (x, 0, 1);
%! y = resize (y, 0, 1);
%! assert (x, y);

## vector of tolerances
%!test
%! x = [-40:0];
%! y1 = (10.^x).*(10.^x);
%! y2 = 10.^(2*x);
%! assert (y1, y2, eps (y1));
%! fail ("assert (y1, y2 + eps*1e-70, eps (y1))");

## Multiple tolerances
%!test
%! x = [1 2; 3 4];
%! y = [0 -1; 1 2];
%! tol = [-0.1 0; -0.2 0.3];
%! try
%!   assert (x, y, tol);
%! catch
%!   errmsg = lasterr ();
%!   if (sum (errmsg () == "\n") != 6)
%!     error ("Incorrect number of errors reported");
%!   endif
%!   assert (! isempty (regexp (errmsg, '\(1,2\).*Abs err 3 exceeds tol 0\>')));
%!   assert (! isempty (regexp (errmsg, '\(2,2\).*Abs err 2 exceeds tol 0.3')));
%!   assert (! isempty (regexp (errmsg, '\(1,1\).*Abs err 1 exceeds tol 0.1')));
%!   assert (! isempty (regexp (errmsg, '\(2,1\).*Rel err 2 exceeds tol 0.2')));
%! end_try_catch

## test input validation
%!error assert ()
%!error assert (1,2,3,4)


## Convert all error indices into tuple format
function cout = ind2tuple (matsize, erridx)

  cout = cell (numel (erridx), 1);
  tmp = cell (1, numel (matsize));
  [tmp{:}] = ind2sub (matsize, erridx(:));
  subs = [tmp{:}];
  if (numel (matsize) == 2)
    subs = subs(:, matsize != 1);
  endif
  for i = 1:numel (erridx)
    loc = sprintf ("%d,", subs(i,:));
    cout{i} = ["(" loc(1:end-1) ")"];
  endfor

endfunction


## Pretty print the various errors in a condensed tabular format.
function str = pprint (argin, err)

  str = ["ASSERT errors for:  assert " argin "\n"];
  str = [str, "\n  Location  |  Observed  |  Expected  |  Reason\n"];
  for i = 1:length (err.index)
    leni = length (err.index{i});
    leno = length (err.observed{i});
    lene = length (err.expected{i});
    str = [str, sprintf("%*s%*s %*s%*s %*s%*s   %s\n",
                        6+fix(leni/2), err.index{i}   , 6-fix(leni/2), "",
                        6+fix(leno/2), err.observed{i}, 6-fix(leno/2), "",
                        6+fix(lene/2), err.expected{i}, 6-fix(lene/2), "",
                        err.reason{i})];
  endfor

endfunction

