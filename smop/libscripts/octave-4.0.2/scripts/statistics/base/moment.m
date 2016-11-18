## Copyright (C) 1995-2015 Kurt Hornik
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
## @deftypefn  {Function File} {} moment (@var{x}, @var{p})
## @deftypefnx {Function File} {} moment (@var{x}, @var{p}, @var{type})
## @deftypefnx {Function File} {} moment (@var{x}, @var{p}, @var{dim})
## @deftypefnx {Function File} {} moment (@var{x}, @var{p}, @var{type}, @var{dim})
## @deftypefnx {Function File} {} moment (@var{x}, @var{p}, @var{dim}, @var{type})
## Compute the @var{p}-th central moment of the vector @var{x}.
##
## @tex
## $$
## {\sum_{i=1}^N (x_i - \bar{x})^p \over N}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## 1/N SUM_i (x(i) - mean(x))^p
## @end group
## @end example
##
## @end ifnottex
##
## If @var{x} is a matrix, return the row vector containing the @var{p}-th
## central moment of each column.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## The optional string @var{type} specifies the type of moment to be computed.
## Valid options are:
##
## @table @asis
## @item @qcode{"c"}
##   Central Moment (default).
##
## @item  @qcode{"a"}
## @itemx @qcode{"ac"}
##   Absolute Central Moment.  The moment about the mean ignoring sign
## defined as
## @tex
## $$
## {\sum_{i=1}^N {\left| x_i - \bar{x} \right|}^p \over N}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## 1/N SUM_i (abs (x(i) - mean(x)))^p
## @end group
## @end example
##
## @end ifnottex
##
## @item @qcode{"r"}
##   Raw Moment.  The moment about zero defined as
##
## @tex
## $$
## {\rm moment} (x) = { \sum_{i=1}^N {x_i}^p \over N }
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## moment (x) = 1/N SUM_i x(i)^p
## @end group
## @end example
##
## @end ifnottex
##
## @item @nospell{@qcode{"ar"}}
##   Absolute Raw Moment.  The moment about zero ignoring sign defined as
## @tex
## $$
## {\sum_{i=1}^N {\left| x_i \right|}^p \over N}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## 1/N SUM_i ( abs (x(i)) )^p
## @end group
## @end example
##
## @end ifnottex
## @end table
##
## If both @var{type} and @var{dim} are given they may appear in any order.
## @seealso{var, skewness, kurtosis}
## @end deftypefn

## Can easily be made to work for continuous distributions (using quad)
## as well, but how does the general case work?

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute moments

function m = moment (x, p, opt1, opt2)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)) || isempty (x))
    error ("moment: X must be a non-empty numeric matrix or vector");
  endif

  if (! (isnumeric (p) && isscalar (p)))
    error ("moment: P must be a numeric scalar");
  endif

  need_dim = false;

  if (nargin == 2)
    type = "";
    need_dim = true;
  elseif (nargin == 3)
    if (ischar (opt1))
      type = opt1;
      need_dim = true;
    else
      dim = opt1;
      type = "";
    endif
  elseif (nargin == 4)
    if (ischar (opt1))
      type = opt1;
      dim = opt2;
    elseif (ischar (opt2))
      type = opt2;
      dim = opt1;
    else
      error ("moment: expecting TYPE to be a string");
    endif
  endif

  nd = ndims (x);
  sz = size (x);
  if (need_dim)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (! (isscalar (dim) && dim == fix (dim)) || ! (1 <= dim && dim <= nd))
      error ("moment: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);

  if (! any (type == "r"))
    x = center (x, dim);
  endif
  if (any (type == "a"))
    x = abs (x);
  endif

  m = sum (x .^ p, dim) / n;

endfunction


%!shared x
%! x = rand (10);
%!assert (moment (x,1), mean (center (x)), eps)
%!assert (moment (x,2), meansq (center (x)), eps)
%!assert (moment (x,1,2), mean (center (x, 2), 2), eps)
%!assert (moment (x,1,"a"), mean (abs (center (x))), eps)
%!assert (moment (x,1,"r"), mean (x), eps)
%!assert (moment (x,1,"ar"), mean (abs (x)), eps)

%!assert (moment (single ([1 2 3]), 1, "r"), single (2))

## Test input validation
%!error moment ()
%!error moment (1)
%!error moment (1, 2, 3, 4, 5)
%!error <X must be a non-empty numeric matrix> moment (['A'; 'B'], 2)
%!error <X must be a non-empty numeric matrix> moment (ones (2,0,3), 2)
%!error <P must be a numeric scalar> moment (1, true)
%!error <P must be a numeric scalar> moment (1, ones (2,2))
%!error <expecting TYPE to be a string> moment (1, 2, 3, 4)
%!error <DIM must be an integer and a valid dimension> moment (1, 2, ones (2,2))
%!error <DIM must be an integer and a valid dimension> moment (1, 2, 1.5)
%!error <DIM must be an integer and a valid dimension> moment (1, 2, 4)

