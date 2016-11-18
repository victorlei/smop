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
## @deftypefn  {Function File} {} mean (@var{x})
## @deftypefnx {Function File} {} mean (@var{x}, @var{dim})
## @deftypefnx {Function File} {} mean (@var{x}, @var{opt})
## @deftypefnx {Function File} {} mean (@var{x}, @var{dim}, @var{opt})
## Compute the mean of the elements of the vector @var{x}.
##
## The mean is defined as
##
## @tex
## $$ {\rm mean}(x) = \bar{x} = {1\over N} \sum_{i=1}^N x_i $$
## @end tex
## @ifnottex
##
## @example
## mean (x) = SUM_i x(i) / N
## @end example
##
## @end ifnottex
## If @var{x} is a matrix, compute the mean for each column and return them
## in a row vector.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## The optional argument @var{opt} selects the type of mean to compute.
## The following options are recognized:
##
## @table @asis
## @item @qcode{"a"}
## Compute the (ordinary) arithmetic mean.  [default]
##
## @item @qcode{"g"}
## Compute the geometric mean.
##
## @item @qcode{"h"}
## Compute the harmonic mean.
## @end table
##
## Both @var{dim} and @var{opt} are optional.  If both are supplied, either
## may appear first.
## @seealso{median, mode}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute arithmetic, geometric, and harmonic mean

function y = mean (x, opt1, opt2)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("mean: X must be a numeric vector or matrix");
  endif

  need_dim = false;

  if (nargin == 1)
    opt = "a";
    need_dim = true;
  elseif (nargin == 2)
    if (ischar (opt1))
      opt = opt1;
      need_dim = true;
    else
      dim = opt1;
      opt = "a";
    endif
  elseif (nargin == 3)
    if (ischar (opt1))
      opt = opt1;
      dim = opt2;
    elseif (ischar (opt2))
      opt = opt2;
      dim = opt1;
    else
      error ("mean: OPT must be a string");
    endif
  else
    print_usage ();
  endif

  nd = ndims (x);
  sz = size (x);
  if (need_dim)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
      || !(1 <= dim && dim <= nd))
      error ("mean: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);

  if (strcmp (opt, "a"))
    y = sum (x, dim) / n;
  elseif (strcmp (opt, "g"))
    if (all (x(:) >= 0))
      y = exp (sum (log (x), dim) ./ n);
    else
      error ("mean: X must not contain any negative values");
    endif
  elseif (strcmp (opt, "h"))
    y = n ./ sum (1 ./ x, dim);
  else
    error ("mean: option '%s' not recognized", opt);
  endif

endfunction


%!test
%! x = -10:10;
%! y = x';
%! z = [y, y+10];
%! assert (mean (x), 0);
%! assert (mean (y), 0);
%! assert (mean (z), [0, 10]);

## Test small numbers
%!assert (mean (repmat (0.1,1,1000), "g"), 0.1, 20*eps)

%!assert (mean (magic (3), 1), [5, 5, 5])
%!assert (mean (magic (3), 2), [5; 5; 5])
%!assert (mean ([2 8], "g"), 4)
%!assert (mean ([4 4 2], "h"), 3)
%!assert (mean (logical ([1 0 1 1])), 0.75)
%!assert (mean (single ([1 0 1 1])), single (0.75))

## Test input validation
%!error mean ()
%!error mean (1, 2, 3, 4)
%!error mean ({1:5})
%!error mean (1, 2, 3)
%!error mean (1, ones (2,2))
%!error mean (1, 1.5)
%!error mean (1, 0)
%!error mean (1, 3)
%!error mean (1, "b")

