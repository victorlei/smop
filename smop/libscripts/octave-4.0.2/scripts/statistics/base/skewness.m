## Copyright (C) 2013-2015 Julien Bect
## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn  {Function File} {} skewness (@var{x})
## @deftypefnx {Function File} {} skewness (@var{x}, @var{flag})
## @deftypefnx {Function File} {} skewness (@var{x}, @var{flag}, @var{dim})
## Compute the sample skewness of the elements of @var{x}.
##
## The sample skewness is defined as
## @tex
## $$
## {\rm skewness} (@var{x}) = {{{1\over N}\,
##          \sum_{i=1}^N (@var{x}_i - \bar{@var{x}})^3} \over \sigma^3},
## $$
## where $N$ is the length of @var{x}, $\bar{@var{x}}$ its mean and $\sigma$
## its (uncorrected) standard deviation.
## @end tex
## @ifnottex
##
## @example
## @group
##                mean ((@var{x} - mean (@var{x})).^3)
## skewness (@var{X}) = ------------------------.
##                       std (@var{x}).^3
## @end group
## @end example
##
## @end ifnottex
##
## @noindent
## The optional argument @var{flag} controls which normalization is used.
## If @var{flag} is equal to 1 (default value, used when @var{flag} is omitted
## or empty), return the sample skewness as defined above.  If @var{flag} is
## equal to 0, return the adjusted skewness coefficient instead:
## @tex
## $$
## {\rm skewness} (@var{x}) = {\sqrt{N (N - 1)} \over N - 2} \times \,
##   {{{1 \over N} \sum_{i=1}^N (@var{x}_i - \bar{@var{x}})^3} \over \sigma^3}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##                   sqrt (N*(N-1))   mean ((@var{x} - mean (@var{x})).^3)
## skewness (@var{X}, 0) = -------------- * ------------------------.
##                       (N - 2)             std (@var{x}).^3
## @end group
## @end example
##
## @end ifnottex
## The adjusted skewness coefficient is obtained by replacing the sample second
## and third central moments by their bias-corrected versions.
##
## If @var{x} is a matrix, or more generally a multi-dimensional array, return
## the skewness along the first non-singleton dimension.  If the optional
## @var{dim} argument is given, operate along this dimension.
##
## @seealso{var, kurtosis, moment}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 29 July 1994
## Adapted-By: jwe

function y = skewness (x, flag, dim)

  if (nargin < 1) || (nargin > 3)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("skewness: X must be a numeric vector or matrix");
  endif

  if (nargin < 2 || isempty (flag))
    flag = 1;  # default: do not use the "bias corrected" version
  else
    if (! isscalar (flag) || (flag != 0 && flag != 1))
      error ("skewness: FLAG must be 0 or 1");
    endif
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin < 3)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim)) || !(1 <= dim && dim <= nd))
      error ("skewness: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);
  sz(dim) = 1;

  x = center (x, dim);   # center also promotes integer, logical to double
  s = std (x, 1, dim);   # Normalize with 1/N
  y = sum (x .^ 3, dim);
  idx = (s != 0);
  y(idx) ./= (n * s(idx) .^ 3);
  y(! idx) = NaN;

  ## Apply bias correction to the second and third central sample moment
  if (flag == 0)
    if (n > 2)
      y *= sqrt (n * (n - 1)) / (n - 2);
    else
      y(:) = NaN;
    endif
  endif

endfunction


%!assert (skewness ([-1, 0, 1]), 0)
%!assert (skewness ([-2, 0, 1]) < 0)
%!assert (skewness ([-1, 0, 2]) > 0)
%!assert (skewness ([-3, 0, 1]) == -1 * skewness ([-1, 0, 3]))
%!assert (skewness (ones (3, 5)), NaN (1, 5))

%!test
%! x = [0; 0; 0; 1];
%! y = [x, 2*x];
%! assert (skewness (y), 1.154700538379251 * [1 1], 5*eps);

%!assert (skewness ([1:5 10; 1:5 10],  0, 2), 1.439590274527954 * [1; 1], eps)
%!assert (skewness ([1:5 10; 1:5 10],  1, 2), 1.051328089232020 * [1; 1], 2*eps)
%!assert (skewness ([1:5 10; 1:5 10], [], 2), 1.051328089232020 * [1; 1], 2*eps)

## Test behaviour on single input
%!assert (skewness (single ([1:5 10])), single (1.0513283), eps ("single"))
%!assert (skewness (single ([1 2]), 0), single (NaN))

## Verify no "divide-by-zero" warnings
%!test
%! wstate = warning ("query", "Octave:divide-by-zero");
%! warning ("on", "Octave:divide-by-zero");
%! unwind_protect
%!   lastwarn ("");  # clear last warning
%!   skewness (1);
%!   assert (lastwarn (), "");
%! unwind_protect_cleanup
%!   warning (wstate, "Octave:divide-by-zero");
%! end_unwind_protect

## Test input validation
%!error skewness ()
%!error skewness (1, 2, 3)
%!error <X must be a numeric vector or matrix> skewness (['A'; 'B'])
%!error <FLAG must be 0 or 1> skewness (1, 2)
%!error <FLAG must be 0 or 1> skewness (1, [1 0])
%!error <DIM must be an integer> skewness (1, [], ones (2,2))
%!error <DIM must be an integer> skewness (1, [], 1.5)
%!error <DIM must be .* a valid dimension> skewness (1, [], 0)
%!error <DIM must be .* a valid dimension> skewness (1, [], 3)
