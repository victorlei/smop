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
## @deftypefn  {Function File} {} kurtosis (@var{x})
## @deftypefnx {Function File} {} kurtosis (@var{x}, @var{flag})
## @deftypefnx {Function File} {} kurtosis (@var{x}, @var{flag}, @var{dim})
## Compute the sample kurtosis of the elements of @var{x}.
##
## The sample kurtosis is defined as
## @tex
## $$
## \kappa_1 = {{{1\over N}\,
##          \sum_{i=1}^N (@var{x}_i - \bar{@var{x}})^4} \over \sigma^4},
## $$
## where $N$ is the length of @var{x}, $\bar{@var{x}}$ its mean, and $\sigma$
## its (uncorrected) standard deviation.
## @end tex
## @ifnottex
##
## @example
## @group
##      mean ((@var{x} - mean (@var{x})).^4)
## k1 = ------------------------
##             std (@var{x}).^4
## @end group
## @end example
##
## @end ifnottex
##
## @noindent
## The optional argument @var{flag} controls which normalization is used.
## If @var{flag} is equal to 1 (default value, used when @var{flag} is omitted
## or empty), return the sample kurtosis as defined above.  If @var{flag} is
## equal to 0, return the @w{"bias-corrected"} kurtosis coefficient instead:
## @tex
## $$
## \kappa_0 = 3 + {\scriptstyle N - 1 \over \scriptstyle (N - 2)(N - 3)} \,
##     \left( (N + 1)\, \kappa_1 - 3 (N - 1) \right)
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##               N - 1
## k0 = 3 + -------------- * ((N + 1) * k1 - 3 * (N - 1))
##          (N - 2)(N - 3)
## @end group
## @end example
##
## @end ifnottex
## The bias-corrected kurtosis coefficient is obtained by replacing the sample
## second and fourth central moments by their unbiased versions.  It is an
## unbiased estimate of the population kurtosis for normal populations.
##
## If @var{x} is a matrix, or more generally a multi-dimensional array, return
## the kurtosis along the first non-singleton dimension.  If the optional
## @var{dim} argument is given, operate along this dimension.
##
## @seealso{var, skewness, moment}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 29 July 1994
## Adapted-By: jwe

function y = kurtosis (x, flag, dim)

  if (nargin < 1) || (nargin > 3)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("kurtosis: X must be a numeric vector or matrix");
  endif

  if (nargin < 2 || isempty (flag))
    flag = 1;  # default: do not use the "bias corrected" version
  else
    if (! isscalar (flag) || (flag != 0 && flag != 1))
      error ("kurtosis: FLAG must be 0 or 1");
    endif
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin < 3)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (! (isscalar (dim) && dim == fix (dim)) || ! (1 <= dim && dim <= nd))
      error ("kurtosis: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);
  sz(dim) = 1;

  x = center (x, dim);   # center also promotes integer, logical to double
  v = var (x, 1, dim);   # normalize with 1/N
  y = sum (x .^ 4, dim);
  idx = (v != 0);
  y(idx) = y(idx) ./ (n * v(idx) .^ 2);
  y(! idx) = NaN;

  ## Apply bias correction to the second and fourth central sample moment
  if (flag == 0)
    if (n > 3)
      C = (n - 1) / ((n - 2) * (n - 3));
      y = 3 + C * ((n + 1) * y - 3 * (n - 1));
    else
      y(:) = NaN;
    endif
  endif

endfunction


%!test
%! x = [-1; 0; 0; 0; 1];
%! y = [x, 2*x];
%! assert (kurtosis (y), [2.5, 2.5], sqrt (eps));

%!assert (kurtosis ([-3, 0, 1]) == kurtosis ([-1, 0, 3]))
%!assert (kurtosis (ones (3, 5)), NaN (1, 5))

%!assert (kurtosis ([1:5 10; 1:5 10],  0, 2), 5.4377317925288901 * [1; 1], 8 * eps)
%!assert (kurtosis ([1:5 10; 1:5 10],  1, 2), 2.9786509002956195 * [1; 1], 8 * eps)
%!assert (kurtosis ([1:5 10; 1:5 10], [], 2), 2.9786509002956195 * [1; 1], 8 * eps)

## Test behaviour on single input
%!assert (kurtosis (single ([1:5 10])), single (2.9786513), eps ("single"))
%!assert (kurtosis (single ([1 2]), 0), single (NaN))

## Verify no "divide-by-zero" warnings
%!test
%! wstate = warning ("query", "Octave:divide-by-zero");
%! warning ("on", "Octave:divide-by-zero");
%! unwind_protect
%!   lastwarn ("");  # clear last warning
%!   kurtosis (1);
%!   assert (lastwarn (), "");
%! unwind_protect_cleanup
%!   warning (wstate, "Octave:divide-by-zero");
%! end_unwind_protect

## Test input validation
%!error kurtosis ()
%!error kurtosis (1, 2, 3)
%!error <X must be a numeric vector or matrix> kurtosis (['A'; 'B'])
%!error <FLAG must be 0 or 1> kurtosis (1, 2)
%!error <FLAG must be 0 or 1> kurtosis (1, [1 0])
%!error <DIM must be an integer> kurtosis (1, [], ones (2,2))
%!error <DIM must be an integer> kurtosis (1, [], 1.5)
%!error <DIM must be .* a valid dimension> kurtosis (1, [], 0)
%!error <DIM must be .* a valid dimension> kurtosis (1, [], 3)
