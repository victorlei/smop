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
## @deftypefn  {Function File} {@var{z} =} zscore (@var{x})
## @deftypefnx {Function File} {@var{z} =} zscore (@var{x}, @var{opt})
## @deftypefnx {Function File} {@var{z} =} zscore (@var{x}, @var{opt}, @var{dim})
## @deftypefnx {Function File} {[@var{z}, @var{mu}, @var{sigma}] =} zscore (@dots{})
## Compute the Z score of @var{x}
##
## If @var{x} is a vector, subtract its mean and divide by its standard
## deviation.  If the standard deviation is zero, divide by 1 instead.
##
## The optional parameter @var{opt} determines the normalization to use when
## computing the standard deviation and has the same definition as the
## corresponding parameter for @code{std}.
##
## If @var{x} is a matrix, calculate along the first non-singleton dimension.
## If the third optional argument @var{dim} is given, operate along this
## dimension.
##
## The optional outputs @var{mu} and @var{sigma} contain the mean and standard
## deviation.
##
## @seealso{mean, std, center}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Subtract mean and divide by standard deviation

function [z, mu, sigma] = zscore (x, opt, dim)

  if (nargin < 1 || nargin > 3 )
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("zscore: X must be a numeric vector or matrix");
  endif

  if (nargin < 2)
    opt = 0;
  else
    if (opt != 0 && opt != 1 || ! isscalar (opt))
      error ("zscore: OPT must be empty, 0, or 1");
    endif
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin < 3)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("zscore: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);
  if (n == 0)
    z = x;
  else

    if (isinteger (x))
      x = double (x);
    endif

    mu = mean (x, dim);
    sigma = std (x, opt, dim);
    s = sigma;
    s(s==0) = 1;
    ## FIXME: Use normal broadcasting once we can disable that warning
    z = bsxfun (@rdivide, bsxfun (@minus, x, mu), s);
  endif

endfunction


%!assert (zscore ([1,2,3]), [-1,0,1])
%!assert (zscore (single ([1,2,3])), single ([-1,0,1]))
%!assert (zscore (int8 ([1,2,3])), [-1,0,1])
%!assert (zscore (ones (3,2,2,2)), zeros (3,2,2,2))
%!assert (zscore ([2,0,-2;0,2,0;-2,-2,2]), [1,0,-1;0,1,0;-1,-1,1])

## Test input validation
%!error zscore ()
%!error zscore (1, 2, 3)
%!error zscore (['A'; 'B'])
%!error zscore (1, ones (2,2))
%!error zscore (1, 1.5)
%!error zscore (1, 1, 0)
%!error zscore (1, 3)

