## Copyright (C) 2012 Rik Wehbring
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
## @deftypefn  {Function File} {} wblrnd (@var{scale}, @var{shape})
## @deftypefnx {Function File} {} wblrnd (@var{scale}, @var{shape}, @var{r})
## @deftypefnx {Function File} {} wblrnd (@var{scale}, @var{shape}, @var{r}, @var{c}, @dots{})
## @deftypefnx {Function File} {} wblrnd (@var{scale}, @var{shape}, [@var{sz}])
## Return a matrix of random samples from the Weibull distribution with
## parameters @var{scale} and @var{shape}.
##
## When called with a single size argument, return a square matrix with
## the dimension specified.  When called with more than one scalar argument the
## first two arguments are taken as the number of rows and columns and any
## further arguments specify additional matrix dimensions.  The size may also
## be specified with a vector of dimensions @var{sz}.
##
## If no size arguments are given then the result matrix is the common size of
## @var{scale} and @var{shape}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Weibull distribution

function rnd = wblrnd (scale, shape, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! isscalar (scale) || ! isscalar (shape))
    [retval, scale, shape] = common_size (scale, shape);
    if (retval > 0)
      error ("wblrnd: SCALE and SHAPE must be of common size or scalars");
    endif
  endif

  if (iscomplex (scale) || iscomplex (shape))
    error ("wblrnd: SCALE and SHAPE must not be complex");
  endif

  if (nargin == 2)
    sz = size (scale);
  elseif (nargin == 3)
    if (isscalar (varargin{1}) && varargin{1} >= 0)
      sz = [varargin{1}, varargin{1}];
    elseif (isrow (varargin{1}) && all (varargin{1} >= 0))
      sz = varargin{1};
    else
      error ("wblrnd: dimension vector must be row vector of non-negative integers");
    endif
  elseif (nargin > 3)
    if (any (cellfun (@(x) (! isscalar (x) || x < 0), varargin)))
      error ("wblrnd: dimensions must be non-negative integers");
    endif
    sz = [varargin{:}];
  endif

  if (! isscalar (scale) && ! isequal (size (scale), sz))
    error ("wblrnd: SCALE and SHAPE must be scalar or of size SZ");
  endif

  if (isa (scale, "single") || isa (shape, "single"))
    cls = "single";
  else
    cls = "double";
  endif

  if (isscalar (scale) && isscalar (shape))
    if ((scale > 0) && (scale < Inf) && (shape > 0) && (shape < Inf))
      rnd = scale * rande (sz, cls) .^ (1/shape);
    else
      rnd = NaN (sz, cls);
    endif
  else
    rnd = scale .* rande (sz, cls) .^ (1./shape);

    k = (scale <= 0) | (scale == Inf) | (shape <= 0) | (shape == Inf);
    rnd(k) = NaN;
  endif

endfunction


%!assert (size (wblrnd (1,2)), [1, 1])
%!assert (size (wblrnd (ones (2,1), 2)), [2, 1])
%!assert (size (wblrnd (ones (2,2), 2)), [2, 2])
%!assert (size (wblrnd (1, 2*ones (2,1))), [2, 1])
%!assert (size (wblrnd (1, 2*ones (2,2))), [2, 2])
%!assert (size (wblrnd (1, 2, 3)), [3, 3])
%!assert (size (wblrnd (1, 2, [4 1])), [4, 1])
%!assert (size (wblrnd (1, 2, 4, 1)), [4, 1])

## Test class of input preserved
%!assert (class (wblrnd (1, 2)), "double")
%!assert (class (wblrnd (single (1), 2)), "single")
%!assert (class (wblrnd (single ([1 1]), 2)), "single")
%!assert (class (wblrnd (1, single (2))), "single")
%!assert (class (wblrnd (1, single ([2 2]))), "single")

## Test input validation
%!error wblrnd ()
%!error wblrnd (1)
%!error wblrnd (ones (3), ones (2))
%!error wblrnd (ones (2), ones (3))
%!error wblrnd (i, 2)
%!error wblrnd (2, i)
%!error wblrnd (1,2, -1)
%!error wblrnd (1,2, ones (2))
%!error wblrnd (1, 2, [2 -1 2])
%!error wblrnd (1,2, 1, ones (2))
%!error wblrnd (1,2, 1, -1)
%!error wblrnd (ones (2,2), 2, 3)
%!error wblrnd (ones (2,2), 2, [3, 2])
%!error wblrnd (ones (2,2), 2, 2, 3)

