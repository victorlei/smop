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
## @deftypefn  {Function File} {} normrnd (@var{mu}, @var{sigma})
## @deftypefnx {Function File} {} normrnd (@var{mu}, @var{sigma}, @var{r})
## @deftypefnx {Function File} {} normrnd (@var{mu}, @var{sigma}, @var{r}, @var{c}, @dots{})
## @deftypefnx {Function File} {} normrnd (@var{mu}, @var{sigma}, [@var{sz}])
## Return a matrix of random samples from the normal distribution with
## parameters mean @var{mu} and standard deviation @var{sigma}.
##
## When called with a single size argument, return a square matrix with
## the dimension specified.  When called with more than one scalar argument the
## first two arguments are taken as the number of rows and columns and any
## further arguments specify additional matrix dimensions.  The size may also
## be specified with a vector of dimensions @var{sz}.
##
## If no size arguments are given then the result matrix is the common size of
## @var{mu} and @var{sigma}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the normal distribution

function rnd = normrnd (mu, sigma, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! isscalar (mu) || ! isscalar (sigma))
    [retval, mu, sigma] = common_size (mu, sigma);
    if (retval > 0)
      error ("normrnd: mu and sigma must be of common size or scalars");
    endif
  endif

  if (iscomplex (mu) || iscomplex (sigma))
    error ("normrnd: MU and SIGMA must not be complex");
  endif

  if (nargin == 2)
    sz = size (mu);
  elseif (nargin == 3)
    if (isscalar (varargin{1}) && varargin{1} >= 0)
      sz = [varargin{1}, varargin{1}];
    elseif (isrow (varargin{1}) && all (varargin{1} >= 0))
      sz = varargin{1};
    else
      error ("normrnd: dimension vector must be row vector of non-negative integers");
    endif
  elseif (nargin > 3)
    if (any (cellfun (@(x) (! isscalar (x) || x < 0), varargin)))
      error ("normrnd: dimensions must be non-negative integers");
    endif
    sz = [varargin{:}];
  endif

  if (! isscalar (mu) && ! isequal (size (mu), sz))
    error ("normrnd: mu and sigma must be scalar or of size SZ");
  endif

  if (isa (mu, "single") || isa (sigma, "single"))
    cls = "single";
  else
    cls = "double";
  endif

  if (isscalar (mu) && isscalar (sigma))
    if (isfinite (mu) && (sigma >= 0) && (sigma < Inf))
      rnd = mu + sigma * randn (sz, cls);
    else
      rnd = NaN (sz, cls);
    endif
  else
    rnd = mu + sigma .* randn (sz, cls);
    k = ! isfinite (mu) | !(sigma >= 0) | !(sigma < Inf);
    rnd(k) = NaN;
  endif

endfunction


%!assert (size (normrnd (1,2)), [1, 1])
%!assert (size (normrnd (ones (2,1), 2)), [2, 1])
%!assert (size (normrnd (ones (2,2), 2)), [2, 2])
%!assert (size (normrnd (1, 2*ones (2,1))), [2, 1])
%!assert (size (normrnd (1, 2*ones (2,2))), [2, 2])
%!assert (size (normrnd (1, 2, 3)), [3, 3])
%!assert (size (normrnd (1, 2, [4 1])), [4, 1])
%!assert (size (normrnd (1, 2, 4, 1)), [4, 1])

## Test class of input preserved
%!assert (class (normrnd (1, 2)), "double")
%!assert (class (normrnd (single (1), 2)), "single")
%!assert (class (normrnd (single ([1 1]), 2)), "single")
%!assert (class (normrnd (1, single (2))), "single")
%!assert (class (normrnd (1, single ([2 2]))), "single")

## Test input validation
%!error normrnd ()
%!error normrnd (1)
%!error normrnd (ones (3), ones (2))
%!error normrnd (ones (2), ones (3))
%!error normrnd (i, 2)
%!error normrnd (2, i)
%!error normrnd (1,2, -1)
%!error normrnd (1,2, ones (2))
%!error normrnd (1, 2, [2 -1 2])
%!error normrnd (1,2, 1, ones (2))
%!error normrnd (1,2, 1, -1)
%!error normrnd (ones (2,2), 2, 3)
%!error normrnd (ones (2,2), 2, [3, 2])
%!error normrnd (ones (2,2), 2, 2, 3)

