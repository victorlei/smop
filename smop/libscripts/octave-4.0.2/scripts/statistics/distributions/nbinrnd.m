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
## @deftypefn  {Function File} {} nbinrnd (@var{n}, @var{p})
## @deftypefnx {Function File} {} nbinrnd (@var{n}, @var{p}, @var{r})
## @deftypefnx {Function File} {} nbinrnd (@var{n}, @var{p}, @var{r}, @var{c}, @dots{})
## @deftypefnx {Function File} {} nbinrnd (@var{n}, @var{p}, [@var{sz}])
## Return a matrix of random samples from the negative binomial distribution
## with parameters @var{n} and @var{p}.
##
## When called with a single size argument, return a square matrix with
## the dimension specified.  When called with more than one scalar argument the
## first two arguments are taken as the number of rows and columns and any
## further arguments specify additional matrix dimensions.  The size may also
## be specified with a vector of dimensions @var{sz}.
##
## If no size arguments are given then the result matrix is the common size of
## @var{n} and @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Pascal distribution

function rnd = nbinrnd (n, p, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! isscalar (n) || ! isscalar (p))
    [retval, n, p] = common_size (n, p);
    if (retval > 0)
      error ("nbinrnd: N and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (n) || iscomplex (p))
    error ("nbinrnd: N and P must not be complex");
  endif

  if (nargin == 2)
    sz = size (n);
  elseif (nargin == 3)
    if (isscalar (varargin{1}) && varargin{1} >= 0)
      sz = [varargin{1}, varargin{1}];
    elseif (isrow (varargin{1}) && all (varargin{1} >= 0))
      sz = varargin{1};
    else
      error ("nbinrnd: dimension vector must be row vector of non-negative integers");
    endif
  elseif (nargin > 3)
    if (any (cellfun (@(x) (! isscalar (x) || x < 0), varargin)))
      error ("nbinrnd: dimensions must be non-negative integers");
    endif
    sz = [varargin{:}];
  endif

  if (! isscalar (n) && ! isequal (size (n), sz))
    error ("nbinrnd: N and P must be scalar or of size SZ");
  endif

  if (isa (n, "single") || isa (p, "single"))
    cls = "single";
  else
    cls = "double";
  endif

  if (isscalar (n) && isscalar (p))
    if ((n > 0) && (n < Inf) && (p > 0) && (p <= 1))
      rnd = randp ((1 - p) ./ p .* randg (n, sz, cls), cls);
    elseif ((n > 0) && (n < Inf) && (p == 0))
      rnd = zeros (sz, cls);
    else
      rnd = NaN (sz, cls);
    endif
  else
    rnd = NaN (sz, cls);

    k = (n > 0) & (n < Inf) & (p == 0);
    rnd(k) = 0;

    k = (n > 0) & (n < Inf) & (p > 0) & (p <= 1);
    rnd(k) = randp ((1 - p(k)) ./ p(k) .* randg (n(k), cls));
  endif

endfunction


%!assert (size (nbinrnd (2, 1/2)), [1, 1])
%!assert (size (nbinrnd (2*ones (2,1), 1/2)), [2, 1])
%!assert (size (nbinrnd (2*ones (2,2), 1/2)), [2, 2])
%!assert (size (nbinrnd (2, 1/2*ones (2,1))), [2, 1])
%!assert (size (nbinrnd (2, 1/2*ones (2,2))), [2, 2])
%!assert (size (nbinrnd (2, 1/2, 3)), [3, 3])
%!assert (size (nbinrnd (2, 1/2, [4 1])), [4, 1])
%!assert (size (nbinrnd (2, 1/2, 4, 1)), [4, 1])

## Test class of input preserved
%!assert (class (nbinrnd (2, 1/2)), "double")
%!assert (class (nbinrnd (single (2), 1/2)), "single")
%!assert (class (nbinrnd (single ([2 2]), 1/2)), "single")
%!assert (class (nbinrnd (2, single (1/2))), "single")
%!assert (class (nbinrnd (2, single ([1/2 1/2]))), "single")

## Test input validation
%!error nbinrnd ()
%!error nbinrnd (1)
%!error nbinrnd (ones (3), ones (2))
%!error nbinrnd (ones (2), ones (3))
%!error nbinrnd (i, 2)
%!error nbinrnd (2, i)
%!error nbinrnd (1,2, -1)
%!error nbinrnd (1,2, ones (2))
%!error nbinrnd (1, 2, [2 -1 2])
%!error nbinrnd (1,2, 1, ones (2))
%!error nbinrnd (1,2, 1, -1)
%!error nbinrnd (ones (2,2), 2, 3)
%!error nbinrnd (ones (2,2), 2, [3, 2])
%!error nbinrnd (ones (2,2), 2, 2, 3)

