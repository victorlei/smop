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
## @deftypefn  {Function File} {} poissrnd (@var{lambda})
## @deftypefnx {Function File} {} poissrnd (@var{lambda}, @var{r})
## @deftypefnx {Function File} {} poissrnd (@var{lambda}, @var{r}, @var{c}, @dots{})
## @deftypefnx {Function File} {} poissrnd (@var{lambda}, [@var{sz}])
## Return a matrix of random samples from the Poisson distribution with
## parameter @var{lambda}.
##
## When called with a single size argument, return a square matrix with
## the dimension specified.  When called with more than one scalar argument the
## first two arguments are taken as the number of rows and columns and any
## further arguments specify additional matrix dimensions.  The size may also
## be specified with a vector of dimensions @var{sz}.
##
## If no size arguments are given then the result matrix is the size of
## @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Poisson distribution

function rnd = poissrnd (lambda, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    sz = size (lambda);
  elseif (nargin == 2)
    if (isscalar (varargin{1}) && varargin{1} >= 0)
      sz = [varargin{1}, varargin{1}];
    elseif (isrow (varargin{1}) && all (varargin{1} >= 0))
      sz = varargin{1};
    else
      error ("poissrnd: dimension vector must be row vector of non-negative integers");
    endif
  elseif (nargin > 2)
    if (any (cellfun (@(x) (! isscalar (x) || x < 0), varargin)))
      error ("poissrnd: dimensions must be non-negative integers");
    endif
    sz = [varargin{:}];
  endif

  if (! isscalar (lambda) && ! isequal (size (lambda), sz))
    error ("poissrnd: LAMBDA must be scalar or of size SZ");
  endif

  if (iscomplex (lambda))
    error ("poissrnd: LAMBDA must not be complex");
  endif

  if (isa (lambda, "single"))
    cls = "single";
  else
    cls = "double";
  endif

  if (isscalar (lambda))
    if (lambda >= 0 && lambda < Inf)
      rnd = randp (lambda, sz, cls);
    else
      rnd = NaN (sz, cls);
    endif
  else
    rnd = NaN (sz, cls);

    k = (lambda >= 0) & (lambda < Inf);
    rnd(k) = randp (lambda(k), cls);
  endif

endfunction


%!assert (size (poissrnd (2)), [1, 1])
%!assert (size (poissrnd (ones (2,1))), [2, 1])
%!assert (size (poissrnd (ones (2,2))), [2, 2])
%!assert (size (poissrnd (1, 3)), [3, 3])
%!assert (size (poissrnd (1, [4 1])), [4, 1])
%!assert (size (poissrnd (1, 4, 1)), [4, 1])

## Test class of input preserved
%!assert (class (poissrnd (2)), "double")
%!assert (class (poissrnd (single (2))), "single")
%!assert (class (poissrnd (single ([2 2]))), "single")

## Test input validation
%!error poissrnd ()
%!error poissrnd (1, -1)
%!error poissrnd (1, ones (2))
%!error poissrnd (1, 2, ones (2))
%!error poissrnd (i)
%!error poissrnd (1, 2, -1)
%!error poissrnd (1, [2 -1 2])
%!error poissrnd (ones (2,2), 3)
%!error poissrnd (ones (2,2), [3, 2])
%!error poissrnd (ones (2,2), 2, 3)

%!assert (poissrnd (0, 1, 1), 0)
%!assert (poissrnd ([0, 0, 0], [1, 3]), [0 0 0])

