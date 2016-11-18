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
## @deftypefn  {Function File} {} geornd (@var{p})
## @deftypefnx {Function File} {} geornd (@var{p}, @var{r})
## @deftypefnx {Function File} {} geornd (@var{p}, @var{r}, @var{c}, @dots{})
## @deftypefnx {Function File} {} geornd (@var{p}, [@var{sz}])
## Return a matrix of random samples from the geometric distribution with
## parameter @var{p}.
##
## When called with a single size argument, return a square matrix with
## the dimension specified.  When called with more than one scalar argument the
## first two arguments are taken as the number of rows and columns and any
## further arguments specify additional matrix dimensions.  The size may also
## be specified with a vector of dimensions @var{sz}.
##
## If no size arguments are given then the result matrix is the size of
## @var{p}.
##
## The geometric distribution models the number of failures (@var{x}-1) of a
## Bernoulli trial with probability @var{p} before the first success (@var{x}).
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the geometric distribution

function rnd = geornd (p, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    sz = size (p);
  elseif (nargin == 2)
    if (isscalar (varargin{1}) && varargin{1} >= 0)
      sz = [varargin{1}, varargin{1}];
    elseif (isrow (varargin{1}) && all (varargin{1} >= 0))
      sz = varargin{1};
    else
      error ("geornd: dimension vector must be row vector of non-negative integers");
    endif
  elseif (nargin > 2)
    if (any (cellfun (@(x) (! isscalar (x) || x < 0), varargin)))
      error ("geornd: dimensions must be non-negative integers");
    endif
    sz = [varargin{:}];
  endif

  if (! isscalar (p) && ! isequal (size (p), sz))
    error ("geornd: P must be scalar or of size SZ");
  endif

  if (iscomplex (p))
    error ("geornd: P must not be complex");
  endif

  if (isa (p, "single"))
    cls = "single";
  else
    cls = "double";
  endif

  if (isscalar (p))
    if (p > 0 && p < 1);
      rnd = floor (- rande (sz, cls) ./ log (1 - p));
    elseif (p == 0)
      rnd = Inf (sz, cls);
    elseif (p == 1)
      rnd = zeros (sz, cls);
    elseif (p < 0 || p > 1)
      rnd = NaN (sz, cls);
    endif
  else
    rnd = floor (- rande (sz, cls) ./ log (1 - p));

    k = !(p >= 0) | !(p <= 1);
    rnd(k) = NaN;

    k = (p == 0);
    rnd(k) = Inf;
  endif

endfunction


%!assert (size (geornd (0.5)), [1, 1])
%!assert (size (geornd (0.5*ones (2,1))), [2, 1])
%!assert (size (geornd (0.5*ones (2,2))), [2, 2])
%!assert (size (geornd (0.5, 3)), [3, 3])
%!assert (size (geornd (0.5, [4 1])), [4, 1])
%!assert (size (geornd (0.5, 4, 1)), [4, 1])

## Test class of input preserved
%!assert (class (geornd (0.5)), "double")
%!assert (class (geornd (single (0.5))), "single")
%!assert (class (geornd (single ([0.5 0.5]))), "single")
%!assert (class (geornd (single (0))), "single")
%!assert (class (geornd (single (1))), "single")

## Test input validation
%!error geornd ()
%!error geornd (ones (3), ones (2))
%!error geornd (ones (2), ones (3))
%!error geornd (i)
%!error geornd (1, -1)
%!error geornd (1, ones (2))
%!error geornd (1, [2 -1 2])
%!error geornd (ones (2,2), 2, 3)
%!error geornd (ones (2,2), 3, 2)

