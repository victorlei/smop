## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 1996-2015 Kurt Hornik
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
## @deftypefn  {Function File} {} discrete_rnd (@var{v}, @var{p})
## @deftypefnx {Function File} {} discrete_rnd (@var{v}, @var{p}, @var{r})
## @deftypefnx {Function File} {} discrete_rnd (@var{v}, @var{p}, @var{r}, @var{c}, @dots{})
## @deftypefnx {Function File} {} discrete_rnd (@var{v}, @var{p}, [@var{sz}])
## Return a matrix of random samples from the univariate distribution which
## assumes the values in @var{v} with probabilities @var{p}.
##
## When called with a single size argument, return a square matrix with
## the dimension specified.  When called with more than one scalar argument the
## first two arguments are taken as the number of rows and columns and any
## further arguments specify additional matrix dimensions.  The size may also
## be specified with a vector of dimensions @var{sz}.
##
## If no size arguments are given then the result matrix is the common size of
## @var{v} and @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from a discrete distribution

function rnd = discrete_rnd (v, p, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! isvector (v))
    error ("discrete_rnd: V must be a vector");
  elseif (! isvector (p) || (length (p) != length (v)))
    error ("discrete_rnd: P must be a vector with length (V) elements");
  elseif (any (isnan (p)))
    error ("discrete_rnd: P must not have any NaN elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_rnd: P must be a nonzero, non-negative vector");
  endif

  if (nargin == 2)
    sz = size (v);
  elseif (nargin == 3)
    if (isscalar (varargin{1}) && varargin{1} >= 0)
      sz = [varargin{1}, varargin{1}];
    elseif (isrow (varargin{1}) && all (varargin{1} >= 0))
      sz = varargin{1};
    else
      error ("discrete_rnd: dimension vector must be row vector of non-negative integers");
    endif
  elseif (nargin > 3)
    if (any (cellfun (@(x) (! isscalar (x) || x < 0), varargin)))
      error ("discrete_rnd: dimensions must be non-negative integers");
    endif
    sz = [varargin{:}];
  endif

  rnd = v(lookup (cumsum (p(1:end-1)) / sum (p), rand (sz)) + 1);
  rnd = reshape (rnd, sz);

endfunction


%!assert (size (discrete_rnd (1:2, 1:2, 3)), [3, 3])
%!assert (size (discrete_rnd (1:2, 1:2, [4 1])), [4, 1])
%!assert (size (discrete_rnd (1:2, 1:2, 4, 1)), [4, 1])

## Test class of input preserved
%!assert (class (discrete_rnd (1:2, 1:2)), "double")
%!assert (class (discrete_rnd (single (1:2), 1:2)), "single")
## FIXME: Maybe this should work, maybe it shouldn't.
#%!assert(class (discrete_rnd (1:2, single(1:2))), "single");

## Test input validation
%!error discrete_rnd ()
%!error discrete_rnd (1)
%!error discrete_rnd (1:2,1:2, -1)
%!error discrete_rnd (1:2,1:2, ones (2))
%!error discrete_rnd (1:2,1:2, [2 -1 2])
%!error discrete_rnd (1:2,1:2, 1, ones (2))
%!error discrete_rnd (1:2,1:2, 1, -1)
## test v,p verification
%!error discrete_rnd (1, ones (2), ones (2,1))
%!error discrete_rnd (1, ones (2,1), ones (1,1))
%!error discrete_rnd (1, ones (2,1), [1 -1])
%!error discrete_rnd (1, ones (2,1), [1 NaN])
%!error discrete_rnd (1, ones (2,1), [0  0])

