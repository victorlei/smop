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
## @deftypefn  {Function File} {} stdnormal_rnd (@var{r})
## @deftypefnx {Function File} {} stdnormal_rnd (@var{r}, @var{c}, @dots{})
## @deftypefnx {Function File} {} stdnormal_rnd ([@var{sz}])
## Return a matrix of random samples from the standard normal distribution
## (mean = 0, standard deviation = 1).
##
## When called with a single size argument, return a square matrix with
## the dimension specified.  When called with more than one scalar argument the
## first two arguments are taken as the number of rows and columns and any
## further arguments specify additional matrix dimensions.  The size may also
## be specified with a vector of dimensions @var{sz}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the standard normal distribution

function rnd = stdnormal_rnd (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    if (isscalar (varargin{1}) && varargin{1} >= 0)
      sz = [varargin{1}, varargin{1}];
    elseif (isrow (varargin{1}) && all (varargin{1} >= 0))
      sz = varargin{1};
    else
      error ("stdnormal_rnd: dimension vector must be row vector of non-negative integers");
    endif
  elseif (nargin > 1)
    if (any (cellfun (@(x) (! isscalar (x) || x < 0), varargin)))
      error ("stdnormal_rnd: dimensions must be non-negative integers");
    endif
    sz = [varargin{:}];
  endif

  rnd = randn (sz);

endfunction


%!assert (size (stdnormal_rnd (3)), [3, 3])
%!assert (size (stdnormal_rnd ([4 1])), [4, 1])
%!assert (size (stdnormal_rnd (4,1)), [4, 1])

## Test input validation
%!error stdnormal_rnd ()
%!error stdnormal_rnd (-1)
%!error stdnormal_rnd (ones (2))
%!error stdnormal_rnd ([2 -1 2])
%!error stdnormal_rnd (1, ones (2))
%!error stdnormal_rnd (1, -1)

