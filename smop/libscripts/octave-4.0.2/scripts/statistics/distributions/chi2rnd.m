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
## @deftypefn  {Function File} {} chi2rnd (@var{n})
## @deftypefnx {Function File} {} chi2rnd (@var{n}, @var{r})
## @deftypefnx {Function File} {} chi2rnd (@var{n}, @var{r}, @var{c}, @dots{})
## @deftypefnx {Function File} {} chi2rnd (@var{n}, [@var{sz}])
## Return a matrix of random samples from the chi-square distribution with
## @var{n} degrees of freedom.
##
## When called with a single size argument, return a square matrix with
## the dimension specified.  When called with more than one scalar argument the
## first two arguments are taken as the number of rows and columns and any
## further arguments specify additional matrix dimensions.  The size may also
## be specified with a vector of dimensions @var{sz}.
##
## If no size arguments are given then the result matrix is the size of
## @var{n}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the chi-square distribution

function rnd = chi2rnd (n, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    sz = size (n);
  elseif (nargin == 2)
    if (isscalar (varargin{1}) && varargin{1} >= 0)
      sz = [varargin{1}, varargin{1}];
    elseif (isrow (varargin{1}) && all (varargin{1} >= 0))
      sz = varargin{1};
    else
      error ("chi2rnd: dimension vector must be row vector of non-negative integers");
    endif
  elseif (nargin > 2)
    if (any (cellfun (@(x) (! isscalar (x) || x < 0), varargin)))
      error ("chi2rnd: dimensions must be non-negative integers");
    endif
    sz = [varargin{:}];
  endif

  if (! isscalar (n) && ! isequal (size (n), sz))
    error ("chi2rnd: N must be scalar or of size SZ");
  endif

  if (iscomplex (n))
    error ("chi2rnd: N must not be complex");
  endif

  if (isa (n, "single"))
    cls = "single";
  else
    cls = "double";
  endif

  if (isscalar (n))
    if ((n > 0) && (n < Inf))
      rnd = 2 * randg (n/2, sz, cls);
    else
      rnd = NaN (sz, cls);
    endif
  else
    rnd = NaN (sz, cls);

    k = (n > 0) | (n < Inf);
    rnd(k) = 2 * randg (n(k)/2, cls);
  endif

endfunction


%!assert (size (chi2rnd (2)), [1, 1])
%!assert (size (chi2rnd (ones (2,1))), [2, 1])
%!assert (size (chi2rnd (ones (2,2))), [2, 2])
%!assert (size (chi2rnd (1, 3)), [3, 3])
%!assert (size (chi2rnd (1, [4 1])), [4, 1])
%!assert (size (chi2rnd (1, 4, 1)), [4, 1])

## Test class of input preserved
%!assert (class (chi2rnd (2)), "double")
%!assert (class (chi2rnd (single (2))), "single")
%!assert (class (chi2rnd (single ([2 2]))), "single")

## Test input validation
%!error chi2rnd ()
%!error chi2rnd (ones (3), ones (2))
%!error chi2rnd (ones (2), ones (3))
%!error chi2rnd (i)
%!error chi2rnd (1, -1)
%!error chi2rnd (1, ones (2))
%!error chi2rnd (1, [2 -1 2])
%!error chi2rnd (ones (2,2), 3)
%!error chi2rnd (ones (2,2), [3, 2])
%!error chi2rnd (ones (2,2), 2, 3)

