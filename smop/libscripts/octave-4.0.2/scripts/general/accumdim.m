## Copyright (C) 2010-2015 VZLU Prague
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
## @deftypefn {Function File} {} accumdim (@var{subs}, @var{vals}, @var{dim}, @var{n}, @var{func}, @var{fillval})
## Create an array by accumulating the slices of an array into the
## positions defined by their subscripts along a specified dimension.
##
## The subscripts are defined by the index vector @var{subs}.
## The dimension is specified by @var{dim}.  If not given, it defaults
## to the first non-singleton dimension.  The length of @var{subs} must
## be equal to @code{size (@var{vals}, @var{dim})}.
##
## The extent of the result matrix in the working dimension will be
## determined by the subscripts themselves.  However, if @var{n} is
## defined it determines this extent.
##
## The default action of @code{accumdim} is to sum the subarrays with the
## same subscripts.  This behavior can be modified by defining the
## @var{func} function.  This should be a function or function handle
## that accepts an array and a dimension, and reduces the array along
## this dimension.  As a special exception, the built-in @code{min} and
## @code{max} functions can be used directly, and @code{accumdim}
## accounts for the middle empty argument that is used in their calling.
##
## The slices of the returned array that have no subscripts associated
## with them are set to zero.  Defining @var{fillval} to some other
## value allows these values to be defined.
##
## An example of the use of @code{accumdim} is:
##
## @example
## @group
## accumdim ([1, 2, 1, 2, 1], [ 7, -10,   4;
##                             -5, -12,   8;
##                            -12,   2,   8;
##                            -10,   9,  -3;
##                             -5,  -3, -13])
## @result{} [-10,-11,-1;-15,-3,5]
## @end group
## @end example
##
## @seealso{accumarray}
## @end deftypefn

function A = accumdim (subs, vals, dim, n = 0, func = [], fillval = 0)

  if (nargin < 2 || nargin > 6)
    print_usage ();
  endif

  if (isempty (fillval))
    fillval = 0;
  endif

  if (! isvector (subs))
    error ("accumdim: SUBS must be a subscript vector");
  elseif (! isindex (subs)) # creates index cache
    error ("accumdim: indices must be positive integers");
  else
    m = max (subs);
    if (n == 0 || isempty (n))
      n = m;
    elseif (n < m)
      error ("accumdim: N index out of range");
    endif
  endif

  sz = size (vals);

  if (nargin < 3)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  elseif (! isindex (dim))
    error ("accumdim: DIM must be a valid dimension");
  elseif (dim > length (sz))
    sz(end+1:dim) = 1;
  endif
  sz(dim) = n;

  if (length (subs) != size (vals, dim))
    error ("accumdim: dimension mismatch");
  endif

  if (isempty (func) || func == @sum)
    ## Fast summation case.
    A = __accumdim_sum__ (subs, vals, dim, n);

    ## Fill in nonzero fill value
    if (fillval != 0)
      mask = true (n, 1);
      mask(subs) = false;
      subsc = {':'}(ones (1, length (sz)));
      subsc{dim} = mask;
      A(subsc{:}) = fillval;
    endif
    return;
  endif

  ## The general case.
  ns = length (subs);
  ## Sort indices.
  [subs, idx] = sort (subs(:));
  ## Identify runs.
  jdx = find (subs(1:ns-1) != subs(2:ns));
  jdx = [jdx; ns];
  ## Collect common slices.
  szc = num2cell (sz);
  szc{dim} = diff ([0; jdx]);
  subsc = {':'}(ones (1, length (sz)));
  subsc{dim} = idx;
  vals = mat2cell (vals(subsc{:}), szc{:});
  ## Apply reductions. Special case min, max.
  if (func == @min || func == @max)
    vals = cellfun (func, vals, {[]}, {dim}, "uniformoutput", false);
  else
    vals = cellfun (func, vals, {dim}, "uniformoutput", false);
  endif
  subs = subs(jdx);

  ## Concatenate reduced slices.
  vals = cat (dim, vals{:});

  ## Construct matrix of fillvals.
  if (fillval == 0)
    A = zeros (sz, class (vals));
  else
    A = repmat (fillval, sz);
  endif

  ## Set the reduced values.
  subsc{dim} = subs;
  A(subsc{:}) = vals;

endfunction


## Test accumdim vs. accumarray
%!shared a
%! a = rand (5, 5, 5);

%!assert (accumdim ([1;3;1;3;3], a)(:,2,3), accumarray ([1;3;1;3;3], a(:,2,3)))
%!assert (accumdim ([2;3;2;2;2], a, 2, 4)(4,:,2), accumarray ([2;3;2;2;2], a(4,:,2), [1,4]))
%!assert (accumdim ([2;3;2;1;2], a, 3, 3, @min)(1,5,:), accumarray ([2;3;2;1;2], a(1,5,:), [1,1,3], @min))
%!assert (accumdim ([1;3;2;2;1], a, 2, 3, @median)(4,:,5), accumarray ([1;3;2;2;1], a(4,:,5), [1,3], @median))

## Test fillval
%!assert (accumdim ([1;3;1;3;3], a)(2,:,:), zeros (1,5,5))
%!assert (accumdim ([1;3;1;3;3], a, 1, 4)([2 4],:,:), zeros (2,5,5))
%!assert (accumdim ([1;3;1;3;3], a, 1, 4, [], pi)([2 4],:,:), pi (2,5,5))

## Test input validation
%!error accumdim (1)
%!error accumdim (1,2,3,4,5,6,7)
%!error <SUBS must be a subscript vector> accumdim (ones (2,2), ones (2,2))
%!error <indices must be positive integers> accumdim ([-1 1], ones (2,2))
%!error <N index out of range> accumdim ([1 2], ones (2,2), 1, 1)
%!error <dimension mismatch> accumdim ([1], ones (2,2))

