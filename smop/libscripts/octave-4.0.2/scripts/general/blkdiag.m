## Copyright (C) 2000-2015 Daniel Calvelo
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
## @deftypefn {Function File} {} blkdiag (@var{A}, @var{B}, @var{C}, @dots{})
## Build a block diagonal matrix from @var{A}, @var{B}, @var{C}, @dots{}
##
## All arguments must be numeric and either two-dimensional matrices or
## scalars.  If any argument is of type sparse, the output will also be sparse.
## @seealso{diag, horzcat, vertcat, sparse}
## @end deftypefn

## Author: Daniel Calvelo
## Modified by: William Poetra Yoga Hadisoeseno

function retval = blkdiag (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! all (cellfun ("isnumeric", varargin)))
    error ("blkdiag: all arguments must be numeric");
  endif

  ## Note: trailing singletons are automatically (correctly) ignored.
  if (! all (cellfun ("ndims", varargin) == 2))
    error ("blkdiag: all arguments must be two-dimensional matrices");
  endif

  ## size is an option for cellfun, but it's a bit different from
  ## calling size directly.
  tmp = cell2mat (cellfun (@size, varargin', "uniformoutput", false));
  csz = cumsum ([0 0; tmp], 1);

  if (any (cellfun ("issparse", varargin)))
    retval = sparse (csz(end,1), csz(end,2));
  else
    retval = zeros (csz(end,:));
  endif

  for p = 1:nargin
    vp = varargin{p};
    if (! isempty (vp))
      retval((csz(p,1)+1):csz(p+1,1),(csz(p,2)+1):csz(p+1,2)) = vp;
    endif
  endfor

endfunction


## regular tests
%!assert (blkdiag (1,ones (2),1), [1,0,0,0;0,1,1,0;0,1,1,0;0,0,0,1])
%!assert (blkdiag ([1,2],[3,4],[5,6]), [1,2,0,0,0,0;0,0,3,4,0,0;0,0,0,0,5,6])
%!assert (blkdiag ([1,2],[3;4],[5,6]), [1,2,0,0,0;0,0,3,0,0;0,0,4,0,0;0,0,0,5,6])
%!assert (blkdiag ([1,2;3,4],[5,6,7]), [1,2,0,0,0;3,4,0,0,0;0,0,5,6,7])
## tests involving empty matrices
%!assert (blkdiag ([],[],[]), [])
%!assert (blkdiag ([],[1,2;3,4],[],5,[]), [1,2,0;3,4,0;0,0,5])
%!assert (blkdiag (zeros (1,0,1),[1,2,3],1,0,5,zeros (0,1,1)), [0,0,0,0,0,0,0;1,2,3,0,0,0,0;0,0,0,1,0,0,0;0,0,0,0,0,0,0;0,0,0,0,0,5,0]);
## tests involving sparse matrices
%!assert (blkdiag (sparse ([1,2;3,4]),[5,6;7,8]), sparse ([1,2,0,0;3,4,0,0;0,0,5,6;0,0,7,8]))
%!assert (blkdiag (sparse ([1,2;3,4]),[5,6]), sparse ([1,2,0,0;3,4,0,0;0,0,5,6]))
## sanity checks
%!test
%! A = rand (round (rand (1, 2) * 10));
%! assert (blkdiag (A), A);

