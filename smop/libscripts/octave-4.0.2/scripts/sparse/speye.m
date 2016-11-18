## Copyright (C) 2004-2015 David Bateman and Andy Adler
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
## @deftypefn  {Function File} {@var{s} =} speye (@var{m}, @var{n})
## @deftypefnx {Function File} {@var{s} =} speye (@var{m})
## @deftypefnx {Function File} {@var{s} =} speye (@var{sz})
## Return a sparse identity matrix of size @var{m}x@var{n}.
##
## The implementation is significantly more efficient than
## @code{sparse (eye (@var{m}))} as the full matrix is not constructed.
##
## Called with a single argument a square matrix of size
## @var{m}-by-@var{m} is created.  If called with a single vector argument
## @var{sz}, this argument is taken to be the size of the matrix to create.
## @seealso{sparse, spdiags, eye}
## @end deftypefn

function s = speye (m, n)
  if (nargin == 1)
    if (isvector (m) && length (m) == 2)
      n = m(2);
      m = m(1);
    elseif (isscalar (m))
      n = m;
    else
      error ("speye: invalid matrix dimension");
    endif
  else
    if (! isscalar (m) || ! isscalar (n))
      error ("speye: invalid matrix dimension");
    endif
  endif

  lo = min ([m, n]);
  s = sparse (1:lo, 1:lo, 1, m, n);
endfunction


%!assert (issparse (speye (4)))
%!assert (speye (4), sparse (1:4,1:4,1))
%!assert (speye (2,4), sparse (1:2,1:2,1,2,4))
%!assert (speye (4,2), sparse (1:2,1:2,1,4,2))
%!assert (speye ([4,2]), sparse (1:2,1:2,1,4,2))

