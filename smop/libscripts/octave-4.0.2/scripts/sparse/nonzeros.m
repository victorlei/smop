## Copyright (C) 2004-2015 Paul Kienzle
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
## @deftypefn {Function File} {} nonzeros (@var{s})
## Return a vector of the nonzero values of the sparse matrix @var{s}.
## @seealso{find, nnz}
## @end deftypefn

function t = nonzeros (s)

  if (nargin != 1)
    print_usage ();
  endif

  [~, ~, t] = find (s);

  t = t(:);

endfunction


%!assert (nonzeros ([1,2;3,0]), [1;3;2])
%!assert (nonzeros ([1,2,3,0]), [1;2;3])
%!assert (nonzeros (sparse ([1,2;3,0])), [1;3;2])
%!assert (nonzeros (sparse ([1,2,3,0])), [1;2;3])

