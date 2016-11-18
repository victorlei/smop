## Copyright (C) 2005-2015 Laurent Mazet
## Copyright (C) 2010 Jaroslav Hajek
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
## @deftypefn {Function File} {@var{m} =} cell2mat (@var{c})
## Convert the cell array @var{c} into a matrix by concatenating all
## elements of @var{c} into a hyperrectangle.
##
## Elements of @var{c} must be numeric, logical, or char matrices; or cell
## arrays; or structs; and @code{cat} must be able to concatenate them together.
## @seealso{mat2cell, num2cell}
## @end deftypefn

function m = cell2mat (c)

  if (nargin != 1)
    print_usage ();
  endif

  nb = numel (c);

  if (nb == 0)
    m = [];
  else
    if (! iscell (c))
      error ("cell2mat: C must be a cell array");
    endif

    ## Check first for valid matrix types
    valid = cellfun ("isnumeric", c);
    valid = cellfun ("islogical", c(! valid));
    valid = cellfun ("isclass", c(! valid), "char");
    if (! all (valid(:)))
      valid = cellfun ("isclass", c, "cell");
      if (! all (valid(:)))
        valid = cellfun ("isclass", c, "struct");
        if (! all (valid(:)))
          error ("cell2mat: wrong type elements or mixed cells, structs, and matrices");
        endif
      endif
    endif

    sz = size (c);
    if (all (cellfun ("numel", c)(:) == 1))
      ## Special case of all scalars
      m = reshape (cat (1, c{:}), sz);
    else

      ## The goal is to minimize the total number of cat() calls.
      ## The dimensions can be concatenated along in arbitrary order.
      ## The numbers of concatenations are:
      ## n / d1
      ## n / (d1 * d2)
      ## n / (d1 * d2 * d3)
      ## etc.
      ## This is minimized if d1 >= d2 >= d3...

      nd = ndims (c);
      [~, isz] = sort (sz, "descend");
      for idim = isz
        if (sz(idim) == 1)
          continue;
        endif
        xdim = [1:idim-1, idim+1:nd];
        cc = num2cell (c, xdim);
        c = cellfun ("cat", {idim}, cc{:}, "uniformoutput", false);
      endfor
      m = c{1};

    endif
  endif

endfunction


%!demo
%! C = {[1], [2 3 4]; [5; 9], [6 7 8; 10 11 12]};
%! cell2mat (C)

%!assert (cell2mat ({}), []);
%!assert (cell2mat ([]), []);
%!test
%! C = {[1], [2 3 4]; [5; 9], [6 7 8; 10 11 12]};
%! D = C; D(:,:,2) = C;
%! E = [1 2 3 4; 5 6 7 8; 9 10 11 12];
%! F = E; F(:,:,2) = E;
%! assert (cell2mat (C), E);
%! assert (cell2mat (D), F);
%!test
%! m = rand (10) + i * rand (10);
%! c = mat2cell (m, [1 2 3 4], [4 3 2 1]);
%! assert (cell2mat (c), m);
%!test
%! m = int8 (256*rand (4, 5, 6, 7, 8));
%! c = mat2cell (m, [1 2 1], [1 2 2], [3 1 1 1], [4 1 2], [3 1 4]);
%! assert (cell2mat (c), m);
%!test
%! m = {1, 2, 3};
%! assert (cell2mat (mat2cell (m, 1, [1 1 1])), m);

%!error cell2mat ()
%!error cell2mat (1,2)
%!error <C must be a cell array> cell2mat ([1,2])
%!error <mixed cells, structs, and matrices> cell2mat ({[1], struct()})
%!error <mixed cells, structs, and matrices> cell2mat ({[1], {1}})
%!error <mixed cells, structs, and matrices> cell2mat ({struct(), {1}})

