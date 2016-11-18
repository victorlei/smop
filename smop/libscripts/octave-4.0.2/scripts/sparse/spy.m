## Copyright (C) 1998-2015 Andy Adler
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
## @deftypefn  {Function File} {} spy (@var{x})
## @deftypefnx {Function File} {} spy (@dots{}, @var{markersize})
## @deftypefnx {Function File} {} spy (@dots{}, @var{line_spec})
## Plot the sparsity pattern of the sparse matrix @var{x}.
##
## If the argument @var{markersize} is given as a scalar value, it is used to
## determine the point size in the plot.
##
## If the string @var{line_spec} is given it is passed to @code{plot} and
## determines the appearance of the plot.
## @seealso{plot, gplot}
## @end deftypefn

function spy (x, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  markersize = NaN;
  if (nnz (x) < 1000)
    line_spec = "*";
  else
    line_spec = ".";
  endif
  for i = 1:length (varargin)
    if (ischar (varargin{i}))
      if (length (varargin{i}) == 1)
        line_spec = [line_spec, varargin{i}];
      else
        line_spec = varargin{i};
      endif
    elseif (isscalar (varargin{i}))
      markersize = varargin{i};
    else
      error ("spy: expected markersize or linespec");
    endif
  endfor

  [i, j, s] = find (x);
  [m, n] = size (x);

  if (isnan (markersize))
    plot (j, i, line_spec);
  else
    plot (j, i, line_spec, "markersize", markersize);
  endif

  axis ([0, n+1, 0, m+1], "ij");

endfunction


%!demo
%! clf;
%! spy (sprand (10,10, 0.2));

## Mark graphical function as tested by demo block
%!assert (1)

