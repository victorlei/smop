## Copyright (C) 2005-2015 Ivana Varekova
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
## @deftypefn  {Function File} {} etreeplot (@var{A})
## @deftypefnx {Function File} {} etreeplot (@var{A}, @var{node_style}, @var{edge_style})
## Plot the elimination tree of the matrix @var{A} or
## @tcode{@var{A}+@var{A}'} if @var{A} in not symmetric.
##
## The optional parameters @var{node_style} and @var{edge_style} define the
## output style.
## @seealso{treeplot, gplot}
## @end deftypefn

function etreeplot (A, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  treeplot (etree (A+A'), varargin{:});
endfunction

