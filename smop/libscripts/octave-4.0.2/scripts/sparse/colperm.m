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
## @deftypefn {Function File} {@var{p} =} colperm (@var{s})
## Return the column permutations such that the columns of
## @code{@var{s} (:, @var{p})} are ordered in terms of increasing number of
## nonzero elements.
##
## If @var{s} is symmetric, then @var{p} is chosen such that
## @code{@var{s} (@var{p}, @var{p})} orders the rows and columns with
## increasing number of nonzeros elements.
## @end deftypefn

function p = colperm (s)

  if (nargin != 1)
    print_usage ();
  endif

  [i, j] = find (s);
  idx = find (diff ([j; Inf]) != 0);
  [dummy, p] = sort (idx - [0; idx(1:(end-1))]);
endfunction

