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
## @deftypefn {Function File} {@var{y} =} spfun (@var{f}, @var{S})
## Compute @code{f(@var{S})} for the nonzero values of @var{S}.
##
## This results in a sparse matrix with the same structure as @var{S}.  The
## function @var{f} can be passed as a string, a function handle, or an
## inline function.
## @seealso{arrayfun, cellfun, structfun}
## @end deftypefn

function y = spfun (f, S)

  if (nargin != 2)
    print_usage ();
  endif

  [i, j, v] = find (S);
  [m, n] = size (S);

  if (isa (f, "function_handle") || isa (f, "inline function"))
    y = sparse (i, j, f(v), m, n);
  else
    y = sparse (i, j, feval (f, v), m, n);
  endif

endfunction


%!assert (spfun ("exp", [1,2;3,0]), sparse ([exp(1),exp(2);exp(3),0]))
%!assert (spfun ("exp", sparse ([1,2;3,0])), sparse ([exp(1),exp(2);exp(3),0]))
%!assert (spfun (@exp, [1,2;3,0]), sparse ([exp(1),exp(2);exp(3),0]))
%!assert (spfun (@exp, sparse ([1,2;3,0])), sparse ([exp(1),exp(2);exp(3),0]))

