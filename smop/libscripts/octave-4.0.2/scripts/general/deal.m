## Copyright (C) 1998-2015 Ariel Tankus
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
## @deftypefn  {Function File} {[@var{r1}, @var{r2}, @dots{}, @var{rn}] =} deal (@var{a})
## @deftypefnx {Function File} {[@var{r1}, @var{r2}, @dots{}, @var{rn}] =} deal (@var{a1}, @var{a2}, @dots{}, @var{an})
##
## Copy the input parameters into the corresponding output parameters.
##
## If only a single input parameter is supplied, its value is copied to each
## of the outputs.
##
## For example,
##
## @example
## [a, b, c] = deal (x, y, z);
## @end example
##
## @noindent
## is equivalent to
##
## @example
## @group
## a = x;
## b = y;
## c = z;
## @end group
## @end example
##
## @noindent
## and
##
## @example
## [a, b, c] = deal (x);
## @end example
##
## @noindent
## is equivalent to
##
## @example
## a = b = c = x;
## @end example
##
## Programming Note: @code{deal} is often used with comma separated lists
## derived from cell arrays or structures.  This is unnecessary as the
## interpreter can perform the same action without the overhead of a function
## call.  For example:
##
## @example
## @group
## c = @{[1 2], "Three", 4@};
## [x, y, z ] = c@{:@}
## @result{}
##    x =
##
##       1   2
##
##    y = Three
##    z =  4
## @end group
## @end example
## @seealso{cell2struct, struct2cell, repmat}
## @end deftypefn

## Author: Ariel Tankus
## Author: Paul Kienzle and Etienne Grossman
## Created: 13.11.98
## Adapted-by: jwe

function [varargout] = deal (varargin)

  if (nargin == 0)
    print_usage ();
  elseif (nargin == 1 || nargin == nargout)
    varargout(1:nargout) = varargin;
  else
    error ("deal: nargin > 1 and nargin != nargout");
  endif

endfunction


%!test
%! [a,b] = deal (1,2);
%! assert (a, 1);
%! assert (b, 2);
%!test
%! [a,b] = deal (1);
%! assert (a, 1);
%! assert (b, 1);

%!error deal ()
%!error <nargin . 1 and nargin != nargout> y = deal (1, 2)

