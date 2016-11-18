## Copyright (C) 1995-2015 Kurt Hornik
## Copyright (C) 2009 VZLU Prague
## Copyright (C) 2009 Jaroslav Hajek
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
## @deftypefn {Function File} {[@var{err}, @var{y1}, @dots{}] =} common_size (@var{x1}, @dots{})
## Determine if all input arguments are either scalar or of common size.
##
## If true, @var{err} is zero, and @var{yi} is a matrix of the common size
## with all entries equal to @var{xi} if this is a scalar or @var{xi}
## otherwise.  If the inputs cannot be brought to a common size, @var{err} is
## 1, and @var{yi} is @var{xi}.  For example:
##
## @example
## @group
## [errorcode, a, b] = common_size ([1 2; 3 4], 5)
##      @result{} errorcode = 0
##      @result{} a = [ 1, 2; 3, 4 ]
##      @result{} b = [ 5, 5; 5, 5 ]
## @end group
## @end example
##
## @noindent
## This is useful for implementing functions where arguments can either be
## scalars or of common size.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 15 October 1994
## Adapted-By: jwe
## Optimized-By: Jaroslav Hajek

function [errorcode, varargout] = common_size (varargin)

  if (nargin < 2)
    error ("common_size: only makes sense if nargin >= 2");
  endif

  ## Find scalar args.
  nscal = cellfun ("numel", varargin) != 1;

  i = find (nscal, 1);

  if (isempty (i))
    errorcode = 0;
    varargout = varargin;
  else
    match = cellfun ("size_equal", varargin, varargin(i));
    if (any (nscal &! match))
      errorcode = 1;
      varargout = varargin;
    else
      errorcode = 0;
      if (nargout > 1)
        scal = ! nscal;
        varargout = varargin;
        if (any (nscal))
          dims = size (varargin{find (nscal, 1)});
          subs = arrayfun (@ones, 1, dims, "uniformoutput", false);
          varargout(scal) = cellindexmat (varargin(scal), subs{:});
        endif
      endif
    endif
  endif
endfunction


%!test
%! m = [1,2;3,4];
%! [err, a, b, c] = common_size (m, 3, 5);
%! assert (err, 0);
%! assert (a, m);
%! assert (b, [3,3;3,3]);
%! assert (c, [5,5;5,5]);

%!error common_size ()

