## Copyright (C) 2012-2015 Carnë Draug
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
## @deftypefn {Function File} {} narginchk (@var{minargs}, @var{maxargs})
## Check for correct number of input arguments.
##
## Generate an error message if the number of arguments in the calling function
## is outside the range @var{minargs} and @var{maxargs}.  Otherwise, do nothing.
##
## Both @var{minargs} and @var{maxargs} must be scalar numeric values.  Zero,
## Inf, and negative values are all allowed, and @var{minargs} and @var{maxargs}
## may be equal.
##
## Note that this function evaluates @code{nargin} on the caller.
##
## @seealso{nargoutchk, error, nargout, nargin}
## @end deftypefn

## Author: Carnë Draug <carandraug+dev@gmail.com>

function narginchk (minargs, maxargs)

  if (nargin != 2)
    print_usage;
  elseif (! isnumeric (minargs) || ! isscalar (minargs))
    error ("minargs must be a numeric scalar");
  elseif (! isnumeric (maxargs) || ! isscalar (maxargs))
    error ("maxargs must be a numeric scalar");
  elseif (minargs > maxargs)
    error ("minargs cannot be larger than maxargs");
  endif

  args = evalin ("caller", "nargin;");

  if (args < minargs)
    error ("not enough input arguments");
  elseif (args > maxargs)
    error ("too many input arguments");
  endif

endfunction


%!function f (nargs, varargin)
%! narginchk (nargs(1), nargs(2));
%!endfunction

%!error <too many input arguments> f([0,0])
%!error <not enough input arguments> f([3, 3], 1)

%!test
%! f([1,1])
%!test
%! f([1,5], 2, 3, 4, 5)

