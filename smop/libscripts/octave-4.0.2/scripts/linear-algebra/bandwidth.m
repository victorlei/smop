## Copyright (C) 2014-2015 Massimiliano Fasi
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
## @deftypefn  {Function File} {@var{bw} =} bandwidth (@var{A}, @var{type})
## @deftypefnx {Function File} {[@var{lower}, @var{upper}] =} bandwidth (@var{A})
## Compute the bandwidth of @var{A}.
##
## The @var{type} argument is the string @qcode{"lower"} for the lower
## bandwidth and @qcode{"upper"} for the upper bandwidth.  If no @var{type} is
## specified return both the lower and upper bandwidth of @var{A}.
##
## The lower/upper bandwidth of a matrix is the number of
## subdiagonals/superdiagonals with nonzero entries.
##
## @seealso{isbanded, isdiag, istril, istriu}
## @end deftypefn

## Author: Massimiliano Fasi

function [lower, upper] = bandwidth (A, type)

  if (! ((nargin == 1 && nargout == 2) || (nargin == 2 && nargout <= 1)))
    print_usage ();
  endif

  if (! isnumeric (A) && ! islogical (A) || ndims (A) != 2)
    error ("bandwidth: A must be a 2-D numeric or logical matrix");
  elseif (nargin == 2 && ! (strcmp (type, "lower") || strcmp (type, "upper")))
    error ('bandwidth: TYPE must be "lower" or "upper"');
  endif

  if (nargin == 1)
    [i, j] = find (A);
    if (isempty (i))
      lower = upper = 0;
    else
      lower = max (i - j);
      upper = max (j - i);
    endif
  else
    [i, j] = find (A);
    if (isempty (i))
      lower = 0;
    elseif (strcmp (type, "lower"))
      lower = max (i - j);
    else
      lower = max (j - i);
    endif
  endif

endfunction


%!test
%! [a,b] = bandwidth (speye (100));
%! assert ([a,b] == [0,0]);
%! assert (bandwidth (speye (100), "upper"), 0);
%! assert (bandwidth (speye (100), "lower"), 0);

%!test
%! A = [2 3 0 0 0; 1 2 3 0 0; 0 1 2 3 0; 0 0 1 2 3; 0 0 0 1 2];
%! [a,b] = bandwidth (A);
%! assert ([a,b] == [1,1]);
%! assert (bandwidth (A, "lower"), 1);
%! assert (bandwidth (A, "upper"), 1);

%!assert (bandwidth ([], "lower"), 0)
%!assert (bandwidth ([], "upper"), 0)
%!assert (bandwidth (zeros (3,3), "lower"), 0)
%!assert (bandwidth (zeros (3,3), "upper"), 0)
%!assert (bandwidth (ones (5,5), "lower"), 4)
%!assert (bandwidth (ones (5,5), "upper"), 4)

%!test
%! [a,b] = bandwidth ([]);
%! assert ([a,b] == [0,0]);
%!test
%! [a,b] = bandwidth (zeros (3,3));
%! assert ([a,b] == [0,0]);
%!test
%! [a,b] = bandwidth (ones (5,5));
%! assert ([a,b] == [4,4]);

## Test input validation
%!error bandwidth ()
%!error bandwidth (1,2,3)
%!error [a,b,c] = bandwidth (ones (2))
%!error [a,b] = bandwidth (ones (2), "upper")
%!error <A must be a 2-D numeric or logical> bandwidth ("string", "lower")
%!error <A must be a 2-D numeric or logical> bandwidth (ones (3,3,3), "lower")
%!error <TYPE must be "lower" or "upper"> bandwidth (ones (2), "uper")
%!error <TYPE must be "lower" or "upper"> bandwidth (ones (2), "uppper")

