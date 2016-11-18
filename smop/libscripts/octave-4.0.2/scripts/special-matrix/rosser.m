## Copyright (C) 1999-2015 Peter Ekberg
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
## @deftypefn {Function File} {} rosser ()
## Return the Rosser matrix.
##
## This is a difficult test case used to evaluate eigenvalue algorithms.
## @seealso{wilkinson, eig}
## @end deftypefn

## Author: Peter Ekberg
##         (peda)

function retval = rosser ()

  if (nargin != 0)
    print_usage ();
  endif

  retval = [611,   196,  -192,   407,    -8,   -52,   -49,    29;
            196,   899,   113,  -192,   -71,   -43,    -8,   -44;
           -192,   113,   899,   196,    61,    49,     8,    52;
            407,  -192,   196,   611,     8,    44,    59,   -23;
             -8,   -71,    61,     8,   411,  -599,   208,   208;
            -52,   -43,    49,    44,  -599,   411,   208,   208;
            -49,    -8,     8,    59,   208,   208,    99,  -911;
             29,   -44,    52,   -23,   208,   208,  -911,    99];

endfunction


%!assert (size (rosser ()), [8,8])
%!assert (rosser ()([1, end]), [611, 99])

%!error (rosser (1))

