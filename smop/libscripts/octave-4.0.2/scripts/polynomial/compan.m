## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn {Function File} {} compan (@var{c})
## Compute the companion matrix corresponding to polynomial coefficient vector
## @var{c}.
##
## The companion matrix is
## @tex
## $$
## A = \left[\matrix{
##  -c_2/c_1 & -c_3/c_1 & \cdots & -c_N/c_1 & -c_{N+1}/c_1\cr
##      1    &     0    & \cdots &     0    &         0   \cr
##      0    &     1    & \cdots &     0    &         0   \cr
##   \vdots  &   \vdots & \ddots &  \vdots  &      \vdots \cr
##      0    &     0    & \cdots &     1    &         0}\right].
## $$
## @end tex
## @ifnottex
## @c Set example in small font to prevent overfull line
##
## @smallexample
## @group
##      _                                                        _
##     |  -c(2)/c(1)   -c(3)/c(1)  @dots{}  -c(N)/c(1)  -c(N+1)/c(1)  |
##     |       1            0      @dots{}       0             0      |
##     |       0            1      @dots{}       0             0      |
## A = |       .            .      .         .             .      |
##     |       .            .       .        .             .      |
##     |       .            .        .       .             .      |
##     |_      0            0      @dots{}       1             0     _|
## @end group
## @end smallexample
##
## @end ifnottex
## The eigenvalues of the companion matrix are equal to the roots of the
## polynomial.
## @seealso{roots, poly, eig}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function A = compan (c)

  if (nargin != 1)
    print_usage ();
  endif

  if (! isvector (c))
    error ("compan: expecting a vector argument");
  endif

  n = length (c);

  if (n == 1)
    A = [];
  else
    A = diag (ones (n-2, 1), -1);
    A(1,:) = -c(2:n) / c(1);
  endif

endfunction


%!assert (compan ([1, 2, 3]), [-2, -3; 1, 0])
%!assert (compan ([1; 2; 3]), [-2, -3; 1, 0])
%!assert (isempty (compan (4)))
%!assert (compan ([3, 2, 1]), [-2/3, -1/3; 1, 0])

%!error compan ([1,2;3,4])
%!error compan ([])

