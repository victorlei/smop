## Copyright (C) 2001-2015 Paul Kienzle
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
## @deftypefn  {Function File} {@var{y} =} filter2 (@var{b}, @var{x})
## @deftypefnx {Function File} {@var{y} =} filter2 (@var{b}, @var{x}, @var{shape})
## Apply the 2-D FIR filter @var{b} to @var{x}.
##
## If the argument @var{shape} is specified, return an array of the desired
## shape.  Possible values are:
##
## @table @asis
## @item @qcode{"full"}
## pad @var{x} with zeros on all sides before filtering.
##
## @item @qcode{"same"}
## unpadded @var{x} (default)
##
## @item @qcode{"valid"}
## trim @var{x} after filtering so edge effects are no included.
## @end table
##
## Note this is just a variation on convolution, with the parameters reversed
## and @var{b} rotated 180 degrees.
## @seealso{conv2}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
## 2001-02-08
##    * initial release

function y = filter2 (b, x, shape)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif
  if (nargin < 3)
    shape = "same";
  endif

  [nr, nc] = size (b);
  y = conv2 (x, b(nr:-1:1, nc:-1:1), shape);
endfunction

