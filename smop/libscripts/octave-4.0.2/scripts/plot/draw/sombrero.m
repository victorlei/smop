## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn  {Function File} {} sombrero ()
## @deftypefnx {Function File} {} sombrero (@var{n})
## @deftypefnx {Function File} {@var{z} =} sombrero (@dots{})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} sombrero (@dots{})
## Plot the familiar 3-D sombrero function.
##
## The function plotted is
## @tex
## $$z = { \rm{sin} (\sqrt {(x^2 + y^2)}) \over \sqrt {(x^2 + y^2)} }$$
## @end tex
## @ifnottex
##
## @example
## z = sin (sqrt (x^2 + y^2)) / (sqrt (x^2 + y^2))
## @end example
##
## @end ifnottex
## Called without a return argument, @code{sombrero} plots the surface of the
## above function over the meshgrid [-8,8] using @code{surf}.
##
## If @var{n} is a scalar the plot is made with @var{n} grid lines.
## The default value for @var{n} is 41.
##
## When called with output arguments, return the data for the function
## evaluated over the meshgrid.  This can subsequently be plotted with
## @code{surf (@var{x}, @var{y}, @var{z})}.
##
## @seealso{peaks, meshgrid, mesh, surf}
## @end deftypefn

## Author: jwe

function [x, y, z] = sombrero (n = 41)

  if (nargin > 2)
    print_usage ();
  elseif (n <= 1)
    error ("sombrero: number of grid lines N must be greater than 1");
  endif

  [xx, yy] = meshgrid (linspace (-8, 8, n));
  r = sqrt (xx.^2 + yy.^2) + eps;  # eps prevents div/0 errors
  zz = sin (r) ./ r;

  if (nargout == 0)
    surf (xx, yy, zz);
  elseif (nargout == 1)
    x = zz;
  else
    x = xx;
    y = yy;
    z = zz;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! sombrero ();
%! title ('sombrero() function');

## Test input validation
%!error sombrero (1,2,3)
%!error <N must be greater than 1> sombrero (1)

