## Copyright (C) 2008-2015 Jaroslav Hajek
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
## @deftypefn {Function File} {} __fdjac__ (@var{fcn}, @var{x}, @var{fvec}, @var{err})
## Undocumented internal function.
## @end deftypefn

function fjac = __fdjac__ (fcn, x, fvec, typicalx, cdif, err = 0)
  if (cdif)
    err = (max (eps, err)) ^ (1/3);
    h = typicalx*err;
    fjac = zeros (length (fvec), numel (x));
    for i = 1:numel (x)
      x1 = x2 = x;
      x1(i) += h(i);
      x2(i) -= h(i);
      fjac(:,i) = (fcn (x1)(:) - fcn (x2)(:)) / (x1(i) - x2(i));
    endfor
  else
    err = sqrt (max (eps, err));
    h = typicalx*err;
    fjac = zeros (length (fvec), numel (x));
    for i = 1:numel (x)
      x1 = x;
      x1(i) += h(i);
      fjac(:,i) = (fcn (x1)(:) - fvec) / (x1(i) - x(i));
    endfor
  endif
endfunction



