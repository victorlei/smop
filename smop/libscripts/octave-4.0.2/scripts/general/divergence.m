## Copyright (C) 2009-2015 Kai Habel
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
## @deftypefn  {Function File} {@var{div} =} divergence (@var{x}, @var{y}, @var{z}, @var{fx}, @var{fy}, @var{fz})
## @deftypefnx {Function File} {@var{div} =} divergence (@var{fx}, @var{fy}, @var{fz})
## @deftypefnx {Function File} {@var{div} =} divergence (@var{x}, @var{y}, @var{fx}, @var{fy})
## @deftypefnx {Function File} {@var{div} =} divergence (@var{fx}, @var{fy})
## Calculate divergence of a vector field given by the arrays @var{fx},
## @var{fy}, and @var{fz} or @var{fx}, @var{fy} respectively.
##
## @tex
## $$
## div F(x,y,z) = \partial_x{F} + \partial_y{F} + \partial_z{F}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##                   d               d               d
## div F(x,y,z)  =   -- F(x,y,z)  +  -- F(x,y,z)  +  -- F(x,y,z)
##                   dx              dy              dz
## @end group
## @end example
##
## @end ifnottex
## The coordinates of the vector field can be given by the arguments @var{x},
## @var{y}, @var{z} or @var{x}, @var{y} respectively.
##
## @seealso{curl, gradient, del2, dot}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function retval = divergence (varargin)

  fidx = 1;
  if (nargin == 2)
    sz = size (varargin{fidx});
    dx = (1:sz(2))(:);
    dy = (1:sz(1))(:);
  elseif (nargin == 3)
    sz = size (varargin{fidx});
    dx = 1:sz(2);
    dy = 1:sz(1);
    dz = 1:sz(3);
  elseif (nargin == 4)
    fidx = 3;
    dx = varargin{1}(1,:);
    dy = varargin{2}(:,1);
  elseif (nargin == 6)
    fidx = 4;
    dx = varargin{1}(1,:,1)(:);
    dy = varargin{2}(:,1,1)(:);
    dz = varargin{3}(1,1,:)(:);
  else
    print_usage ();
  endif

  if (nargin == 4 || nargin == 2)
    if (! size_equal (varargin{fidx},varargin{fidx + 1}))
      error ("divergence: size of X and Y must match");
    elseif (ndims (varargin{fidx}) != 2)
      error ("divergence: expected two-dimensional matrices X and Y");
    elseif (length (dx) != columns (varargin{fidx})
            || length (dy) != rows (varargin{fidx}))
      error ("divergence: size of dx and dy must match the respective dimension of X and Y");
    endif

    retval = gradient (varargin{fidx}, dx, dy);
    retval += gradient (varargin{fidx + 1}.', dy, dx).';

  elseif (nargin == 6 || nargin == 3)
    if (! size_equal (varargin{fidx},varargin{fidx + 1},varargin{fidx + 2}))
      error ("divergence: size of X, Y, and Z must match");
    elseif (ndims (varargin{fidx}) != 3)
      error ("divergence: expected two-dimensional matrices X, Y, and Z");
    elseif ((length (dx) != size (varargin{fidx}, 2))
         || (length (dy) != size (varargin{fidx}, 1))
         || (length (dz) != size (varargin{fidx}, 3)))
      error ("divergence: size of dx, dy, and dz must match the respective dimesion of X, Y, and Z");
    endif

    ## x-direction
    retval = gradient (varargin{fidx}, dx, dy, dz);
    ## y-direction
    retval += shiftdim (gradient (shiftdim (varargin{fidx + 1}, 2), dy), 1);
    ## z-direction
    retval += shiftdim (gradient (shiftdim (varargin{fidx + 2}, 1), dz), 2);
  endif

endfunction


%!test
%! [X,Y] = meshgrid (-20:20,-22:22);
%! div = divergence (X-Y,Y);
%! assert (all (div(:) == 2));
%! assert (size_equal (X,Y,div));

