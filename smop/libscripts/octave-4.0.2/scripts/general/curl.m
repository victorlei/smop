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
## @deftypefn  {Function File} {[@var{cx}, @var{cy}, @var{cz}, @var{v}] =} curl (@var{x}, @var{y}, @var{z}, @var{fx}, @var{fy}, @var{fz})
## @deftypefnx {Function File} {[@var{cz}, @var{v}] =} curl (@var{x}, @var{y}, @var{fx}, @var{fy})
## @deftypefnx {Function File} {[@dots{}] =} curl (@var{fx}, @var{fy}, @var{fz})
## @deftypefnx {Function File} {[@dots{}] =} curl (@var{fx}, @var{fy})
## @deftypefnx {Function File} {@var{v} =} curl (@dots{})
## Calculate curl of vector field given by the arrays @var{fx}, @var{fy}, and
## @var{fz} or @var{fx}, @var{fy} respectively.
## @tex
## $$ curl F(x,y,z) = \left( {\partial{d} \over \partial{y}} F_z - {\partial{d} \over \partial{z}} F_y, {\partial{d} \over \partial{z}} F_x - {\partial{d} \over \partial{x}} F_z, {\partial{d} \over \partial{x}} F_y - {\partial{d} \over \partial{y}} F_x \right)$$
## @end tex
## @ifnottex
##
## @example
## @group
##                   / d         d       d         d       d         d     \
## curl F(x,y,z)  =  | -- Fz  -  -- Fy,  -- Fx  -  -- Fz,  -- Fy  -  -- Fx |
##                   \ dy        dz      dz        dx      dx        dy    /
## @end group
## @end example
##
## @end ifnottex
## The coordinates of the vector field can be given by the arguments @var{x},
## @var{y}, @var{z} or @var{x}, @var{y} respectively.  @var{v} calculates the
## scalar component of the angular velocity vector in direction of the z-axis
## for two-dimensional input.  For three-dimensional input the scalar
## rotation is calculated at each grid point in direction of the vector field
## at that point.
## @seealso{divergence, gradient, del2, cross}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function varargout = curl (varargin)

  fidx = 1;
  if (nargin == 2)
    sz = size (varargin{fidx});
    dx = (1:sz(2))(:);
    dy = (1:sz(1))(:);
  elseif (nargin == 3)
    sz = size (varargin{fidx});
    dx = (1:sz(2))(:);
    dy = (1:sz(1))(:);
    dz = (1:sz(3))(:);
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
    if (! size_equal (varargin{fidx}, varargin{fidx + 1}))
      error ("curl: size of X and Y must match");
    elseif (ndims (varargin{fidx}) != 2)
      error ("curl: expected two-dimensional matrices X and Y");
    elseif ((length (dx) != columns (varargin{fidx}))
         || (length (dy) != rows (varargin{fidx})))
      error ("curl: size of dx and dy must match the respective dimension of X and Y");
    endif

    dFx_dy = gradient (varargin{fidx}.', dy, dx).';
    dFy_dx = gradient (varargin{fidx + 1}, dx, dy);
    rot_z = dFy_dx - dFx_dy;
    av = rot_z / 2;
    if (nargout == 0 || nargout == 1)
      varargout{1} = av;
    else
      varargout{1} = rot_z;
      varargout{2} = av;
    endif

  elseif (nargin == 6 || nargin == 3)
    if (! size_equal (varargin{fidx}, varargin{fidx + 1}, varargin{fidx + 2}))
      error ("curl: size of X, Y, and Z must match");
    elseif (ndims (varargin{fidx}) != 3)
      error ("curl: expected two-dimensional matrices X, Y, and Z");
    elseif ((length (dx) != size (varargin{fidx}, 2))
         || (length (dy) != size (varargin{fidx}, 1))
         || (length (dz) != size (varargin{fidx}, 3)))
      error ("curl: size of dx, dy, and dz must match the respective dimesion of X, Y, and Z");
    endif

    [~, dFx_dy, dFx_dz] = gradient (varargin{fidx}, dx, dy, dz);
    [dFy_dx, ~, dFy_dz] = gradient (varargin{fidx + 1}, dx, dy, dz);
    [dFz_dx, dFz_dy] = gradient (varargin{fidx + 2}, dx, dy, dz);
    rot_x = dFz_dy - dFy_dz;
    rot_y = dFx_dz - dFz_dx;
    rot_z = dFy_dx - dFx_dy;
    l = sqrt(varargin{fidx}.^2 + varargin{fidx + 1}.^2 + varargin{fidx + 2}.^2);
    av = (rot_x .* varargin{fidx} +
          rot_y .* varargin{fidx + 1} +
          rot_z .* varargin{fidx + 2}) ./ (2 * l);

    if (nargout == 0 || nargout == 1)
      varargout{1} = av;
    else
      varargout{1} = rot_x;
      varargout{2} = rot_y;
      varargout{3} = rot_z;
      varargout{4} = av;
    endif
  endif

endfunction


%!test
%! [X,Y] = meshgrid (-20:20,-22:22);
%! av = curl (2*(X-Y), Y);
%! assert (all (av(:) == 1));
%! [cz,av] = curl (2*(X-Y), Y);
%! assert (all (cz(:) == 2));
%! assert (all (av(:) == 1));
%! [cz,av] = curl (X/2, Y/2, 2*(X-Y), Y);
%! assert (all (cz(:) == 4));
%! assert (all (av(:) == 2));
%! assert (size_equal (X,Y,cz,av));

