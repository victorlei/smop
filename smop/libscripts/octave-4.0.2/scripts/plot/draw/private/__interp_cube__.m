## Copyright (C) 2009-2015 Martin Helm
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
##
## Author: Martin Helm <martin@mhelm.de>

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{vxyz}, @var{idx}, @var{frac}] =} __interp_cube__ (@var{x}, @var{y}, @var{z}, @var{val}, @var{v})
## Undocumented internal function.
## @end deftypefn

function [Vxyz, idx, frac] = __interp_cube__ (x, y, z, val, v, req = "values" )
  if (isnumeric (x) && ndims (x) == 3 && isnumeric (y) && ndims (y) == 3
       && isnumeric (z) && ndims (z) == 3 && size_equal (x, y, z, val))
    x = squeeze (x(1,:,1))(:);
    y = squeeze (y(:,1,1))(:);
    z = squeeze (z(1,1,:))(:);
  elseif (isvector (x) && isvector (y) && isvector (z))
    x = x(:);
    y = y(:);
    z = z(:);
  else
    error ("__interp_cube__: X, Y, Z have wrong dimensions");
  endif
  if (size (val) != [length(x), length(y), length(z)])
    error ("__interp_cube__: VAL has wrong dimensions");
  endif
  if (columns (v) != 3)
    error ( "V has to be Nx3 matrix");
  endif
  ##if (! ischar (req))
  ## error ('__interp_cube__: Invalid request parameter use "values", "normals" or "normals8"');
  ##endif
  if (isempty (v))
    Vxyz = idx = frac = [];
    return;
  endif

  switch (req)
    case "normals"
      [idx, frac] = cube_idx (x, y, z, v);

      dx = x(2:end) - x(1:end-1);
      dy = y(2:end) - y(1:end-1);
      dz = z(2:end) - z(1:end-1);
      dx = 0.5 .* [dx;dx(end)](idx(:,2));
      dy = 0.5 .* [dy;dy(end)](idx(:,1));
      dz = 0.5 .* [dz;dz(end)](idx(:,3));

      p000 = [v(:, 1) - dx, v(:, 2) - dy, v(:, 3) - dz];
      p100 = [v(:, 1) + dx, v(:, 2) - dy, v(:, 3) - dz];
      p010 = [v(:, 1) - dx, v(:, 2) + dy, v(:, 3) - dz];
      p001 = [v(:, 1) - dx, v(:, 2) - dy, v(:, 3) + dz];
      p011 = [v(:, 1) - dx, v(:, 2) + dy, v(:, 3) + dz];
      p101 = [v(:, 1) + dx, v(:, 2) - dy, v(:, 3) + dz];
      p110 = [v(:, 1) + dx, v(:, 2) + dy, v(:, 3) - dz];
      p111 = [v(:, 1) + dx, v(:, 2) + dy, v(:, 3) + dz];

      v000 = interp_cube_trilin (x, y, z, val, p000);
      v100 = interp_cube_trilin (x, y, z, val, p100);
      v010 = interp_cube_trilin (x, y, z, val, p010);
      v001 = interp_cube_trilin (x, y, z, val, p001);
      v011 = interp_cube_trilin (x, y, z, val, p011);
      v101 = interp_cube_trilin (x, y, z, val, p101);
      v110 = interp_cube_trilin (x, y, z, val, p110);
      v111 = interp_cube_trilin (x, y, z, val, p111);

      Dx = -v000 .+ v100 .- v010 .- v001 .- v011 .+ v101 .+ v110 .+ v111;
      Dy = -v000 .- v100 .+ v010 .- v001 .+ v011 .- v101 .+ v110 .+ v111;
      Dz = -v000 .- v100 .- v010 .+ v001 .+ v011 .+ v101 .- v110 .+ v111;
      Vxyz = 0.5 .* [Dx./dx, Dy./dy, Dz./dz];
    case "normals8"
      [idx, frac] = cube_idx (x, y, z, v);

      dx = x(2:end) - x(1:end-1);
      dy = y(2:end) - y(1:end-1);
      dz = z(2:end) - z(1:end-1);
      dx = [dx;dx(end)](idx(:,2));
      dy = [dy;dy(end)](idx(:,1));
      dz = [dz;dz(end)](idx(:,3));
      [Dx, Dy, Dz, idx, frac] = interp_cube_trilin_grad (x, y, z, val, v);
      Vxyz = [Dx./dx, Dy./dy, Dz./dz];
    case "values"
      [Vxyz, idx, frac] = interp_cube_trilin (x, y, z, val, v);
   otherwise
     error ('__interp_cube__: Invalid request type "%s", use "values", "normals" or "normals8"', req);
  endswitch
endfunction

function [Vxyz, idx, frac] = interp_cube_trilin (x, y, z, val, v)
  [idx, frac] = cube_idx (x(:), y(:), z(:), v);
  sval = size (val);
  i000 = sub2ind (sval, idx(:, 1), idx(:, 2), idx(:, 3));
  i100 = sub2ind (sval, idx(:, 1)+1, idx(:, 2), idx(:, 3));
  i010 = sub2ind (sval, idx(:, 1), idx(:, 2)+1, idx(:, 3));
  i001 = sub2ind (sval, idx(:, 1), idx(:, 2), idx(:, 3)+1);
  i101 = sub2ind (sval, idx(:, 1)+1, idx(:, 2), idx(:, 3)+1);
  i011 = sub2ind (sval, idx(:, 1), idx(:, 2)+1, idx(:, 3)+1);
  i110 = sub2ind (sval, idx(:, 1)+1, idx(:, 2)+1, idx(:, 3));
  i111 = sub2ind (sval, idx(:, 1)+1, idx(:, 2)+1, idx(:, 3)+1 );
  Bx = frac(:, 1);
  By = frac(:, 2);
  Bz = frac(:, 3);
  Vxyz = ...
    val( i000 ) .* (1 .- Bx) .* (1 .- By) .* (1 .- Bz) .+ ...
    val( i100 ) .* Bx .* (1 .- By) .* (1 .- Bz) .+ ...
    val( i010 ) .* (1 .- Bx) .* By .* (1 .- Bz) .+ ...
    val( i001 ) .* (1 .- Bx) .* (1 .- By) .* Bz .+ ...
    val( i011 ) .* (1 .- Bx) .* By .* Bz .+ ...
    val( i101 ) .* Bx .* (1 .- By) .* Bz .+ ...
    val( i110 ) .* Bx .* By .* (1 .- Bz) .+ ...
    val( i111 ) .* Bx .* By .* Bz;
endfunction

function [Dx, Dy, Dz, idx, frac] = interp_cube_trilin_grad (x, y, z, val, v)
  [idx, frac] = cube_idx (x(:), y(:), z(:), v);
  sval = size (val);
  i000 = sub2ind (sval, idx(:, 1), idx(:, 2), idx(:, 3));
  i100 = sub2ind (sval, idx(:, 1)+1, idx(:, 2), idx(:, 3));
  i010 = sub2ind (sval, idx(:, 1), idx(:, 2)+1, idx(:, 3));
  i001 = sub2ind (sval, idx(:, 1), idx(:, 2), idx(:, 3)+1);
  i101 = sub2ind (sval, idx(:, 1)+1, idx(:, 2), idx(:, 3)+1);
  i011 = sub2ind (sval, idx(:, 1), idx(:, 2)+1, idx(:, 3)+1);
  i110 = sub2ind (sval, idx(:, 1)+1, idx(:, 2)+1, idx(:, 3));
  i111 = sub2ind (sval, idx(:, 1)+1, idx(:, 2)+1, idx(:, 3)+1 );
  Bx = frac(:, 1);
  By = frac(:, 2);
  Bz = frac(:, 3);
  Dx = ...
    val( i000 ) .* -1 .* (1 .- By) .* (1 .- Bz) .+ ...
    val( i100 ) .* (1 .- By) .* (1 .- Bz) .+ ...
    val( i010 ) .* -1 .* By .* (1 .- Bz) .+ ...
    val( i001 ) .* -1 .* (1 .- By) .* Bz .+ ...
    val( i011 ) .* -1 .* By .* Bz .+ ...
    val( i101 ) .* (1 .- By) .* Bz .+ ...
    val( i110 ) .* By .* (1 .- Bz) .+ ...
    val( i111 ) .* By .* Bz;
  Dy = ...
    val( i000 ) .* (1 .- Bx) .* -1 .* (1 .- Bz) .+ ...
    val( i100 ) .* Bx .* -1 .* (1 .- Bz) .+ ...
    val( i010 ) .* (1 .- Bx) .* (1 .- Bz) .+ ...
    val( i001 ) .* (1 .- Bx) .* -1 .* Bz .+ ...
    val( i011 ) .* (1 .- Bx) .* Bz .+ ...
    val( i101 ) .* Bx .* -1 .* Bz .+ ...
    val( i110 ) .* Bx .* (1 .- Bz) .+ ...
    val( i111 ) .* Bx .* Bz;
  Dz = ...
    val( i000 ) .* (1 .- Bx) .* (1 .- By) .* -1 .+ ...
    val( i100 ) .* Bx .* (1 .- By) .* -1 .+ ...
    val( i010 ) .* (1 .- Bx) .* By .* -1 .+ ...
    val( i001 ) .* (1 .- Bx) .* (1 .- By) .+ ...
    val( i011 ) .* (1 .- Bx) .* By + ...
    val( i101 ) .* Bx .* (1 .- By) .+ ...
    val( i110 ) .* Bx .* By .* -1 .+ ...
    val( i111 ) .* Bx .* By;
endfunction

function [idx, frac] = cube_idx (x, y, z, v)
  idx = zeros (size (v));
  frac = zeros (size (v));
  idx(:, 2) = lookup (x(2:end-1), v(:, 1)) + 1;
  frac(:, 2) = (v(:, 1) - x(idx(:, 2)) )...
      ./ (x(idx(:, 2)+1) - x(idx(:, 2)));
  idx(:, 1) = lookup (y(2:end-1), v(:, 2)) + 1;
  frac(:, 1) = (v(:, 2) - y(idx(:, 1))) ...
      ./ (y(idx(:, 1)+1) - y(idx(:, 1)));
  idx(:, 3) = lookup (z(2:end-1), v(:, 3)) + 1;
  frac(:, 3) = (v(:, 3) - z(idx(:, 3))) ...
      ./ (z(idx(:, 3)+1) - z(idx(:, 3)));
endfunction

