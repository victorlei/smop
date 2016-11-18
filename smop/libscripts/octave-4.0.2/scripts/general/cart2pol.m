## Copyright (C) 2000-2016 Kai Habel
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
## @deftypefn  {Function File} {[@var{theta}, @var{r}] =} cart2pol (@var{x}, @var{y})
## @deftypefnx {Function File} {[@var{theta}, @var{r}, @var{z}] =} cart2pol (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {[@var{theta}, @var{r}] =} cart2pol (@var{C})
## @deftypefnx {Function File} {[@var{theta}, @var{r}, @var{z}] =} cart2pol (@var{C})
## @deftypefnx {Function File} {@var{P} =} cart2pol (@dots{})
##
## Transform Cartesian coordinates to polar or cylindrical coordinates.
##
## The inputs @var{x}, @var{y} (, and @var{z}) must be the same shape, or
## scalar.  If called with a single matrix argument then each row of @var{C}
## represents the Cartesian coordinate (@var{x}, @var{y} (, @var{z})).
##
## @var{theta} describes the angle relative to the positive x-axis.
##
## @var{r} is the distance to the z-axis @w{(0, 0, z)}.
##
## If only a single return argument is requested then return a matrix @var{P}
## where each row represents one polar/(cylindrical) coordinate
## (@var{theta}, @var{phi} (, @var{z})).
## @seealso{pol2cart, cart2sph, sph2cart}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [theta, r, z] = cart2pol (x, y, z = [])

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 1)
    if (! (isnumeric (x) && ismatrix (x)
           && (columns (x) == 2 || columns (x) == 3)))
      error ("cart2pol: matrix input must have 2 or 3 columns [X, Y (, Z)]");
    endif
    if (columns (x) == 3)
      z = x(:,3);
    endif
    y = x(:,2);
    x = x(:,1);
  elseif (nargin == 2)
    if (! ((isnumeric (x) && isnumeric (y))
            && (size_equal (x, y) || isscalar (x) || isscalar (y))))
      error ("cart2pol: X, Y must be numeric arrays of the same size, or scalar");
    endif
  elseif (nargin == 3)
    if (! ((isnumeric (x) && isnumeric (y) && isnumeric (z))
            && (size_equal (x, y) || isscalar (x) || isscalar (y))
            && (size_equal (x, z) || isscalar (x) || isscalar (z))
            && (size_equal (y, z) || isscalar (y) || isscalar (z))))
      error ("cart2pol: X, Y, Z must be numeric arrays of the same size, or scalar");
    endif
  endif

  theta = atan2 (y, x);
  r = sqrt (x .^ 2 + y .^ 2);

  if (nargout <= 1)
    theta = [theta(:), r(:), z(:)];
  endif

endfunction


%!test
%! x = [0, 1, 2];
%! y = 0;
%! [t, r] = cart2pol (x, y);
%! assert (t, [0, 0, 0]);
%! assert (r, x);

%!test
%! x = [0, 1, 2];
%! y = [0, 1, 2];
%! P = cart2pol (x, y);
%! assert (P(:,1), [0; pi/4; pi/4], sqrt (eps));
%! assert (P(:,2), sqrt (2)*[0; 1; 2], sqrt (eps));

%!test
%! x = [0, 1, 2];
%! y = [0, 1, 2];
%! z = [0, 1, 2];
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, pi/4, pi/4], sqrt (eps));
%! assert (r, sqrt (2)*[0, 1, 2], sqrt (eps));
%! assert (z, z2);

%!test
%! x = [0, 1, 2];
%! y = 0;
%! z = 0;
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, 0, 0], eps);
%! assert (r, x, eps);
%! assert (z, z2);

%!test
%! x = 0;
%! y = [0, 1, 2];
%! z = 0;
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, 1, 1]*pi/2, eps);
%! assert (r, y, eps);
%! assert (z, z2);

%!test
%! x = 0;
%! y = 0;
%! z = [0, 1, 2];
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, 0);
%! assert (r, 0);
%! assert (z, z2);

%!test
%! C = [0, 0; 1, 1; 2, 2];
%! P = [0, 0; pi/4, sqrt(2); pi/4, 2*sqrt(2)];
%! assert (cart2pol (C), P, sqrt (eps));

%!test
%! C = [0, 0, 0; 1, 1, 1; 2, 2, 2];
%! P = [0, 0, 0; pi/4, sqrt(2), 1; pi/4, 2*sqrt(2), 2];
%! assert (cart2pol (C), P, sqrt (eps));

%!test
%! x = zeros (1, 1, 1, 2);
%! x(1, 1, 1, 2) = sqrt (2);
%! y = x;
%! [t, r] = cart2pol (x, y);
%! T = zeros (1, 1, 1, 2);
%! T(1, 1, 1, 2) = pi/4;
%! R = zeros (1, 1, 1, 2);
%! R(1, 1, 1, 2) = 2;
%! assert (t, T, eps);
%! assert (r, R, eps);

%!test
%! [x, y, Z] = meshgrid ([0, 1], [0, 1], [0, 1]);
%! [t, r, z] = cart2pol (x, y, Z);
%! T(:, :, 1) = [0, 0; pi/2, pi/4];
%! T(:, :, 2) = T(:, :, 1);
%! R = sqrt (x.^2 + y.^2);
%! assert (t, T, eps);
%! assert (r, R, eps);
%! assert (z, Z);

## Test input validation
%!error cart2pol ()
%!error cart2pol (1,2,3,4)
%!error <matrix input must have 2 or 3 columns> cart2pol ({1,2,3})
%!error <matrix input must have 2 or 3 columns> cart2pol (ones (3,3,2))
%!error <matrix input must have 2 or 3 columns> cart2pol ([1])
%!error <matrix input must have 2 or 3 columns> cart2pol ([1,2,3,4])
%!error <numeric arrays of the same size> cart2pol ({1,2,3}, [1,2,3])
%!error <numeric arrays of the same size> cart2pol ([1,2,3], {1,2,3})
%!error <numeric arrays of the same size> cart2pol (ones (3,3,3), ones (3,2,3))
%!error <numeric arrays of the same size> cart2pol ({1,2,3}, [1,2,3], [1,2,3])
%!error <numeric arrays of the same size> cart2pol ([1,2,3], {1,2,3}, [1,2,3])
%!error <numeric arrays of the same size> cart2pol ([1,2,3], [1,2,3], {1,2,3})
%!error <numeric arrays of the same size> cart2pol (ones (3,3,3), 1, ones (3,2,3))
%!error <numeric arrays of the same size> cart2pol (ones (3,3,3), ones (3,2,3), 1)

