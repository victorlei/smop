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
## @deftypefn  {Function File} {[@var{x}, @var{y}, @var{z}] =} sph2cart (@var{theta}, @var{phi}, @var{r})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} sph2cart (@var{S})
## @deftypefnx {Function File} {@var{C} =} sph2cart (@dots{})
## Transform spherical coordinates to Cartesian coordinates.
##
## The inputs @var{theta}, @var{phi}, and @var{r} must be the same shape, or
## scalar.  If called with a single matrix argument then each row of @var{S}
## represents the spherical coordinate (@var{theta}, @var{phi}, @var{r}).
##
## @var{theta} describes the angle relative to the positive x-axis.
##
## @var{phi} is the angle relative to the xy-plane.
##
## @var{r} is the distance to the origin @w{(0, 0, 0)}.
##
## If only a single return argument is requested then return a matrix @var{C}
## where each row represents one Cartesian coordinate
## (@var{x}, @var{y}, @var{z}).
## @seealso{cart2sph, pol2cart, cart2pol}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [x, y, z] = sph2cart (theta, phi, r)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    if (! (isnumeric (theta) && ismatrix (theta) && columns (theta) == 3))
      error ("sph2cart: matrix input must have 3 columns [THETA, PHI, R]");
    endif
    r = theta(:,3);
    phi = theta(:,2);
    theta = theta(:,1);
  else
    if (! ((isnumeric (theta) && isnumeric (phi) && isnumeric (r))
            && (size_equal (theta, phi) || isscalar (theta) || isscalar (phi))
            && (size_equal (theta, r) || isscalar (theta) || isscalar (r))
            && (size_equal (phi, r) || isscalar (phi) || isscalar (r))))
      error ("sph2cart: THETA, PHI, R must be numeric arrays of the same size, or scalar");
    endif
  endif

  x = r .* cos (phi) .* cos (theta);
  y = r .* cos (phi) .* sin (theta);
  z = r .* sin (phi);

  if (nargout <= 1)
    x = [x(:), y(:), z(:)];
  endif

endfunction


%!test
%! t = [0, 0, 0];
%! p = [0, 0, 0];
%! r = [0, 1, 2];
%! [x, y, z] = sph2cart (t, p, r);
%! assert (x, r);
%! assert (y, [0, 0, 0]);
%! assert (z, [0, 0, 0]);

%!test
%! t = 0;
%! p = [0, 0, 0];
%! r = [0, 1, 2];
%! C = sph2cart (t, p, r);
%! assert (C(:,1), r(:));
%! assert (C(:,2), [0; 0; 0]);
%! assert (C(:,3), [0; 0; 0]);

%!test
%! t = [0, 0, 0];
%! p = 0;
%! r = [0, 1, 2];
%! [x, y, z] = sph2cart (t, p, r);
%! assert (x, r);
%! assert (y, [0, 0, 0]);
%! assert (z, [0, 0, 0]);

%!test
%! t = [0, 0.5, 1]*pi;
%! p = [0, 0, 0];
%! r = 1;
%! [x, y, z] = sph2cart (t, p, r);
%! assert (x, [1, 0, -1], eps);
%! assert (y, [0, 1, 0], eps);
%! assert (z, [0, 0, 0], eps);

%!test
%! S = [ 0, 0, 1; 0.5*pi, 0, 1; pi, 0, 1];
%! C = [ 1, 0, 0; 0, 1, 0; -1, 0, 0];
%! assert (sph2cart (S), C, eps);

%!test
%! [t, p, r] = meshgrid ([0, pi/2], [0, pi/2], [0, 1]);
%! [x, y, z] = sph2cart (t, p, r);
%! X = zeros(2, 2, 2);
%! X(1, 1, 2) = 1;
%! Y = zeros(2, 2, 2);
%! Y(1, 2, 2) = 1;
%! Z = zeros(2, 2, 2);
%! Z(2, :, 2) = [1 1];
%! assert (x, X, eps);
%! assert (y, Y, eps);
%! assert (z, Z);

## Test input validation
%!error sph2cart ()
%!error sph2cart (1,2)
%!error sph2cart (1,2,3,4)
%!error <matrix input must have 3 columns> sph2cart ({1,2,3})
%!error <matrix input must have 3 columns> sph2cart (ones (3,3,2))
%!error <matrix input must have 3 columns> sph2cart ([1,2,3,4])
%!error <numeric arrays of the same size> sph2cart ({1,2,3}, [1,2,3], [1,2,3])
%!error <numeric arrays of the same size> sph2cart ([1,2,3], {1,2,3}, [1,2,3])
%!error <numeric arrays of the same size> sph2cart ([1,2,3], [1,2,3], {1,2,3})
%!error <numeric arrays of the same size> sph2cart (ones (3,3,3), 1, ones (3,2,3))
%!error <numeric arrays of the same size> sph2cart (ones (3,3,3), ones (3,2,3), 1)

