## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Function File} {} surfnorm (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} surfnorm (@var{z})
## @deftypefnx {Function File} {} surfnorm (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} surfnorm (@var{hax}, @dots{})
## @deftypefnx {Function File} {[@var{nx}, @var{ny}, @var{nz}] =} surfnorm (@dots{})
## Find the vectors normal to a meshgridded surface.
##
## If @var{x} and @var{y} are vectors, then a typical vertex is
## (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, columns of @var{z} correspond
## to different @var{x} values and rows of @var{z} correspond to different
## @var{y} values.  If only a single input @var{z} is given then @var{x} is
## taken to be @code{1:rows (@var{z})} and @var{y} is
## @code{1:columns (@var{z})}.
##
## If no return arguments are requested, a surface plot with the normal
## vectors to the surface is plotted.
##
## Any property/value input pairs are assigned to the surface object.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## If output arguments are requested then the components of the normal
## vectors are returned in @var{nx}, @var{ny}, and @var{nz} and no plot is
## made.
##
## An example of the use of @code{surfnorm} is
##
## @example
## surfnorm (peaks (25));
## @end example
##
## Algorithm: The normal vectors are calculated by taking the cross product
## of the diagonals of each of the quadrilaterals in the meshgrid to find the
## normal vectors of the centers of these quadrilaterals.  The four nearest
## normal vectors to the meshgrid points are then averaged to obtain the
## normal to the surface at the meshgridded points.
##
## @seealso{isonormals, quiver3, surf, meshgrid}
## @end deftypefn

function [Nx, Ny, Nz] = surfnorm (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("surfnorm", varargin{:});

  if (nargin == 0 || nargin == 2)
    print_usage ();
  endif

  if (nargin == 1)
    z = varargin{1};
    [x, y] = meshgrid (1:columns (z), 1:rows (z));
    ioff = 2;
  else
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
    ioff = 4;
  endif

  if (iscomplex (z) || iscomplex (x) || iscomplex (y))
    error ("surfnorm: X, Y, and Z must be 2-D real matrices");
  endif
  if (! size_equal (x, y, z))
    error ("surfnorm: X, Y, and Z must have the same dimensions");
  endif

  ## Do a linear extrapolation for mesh points on the boundary so that the mesh
  ## is increased by 1 on each side.  This allows each original meshgrid point
  ## to be surrounded by four quadrilaterals and the same calculation can be
  ## used for interior and boundary points.  The extrapolation works badly for
  ## closed surfaces like spheres.
  xx = [2 * x(:,1) - x(:,2), x, 2 * x(:,end) - x(:,end-1)];
  xx = [2 * xx(1,:) - xx(2,:); xx; 2 * xx(end,:) - xx(end-1,:)];
  yy = [2 * y(:,1) - y(:,2), y, 2 * y(:,end) - y(:,end-1)];
  yy = [2 * yy(1,:) - yy(2,:); yy; 2 * yy(end,:) - yy(end-1,:)];
  zz = [2 * z(:,1) - z(:,2), z, 2 * z(:,end) - z(:,end-1)];
  zz = [2 * zz(1,:) - zz(2,:); zz; 2 * zz(end,:) - zz(end-1,:)];

  u.x = xx(1:end-1,1:end-1) - xx(2:end,2:end);
  u.y = yy(1:end-1,1:end-1) - yy(2:end,2:end);
  u.z = zz(1:end-1,1:end-1) - zz(2:end,2:end);
  v.x = xx(1:end-1,2:end) - xx(2:end,1:end-1);
  v.y = yy(1:end-1,2:end) - yy(2:end,1:end-1);
  v.z = zz(1:end-1,2:end) - zz(2:end,1:end-1);

  c = cross ([u.x(:), u.y(:), u.z(:)], [v.x(:), v.y(:), v.z(:)]);
  w.x = reshape (c(:,1), size (u.x));
  w.y = reshape (c(:,2), size (u.y));
  w.z = reshape (c(:,3), size (u.z));

  ## Create normal vectors as mesh vectices from normals at mesh centers
  nx = (w.x(1:end-1,1:end-1) + w.x(1:end-1,2:end) +
        w.x(2:end,1:end-1) + w.x(2:end,2:end)) / 4;
  ny = (w.y(1:end-1,1:end-1) + w.y(1:end-1,2:end) +
        w.y(2:end,1:end-1) + w.y(2:end,2:end)) / 4;
  nz = (w.z(1:end-1,1:end-1) + w.z(1:end-1,2:end) +
        w.z(2:end,1:end-1) + w.z(2:end,2:end)) / 4;

  ## FIXME: According to Matlab documentation the vertex normals
  ##        returned are not normalized.
  ## Normalize the normal vectors
  len = sqrt (nx.^2 + ny.^2 + nz.^2);
  nx ./= len;
  ny ./= len;
  nz ./= len;

  if (nargout == 0)
    oldfig = [];
    if (! isempty (hax))
      oldfig = get (0, "currentfigure");
    endif
    unwind_protect
      hax = newplot (hax);

      surf (x, y, z, varargin{ioff:end});
      old_hold_state = get (hax, "nextplot");
      unwind_protect
        set (hax, "nextplot", "add");

        ## FIXME: Scale unit normals by data aspect ratio in order for
        ##        normals to appear correct.
        ##daratio = daspect (hax);
        ##daspect ("manual");
        ##len = norm (daratio);
        ## This assumes an even meshgrid which isn't a great assumption
        ##dx = x(1,2) - x(1,1);
        ##dy = y(2,1) - y(1,1);
        ##nx *= daratio(1);
        ##ny *= daratio(2);
        ##nz *= daratio(3);
        ##len = sqrt (nx.^2 + ny.^2 + nz.^2);
        ##nx ./= len;
        ##ny ./= len;
        ##nz ./= len;
        plot3 ([x(:).'; x(:).' + nx(:).' ; NaN(size(x(:).'))](:),
               [y(:).'; y(:).' + ny(:).' ; NaN(size(y(:).'))](:),
               [z(:).'; z(:).' + nz(:).' ; NaN(size(z(:).'))](:),
               "r");
      unwind_protect_cleanup
        set (hax, "nextplot", old_hold_state);
      end_unwind_protect

    unwind_protect_cleanup
      if (! isempty (oldfig))
        set (0, "currentfigure", oldfig);
      endif
    end_unwind_protect
  else
    Nx = nx;
    Ny = ny;
    Nz = nz;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! surfnorm (peaks (32));
%! shading interp;
%! title ({'surfnorm() shows surface and normals at each vertex', ...
%!         'peaks() function with 32 faces'});

%!demo
%! clf;
%! colormap ('default');
%! [x, y, z] = sombrero (10);
%! surfnorm (x, y, z);

## Test input validation
%!error surfnorm ()
%!error surfnorm (1,2)
%!error <X, Y, and Z must be 2-D real matrices> surfnorm (i)
%!error <X, Y, and Z must be 2-D real matrices> surfnorm (i, 1, 1)
%!error <X, Y, and Z must be 2-D real matrices> surfnorm (1, i, 1)
%!error <X, Y, and Z must be 2-D real matrices> surfnorm (1, 1, i)
%!error <X, Y, and Z must have the same dimensions> surfnorm ([1 2], 1, 1)
%!error <X, Y, and Z must have the same dimensions> surfnorm (1, [1 2], 1)
%!error <X, Y, and Z must have the same dimensions> surfnorm (1, 1, [1 2])

