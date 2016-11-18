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
## @deftypefn  {Function File} {} surface (@var{x}, @var{y}, @var{z}, @var{c})
## @deftypefnx {Function File} {} surface (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} surface (@var{z}, @var{c})
## @deftypefnx {Function File} {} surface (@var{z})
## @deftypefnx {Function File} {} surface (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} surface (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} surface (@dots{})
## Create a surface graphic object given matrices @var{x} and @var{y} from
## @code{meshgrid} and a matrix of values @var{z} corresponding to the
## @var{x} and @var{y} coordinates of the surface.
##
## If @var{x} and @var{y} are vectors, then a typical vertex is
## (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, columns of @var{z} correspond
## to different @var{x} values and rows of @var{z} correspond to different
## @var{y} values.  If only a single input @var{z} is given then @var{x} is
## taken to be @code{1:rows (@var{z})} and @var{y} is
## @code{1:columns (@var{z})}.
##
## Any property/value input pairs are assigned to the surface object.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
## @seealso{surf, mesh, patch, line}
## @end deftypefn

## Author: jwe

function h = surface (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("surface", varargin{:});

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  [htmp, bad_usage] = __surface__ (hax, varargin{:});

  if (bad_usage)
    print_usage ();
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction

function [h, bad_usage] = __surface__ (ax, varargin)

  h = 0;
  bad_usage = false;
  firststring = find (cellfun ("isclass", varargin, "char"), 1);
  if (isempty (firststring))
    firststring = nargin;
  endif

  switch (firststring)
    case 5
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      c = varargin{4};

      if (iscomplex (x) || iscomplex (y) || iscomplex (z) || iscomplex (c))
        error ("mesh: X, Y, Z, C arguments must be real");
      endif

      [z_nr, z_nc] = size (z);
      [c_nr, c_nc, c_np] = size (c);
      if (! (z_nr == c_nr && z_nc == c_nc && (c_np == 1 || c_np == 3)))
        error ("surface: Z and C must have the same size");
      endif

      if (isvector (x) && isvector (y) && ismatrix (z))
        if (rows (z) == length (y) && columns (z) == length (x))
          x = x(:)';
          y = y(:);
        else
          error ("surface: rows (Z) must be the same as length (Y) and columns (Z) must be the same as length (X)");
        endif
      elseif (ismatrix (x) && ismatrix (y) && ismatrix (z))
        if (! size_equal (x, y, z))
          error ("surface: X, Y, and Z must have the same dimensions");
        endif
      else
        error ("surface: X and Y must be vectors and Z must be a matrix");
      endif

    case 4
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      c = z;

      if (iscomplex (x) || iscomplex (y) || iscomplex (z))
        error ("mesh: X, Y, Z arguments must be real");
      endif

      if (isvector (x) && isvector (y) && ismatrix (z))
        if (rows (z) == length (y) && columns (z) == length (x))
          x = x(:)';
          y = y(:);
        else
          error ("surface: rows (Z) must be the same as length (Y) and columns (Z) must be the same as length (X)");
        endif
      elseif (ismatrix (x) && ismatrix (y) && ismatrix (z))
        if (! size_equal (x, y, z))
          error ("surface: X, Y, and Z must have the same dimensions");
        endif
      else
        error ("surface: X and Y must be vectors and Z must be a matrix");
      endif

    case 3
      z = varargin{1};
      c = varargin{2};

      if (iscomplex (z) || iscomplex (c))
        error ("mesh: X, C arguments must be real");
      endif

      if (ismatrix (z) && ! isvector (z) && ! isscalar (z))
        [nr, nc] = size (z);
        x = 1:nc;
        y = (1:nr)';
      else
        error ("surface: Z argument must be a matrix");
      endif

    case 2
      z = varargin{1};
      c = z;

      if (iscomplex (z))
        error ("mesh: Z argument must be real");
      endif

      if (ismatrix (z) && ! isvector (z) && ! isscalar (z))
        [nr, nc] = size (z);
        x = 1:nc;
        y = (1:nr)';
      else
        error ("surface: Z argument must be a matrix");
      endif

    case 1
      x = 1:3;
      y = x';
      c = z = eye (3);

    otherwise
      bad_usage = true;
      return;

  endswitch

  if (firststring < nargin)
    other_args = varargin(firststring:end);
  else
    other_args = {};  # make a default surface object.
  endif
  h = __go_surface__ (ax, "xdata", x, "ydata", y, "zdata", z, "cdata", c,
                      other_args{:});

  if (! ishold ())
    set (ax, "view", [0, 90], "box", "off");
  endif

endfunction


## Functional tests for surface() are in surf.m, surfc.m, surfl.m, and pcolor.m
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = surface;
%!   assert (findobj (hf, "type", "surface"), h);
%!   assert (get (h, "xdata"), 1:3, eps);
%!   assert (get (h, "ydata"), (1:3)', eps);
%!   assert (get (h, "zdata"), eye (3));
%!   assert (get (h, "cdata"), eye (3));
%!   assert (get (h, "type"), "surface");
%!   assert (get (h, "linestyle"), get (0, "defaultsurfacelinestyle"));
%!   assert (get (h, "linewidth"), get (0, "defaultsurfacelinewidth"), eps);
%!   assert (get (h, "marker"), get (0, "defaultsurfacemarker"));
%!   assert (get (h, "markersize"), get (0, "defaultsurfacemarkersize"));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

