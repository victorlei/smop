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

function interpimages (nm, typ)
  graphics_toolkit ("gnuplot");
  set_print_size ();
  hide_output ();
  if (strcmp (typ, "png"))
    set (0, "defaulttextfontname", "*");
  endif
  if (strcmp (typ, "eps"))
    d_typ = "-depsc2";
  else
    d_typ = ["-d", typ];
  endif

  if (strcmp (typ, "txt"))
    image_as_txt (nm);
  elseif (strcmp (nm, "interpft"))
    t = 0 : 0.3 : pi; dt = t(2)-t(1);
    n = length (t); k = 100;
    ti = t(1) + [0 : k-1]*dt*n/k;
    y = sin (4*t + 0.3) .* cos (3*t - 0.1);
    yp = sin (4*ti + 0.3) .* cos (3*ti - 0.1);
    plot (ti, yp, "g", ti, interp1 (t, y, ti, "spline"), "b", ...
          ti, interpft (y, k), "c", t, y, "r+");
    legend ("sin(4t+0.3)cos(3t-0.1)", "spline", "interpft", "data");
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "interpn"))
    x = y = z = -1:1;
    f = @(x,y,z) x.^2 - y - z.^2;
    [xx, yy, zz] = meshgrid (x, y, z);
    v = f (xx,yy,zz);
    xi = yi = zi = -1:0.1:1;
    [xxi, yyi, zzi] = ndgrid (xi, yi, zi);
    vi = interpn (x, y, z, v, xxi, yyi, zzi, "spline");
    mesh (zi, yi, squeeze (vi(1,:,:)));
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "interpderiv1"))
    t = -2:2;
    dt = 1;
    ti =-2:0.025:2;
    dti = 0.025;
    y = sign (t);
    ys = interp1 (t,y,ti,"spline");
    yp = interp1 (t,y,ti,"pchip");
    plot (ti, ys,"r-", ti, yp,"g-");
    legend ("spline","pchip", 4);
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "interpderiv2"))
    t = -2:2;
    dt = 1;
    ti =-2:0.025:2;
    dti = 0.025;
    y = sign (t);
    ddys = diff (diff (interp1 (t,y,ti,"spline"))./dti)./dti;
    ddyp = diff (diff (interp1 (t,y,ti,"pchip"))./dti)./dti;
    plot (ti(2:end-1),ddys,"r*", ti(2:end-1),ddyp,"g+");
    legend ("spline", "pchip");
    print ([nm "." typ], d_typ);
  endif
  hide_output ();
endfunction

function set_print_size ()
  image_size = [5.0, 3.5]; # in inches, 16:9 format
  border = 0;              # For postscript use 50/72
  set (0, "defaultfigurepapertype", "<custom>");
  set (0, "defaultfigurepaperorientation", "landscape");
  set (0, "defaultfigurepapersize", image_size + 2*border);
  set (0, "defaultfigurepaperposition", [border, border, image_size]);
endfunction

## Use this function before plotting commands and after every call to
## print since print() resets output to stdout (unfortunately, gnpulot
## can't pop output as it can the terminal type).
function hide_output ()
  f = figure (1);
  set (f, "visible", "off");
endfunction

## generate something for the texinfo @image command to process
function image_as_txt(nm)
  fid = fopen (sprintf ("%s.txt", nm), "wt");
  fputs (fid, "\n");
  fputs (fid, "+---------------------------------+\n");
  fputs (fid, "| Image unavailable in text mode. |\n");
  fputs (fid, "+---------------------------------+\n");
  fclose (fid);
endfunction

