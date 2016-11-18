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

function geometryimages (nm, typ)
  graphics_toolkit ("gnuplot");
  set_print_size ();
  hide_output ();
  if (strcmp (typ, "png"))
    set (0, "defaulttextfontname", "*");
  endif
  if (strcmp (typ, "eps"))
    d_typ = "-depsc2";
  else
    d_typ = ["-d" typ];
  endif

  if (! __have_feature__ ("QHULL")
      && any (strcmp (nm, {"voronoi", "griddata", "convhull", "delaunay", ...
                           "triplot"})))
    sombreroimage (nm, typ, d_typ);
  elseif (strcmp (typ, "txt"))
    image_as_txt (nm);
  elseif (strcmp (nm, "voronoi"))
    rand ("state", 9);
    x = rand (10, 1);
    y = rand (10, 1);
    tri = delaunay (x, y);
    [vx, vy] = voronoi (x, y, tri);
    triplot (tri, x, y, "b");
    hold on;
    plot (vx, vy, "r");
    [r, c] = tri2circ (tri(end,:), x, y);
    pc = [-1:0.01:1];
    xc = r * sin (pi*pc) + c(1);
    yc = r * cos (pi*pc) + c(2);
    plot (xc, yc, "g-", "LineWidth", 3);
    axis([0, 1, 0, 1]);
    legend ("Delaunay Triangulation", "Voronoi Diagram");
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "triplot"))
    rand ("state", 2)
    x = rand (20, 1);
    y = rand (20, 1);
    tri = delaunay (x, y);
    triplot (tri, x, y);
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "griddata"))
    rand ("state", 1);
    x = 2 * rand (1000,1) - 1;
    y = 2 * rand (size (x)) - 1;
    z = sin (2 * (x.^2 + y.^2));
    [xx,yy] = meshgrid (linspace (-1,1,32));
    griddata (x,y,z,xx,yy);
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "convhull"))
    x = -3:0.05:3;
    y = abs (sin (x));
    k = convhull (x, y);
    plot (x(k),y(k),'r-', x,y,'b+');
    axis ([-3.05, 3.05, -0.05, 1.05]);
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "delaunay"))
    rand ("state", 1);
    x = rand (1, 10);
    y = rand (1, 10);
    T = delaunay (x, y);
    X = [ x(T(:,1)); x(T(:,2)); x(T(:,3)); x(T(:,1)) ];
    Y = [ y(T(:,1)); y(T(:,2)); y(T(:,3)); y(T(:,1)) ];
    axis ([0, 1, 0, 1]);
    plot (X,Y,"b", x,y,"r*");
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "inpolygon"))
    randn ("state", 2);
    x = randn (100, 1);
    y = randn (100, 1);
    vx = cos (pi * [-1 : 0.1: 1]);
    vy = sin (pi * [-1 : 0.1 : 1]);
    in = inpolygon (x, y, vx, vy);
    plot (vx, vy, x(in), y(in), "r+", x(!in), y(!in), "bo");
    axis ([-2, 2, -2, 2]);
    print ([nm "." typ], d_typ);
  else
    error ("unrecognized plot requested");
  endif
  hide_output ();
endfunction

function [r, c] = tri2circ (tri, xx, yy)
  x = xx(tri);
  y = yy(tri);
  m = (y(1:end-1) - y(2:end)) ./ (x(1:end-1) - x(2:end));
  xc = (prod(m) .* (y(1) - y(end)) + m(end)*(x(1)+x(2)) - m(1)*(x(2)+x(3))) ...
        ./ (2 * (m(end) - m(1)));
  yc = - (xc - (x(2) + x(3))./2) ./ m(end) + (y(2) + y(3)) / 2;
  c = [xc, yc];
  r = sqrt ((xc - x(1)).^2 + (yc - y(1)).^2);
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

function sombreroimage (nm, typ, d_typ)
  if (strcmp (typ, "txt"))
    fid = fopen (sprintf ("%s.txt", nm), "wt");
    fputs (fid, "+-----------------------------+\n");
    fputs (fid, "| Image unavailable because   |\n");
    fputs (fid, "| of a missing QHULL library. |\n");
    fputs (fid, "+-----------------------------+\n");
    fclose (fid);
    return;
  else ## if (!strcmp (typ, "txt"))

    hide_output ();

    [x, y, z] = sombrero ();
    unwind_protect
      mesh (x, y, z);
      title ("Sorry, graphics not available because octave was\\ncompiled without the QHULL library.");
    unwind_protect_cleanup
      print ([nm "." typ], d_typ);
      hide_output ();
    end_unwind_protect
  endif
endfunction

## generate something for the texinfo @image command to process
function image_as_txt (nm)
  fid = fopen (sprintf ("%s.txt", nm), "wt");
  fputs (fid, "\n");
  fputs (fid, "+---------------------------------+\n");
  fputs (fid, "| Image unavailable in text mode. |\n");
  fputs (fid, "+---------------------------------+\n");
  fclose (fid);
endfunction

