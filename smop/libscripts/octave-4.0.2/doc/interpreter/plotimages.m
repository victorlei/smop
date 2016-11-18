## Copyright (C) 2007-2015 John W. Eaton and David Bateman
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

function plotimages (nm, typ)
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

  if (strcmp(typ , "txt"))
    image_as_txt(nm);
  elseif (strcmp (nm, "plot"))
    x = -10:0.1:10;
    plot (x, sin (x));
    xlabel ("x");
    ylabel ("sin (x)");
    title ("Simple 2-D Plot");
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "hist"))
    randn ("state", 1);
    hist (randn (10000, 1), 30);
    xlabel ("Value");
    ylabel ("Count");
    title ("Histogram of 10,000 normally distributed random numbers");
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "errorbar"))
    rand ("state", 2);
    x = 0:0.1:10;
    y = sin (x);
    lerr = 0.1 .* rand (size (x));
    uerr = 0.1 .* rand (size (x));
    errorbar (x, y, lerr, uerr);
    axis ([0, 10, -1.1, 1.1]);
    xlabel ("x");
    ylabel ("sin (x)");
    title ("Errorbar plot of sin (x)");
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "polar"))
    polar (0:0.1:10*pi, 0:0.1:10*pi);
    title ("Example polar plot from 0 to 10*pi");
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "mesh"))
    tx = ty = linspace (-8, 8, 41)';
    [xx, yy] = meshgrid (tx, ty);
    r = sqrt (xx .^ 2 + yy .^ 2) + eps;
    tz = sin (r) ./ r;
    mesh (tx, ty, tz);
    xlabel ("tx");
    ylabel ("ty");
    zlabel ("tz");
    title ("3-D Sombrero plot");
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "plot3"))
    t = 0:0.1:10*pi;
    r = linspace (0, 1, numel (t));
    z = linspace (0, 1, numel (t));
    plot3 (r.*sin(t), r.*cos(t), z);
    xlabel ("r.*sin (t)");
    ylabel ("r.*cos (t)");
    zlabel ("z");
    title ("plot3 display of 3-D helix");
    print ([nm "." typ], d_typ);
  elseif (strcmp (nm, "extended"))
    x = 0:0.01:3;
    plot (x,erf(x));
    hold on;
    plot (x,x,"r");
    axis ([0, 3, 0, 1]);
    text (0.65, 0.6175, ['\leftarrow x = {2/\surd\pi {\fontsize{16}' ...
          '\int_{\fontsize{8}0}^{\fontsize{8}x}} e^{-t^2} dt} = 0.6175']);
    xlabel ("x");
    ylabel ("erf (x)");
    title ("erf (x) with text annotation");
    print ([nm "." typ], d_typ);
  else
    error ("unrecognized plot requested");
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
