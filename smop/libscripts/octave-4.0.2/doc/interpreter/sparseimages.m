## Copyright (C) 2006-2015 David Bateman
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

function sparseimages (nm, typ)
  graphics_toolkit ("gnuplot");
  set_print_size ();
  if (strcmp (typ, "png"))
    set (0, "defaulttextfontname", "*");
  endif

  if (__have_feature__ ("COLAMD")
      && __have_feature__ ("CHOLMOD")
      && __have_feature__ ("UMFPACK"))
    if (strcmp (typ,"txt"))
      txtimages (nm, 15, typ);
    else
      if (strcmp (nm, "gplot"))
        gplotimages ("gplot", typ);
      elseif (strcmp (nm, "grid"))
        femimages ("grid", typ);
      else
        otherimages (nm, 200, typ);
      endif
    endif
  else ## There is no sparse matrix implementation available because
       ## of missing libraries, plot sombreros instead
    sombreroimage (nm, typ);
  endif
endfunction

function set_print_size ()
  image_size = [5.0, 3.5]; # in inches, 16:9 format
  border = 0;              # For postscript use 50/72
  set (0, "defaultfigurepapertype", "<custom>");
  set (0, "defaultfigurepaperorientation", "landscape");
  set (0, "defaultfigurepapersize", image_size + 2*border);
  set (0, "defaultfigurepaperposition", [border, border, image_size]);
endfunction

function hide_output ()
  f = figure (1);
  set (f, "visible", "off");
endfunction

function gplotimages (nm, typ)
  hide_output ();
  if (strcmp (typ, "eps"))
    d_typ = "-depsc2";
  else
    d_typ = ["-d" typ];
  endif

  A = sparse ([2,6,1,3,2,4,3,5,4,6,1,5],
              [1,1,2,2,3,3,4,4,5,5,6,6], 1, 6, 6);
  xy = [0,4,8,6,4,2;5,0,5,7,5,7]';
  gplot (A, xy);
  print ([nm "." typ], d_typ);
  hide_output ();
endfunction

function txtimages (nm, n, typ)
  a = 10*speye (n) + sparse (1:n,ceil([1:n]/2),1,n,n) + ...
      sparse (ceil ([1:n]/2),1:n,1,n,n);
  if (strcmp (nm, "gplot") || strcmp (nm, "grid"))
    fid = fopen (sprintf ("%s.txt", nm), "wt");
    fputs (fid, "\n");
    fputs (fid, "+---------------------------------+\n");
    fputs (fid, "| Image unavailable in text mode. |\n");
    fputs (fid, "+---------------------------------+\n");
    fclose (fid);
  elseif (strcmp (nm, "spmatrix"))
    printsparse (a, ["spmatrix." typ]);
  else
    if (__have_feature__ ("COLAMD") && __have_feature__ ("CHOLMOD"))
      if (strcmp (nm, "spchol"))
        r1 = chol (a);
        printsparse (r1, ["spchol." typ]);
      elseif (strcmp (nm, "spcholperm"))
        [r2,p2,q2] = chol (a);
        printsparse(r2, ["spcholperm." typ]);
      endif
      ## printf("Text NNZ: Matrix %d, Chol %d, PermChol %d\n",nnz(a),nnz(r1),nnz(r2));
    endif
  endif
endfunction

function otherimages (nm, n, typ)
  hide_output ();
  if (strcmp (typ, "eps"))
    d_typ = "-depsc2";
  else
    d_typ = ["-d" typ];
  endif

  a = 10*speye (n) + sparse (1:n,ceil([1:n]/2),1,n,n) + ...
      sparse (ceil ([1:n]/2),1:n,1,n,n);
  if (strcmp (nm, "spmatrix"))
    spy (a);
    axis ("ij");
    print (["spmatrix." typ], d_typ);
    hide_output ();
  else
    if (__have_feature__ ("COLAMD") && __have_feature__ ("CHOLMOD"))
      if (strcmp (nm, "spchol"))
        r1 = chol (a);
        spy (r1);
        axis ("ij");
        print (["spchol." typ], d_typ);
        hide_output ();
      elseif (strcmp (nm, "spcholperm"))
        [r2,p2,q2] = chol (a);
        spy (r2);
        axis ("ij");
        print (["spcholperm." typ], d_typ);
        hide_output ();
      endif
      ## printf("Image NNZ: Matrix %d, Chol %d, PermChol %d\n",nnz(a),nnz(r1),nnz(r2));
    endif
  endif
endfunction

function printsparse (a, nm)
  fid = fopen (nm,"wt");
  fputs (fid, "\n");
  for i = 1:rows (a)
    if (rem (i,5) == 0)
      fprintf (fid,"         %2d - ", i);
    else
      fprintf (fid,"            | ");
    endif
    for j = 1:columns (a)
      if (a(i,j) == 0)
        fprintf (fid,"  ");
      else
        fprintf (fid," *");
      endif
    endfor
    fprintf (fid,"\n");
  endfor
  fprintf (fid,"            |-");
  for j = 1:columns (a)
    if (rem (j,5) == 0)
      fprintf (fid,"-|");
    else
      fprintf (fid,"--");
    endif
  endfor
  fprintf (fid,"\n");
  fprintf (fid,"              ");
  for j = 1:columns (a)
    if (rem (j,5) == 0)
      fprintf (fid,"%2d",j);
    else
      fprintf (fid,"  ");
    endif
  endfor
  fclose (fid);
endfunction

function femimages (nm, typ)
  hide_output ();
  if (strcmp (typ, "eps"))
    d_typ = "-depsc2";
  else
    d_typ = ["-d" typ];
  endif

  if (__have_feature__ ("COLAMD")
      && __have_feature__ ("CHOLMOD")
      && __have_feature__ ("UMFPACK"))
    ## build a rectangle
    node_y = [1;1.2;1.5;1.8;2] * ones (1,11);
    node_x = ones (5,1) * [1,1.05,1.1,1.2,1.3,1.5,1.7,1.8,1.9,1.95,2];
    nodes = [node_x(:), node_y(:)];

    [h,w] = size (node_x);
    elems = [];
    for idx = 1 : w-1
      widx = (idx-1)*h;
      elems = [elems; widx+[(1:h-1);(2:h);h+(1:h-1)]'];
      elems = [elems; widx+[(2:h);h+(2:h);h+(1:h-1)]'];
    endfor

    E = size (elems,1);  # No. of elements
    N = size (nodes,1);  # No. of elements
    D = size (elems,2);  # dimensions+1

    ## Plot FEM Geometry
    elemx = elems(:,[1,2,3,1])';
    xelems = reshape (nodes(elemx, 1), 4, E);
    yelems = reshape (nodes(elemx, 2), 4, E);

    ## Set element conductivity
    conductivity = [1*ones(1,16), 2*ones(1,48), 1*ones(1,16)];

    ## Dirichlet boundary conditions
    D_nodes = [1:5, 51:55];
    D_value = [10*ones(1,5), 20*ones(1,5)];

    ## Neumann boundary conditions
    ## Note that N_value must be normalized by the boundary
    ##   length and element conductivity
    N_nodes = [];
    N_value = [];

    ## Calculate connectivity matrix
    C = sparse ((1:D*E), reshape (elems',D*E,1),1, D*E, N);

    ## Calculate stiffness matrix
    Siidx = floor ([0:D*E-1]'/D)*D*ones(1,D) + ones(D*E,1)*(1:D);
    Sjidx = [1:D*E]'*ones (1,D);
    Sdata = zeros (D*E,D);
    dfact = prod (2:(D-1));
    for j = 1:E
      a = inv ([ ones(D,1), nodes( elems(j,:), : ) ]);
      const = conductivity(j)*2/dfact/abs (det (a));
      Sdata(D*(j-1)+(1:D),:) = const * a(2:D,:)'*a(2:D,:);
    endfor

    ## Element-wise system matrix
    SE = sparse (Siidx,Sjidx,Sdata);
    ## Global system matrix
    S = C'* SE *C;

    ## Set Dirichlet boundary
    V = zeros (N,1);
    V(D_nodes) = D_value;
    idx = 1:N;
    idx(D_nodes) = [];

    ## Set Neumann boundary
    Q = zeros (N,1);
    Q(N_nodes) = N_value; # FIXME

    V(idx) = S(idx,idx) \ ( Q(idx) - S(idx,D_nodes)*V(D_nodes) );

    velems = reshape (V(elemx), 4, E);

    plot3 (xelems, yelems, velems);
    view (80, 10);
    print ([nm "." typ], d_typ);
    hide_output ();
  endif
endfunction

## There is no sparse matrix implementation available because of missing
## libraries, plot sombreros instead. Also plot a nice title that we are
## sorry about that.
function sombreroimage (nm, typ)
  if (strcmp (typ, "txt"))
    fid = fopen (sprintf ("%s.txt", nm), "wt");
    fputs (fid, "\n");
    fputs (fid, "+---------------------------------------+\n");
    fputs (fid, "| Image unavailable because of a        |\n");
    fputs (fid, "| missing sparse matrix implementation. |\n");
    fputs (fid, "+---------------------------------------+\n");
    fclose (fid);
    return;
  else ## if (!strcmp (typ, "txt"))

    hide_output ();
    if (strcmp (typ, "eps"))
      d_typ = "-depsc2";
    else
      d_typ = ["-d" typ];
    endif

    [x, y, z] = sombrero ();
    unwind_protect
      mesh (x, y, z);
      title ("Sorry, graphics are unavailable because Octave was\ncompiled without a sparse matrix implementation.");
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

