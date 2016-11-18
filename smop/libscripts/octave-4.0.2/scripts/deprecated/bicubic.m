## Copyright (C) 2005-2015 Hoxide Ma
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
## @deftypefn {Function File} {@var{zi} =} bicubic (@var{x}, @var{y}, @var{z}, @var{xi}, @var{yi}, @var{extrapval})
##
## @code{bicubic} is deprecated and will be removed in Octave version 4.4.
## Use @code{interp2 (@dots{}, "spline")} for the equivalent functionality.
##
## Return a matrix @var{zi} corresponding to the bicubic
## interpolations at @var{xi} and @var{yi} of the data supplied
## as @var{x}, @var{y} and @var{z}.  Points outside the grid are set
## to @var{extrapval}.
##
## See @url{http://wiki.woodpecker.org.cn/moin/Octave/Bicubic}
## for further information.
## @seealso{interp2}
## @end deftypefn

## Bicubic interpolation method.
## Author: Hoxide Ma <hoxide_dirac@yahoo.com.cn>

## Deprecated in version 4.0

function zi = bicubic (x, y, z, xi, yi, extrapval, spline_alpha)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "bicubic is obsolete and will be removed from a future version of Octave, please use interp2 instead");
  endif

  if (nargin < 1 || nargin > 7)
    print_usage ();
  endif

  if (nargin == 7 && isscalar (spline_alpha))
    a = spline_alpha;
  else
    a = 0.5;
  endif

  if (nargin < 6)
    extrapval = NaN;
  endif

  if (isa (x, "single") || isa (y, "single") || isa (z, "single")
      || isa (xi, "single") || isa (yi, "single"))
    myeps = eps ("single");
  else
    myeps = eps ();
  endif

  if (nargin <= 2)
    ## bicubic (z) or bicubic (z, 2)
    if (nargin == 1)
      n = 1;
    else
      n = y;
    endif
    z = x;
    x = [];
    [rz, cz] = size (z);
    s = linspace (1, cz, (cz-1) * pow2 (n) + 1);
    t = linspace (1, rz, (rz-1) * pow2 (n) + 1);
  elseif (nargin == 3)
    if (! isvector (x) || ! isvector (y))
      error ("bicubic: XI and YI must be vector");
    endif
    s = y;
    t = z;
    z = x;
    [rz, cz] = size (z);
  elseif (nargin == 5 || nargin == 6)
    [rz, cz] = size (z) ;
    if (isvector (x) && isvector (y))
      if (rz != length (y) || cz != length (x))
        error ("bicubic: length of X and Y must match the size of Z");
      endif
    elseif (size_equal (x, y) && size_equal (x, z))
      x = x(1,:);
      y = y(:,1);
    else
      error ("bicubic: X, Y and Z must be equal size matrices of same size");
    endif

    if (all (diff (x) < 0))
      flipx = true;
      x = fliplr (x);
    elseif (all (diff (x) > 0))
      flipx = false;
    else
      error ("bicubic:nonmonotonic", "bicubic: X values must be monotonic");
    endif
    if (all (diff (y) < 0))
      flipy = true;
      y = flipud (y);
    elseif (all (diff (y) > 0))
      flipy = false;
    else
      error ("bicubic:nonmonotonic", "bicubic: Y values must be monotonic");
    endif

    ## Mark values outside the lookup table.
    xfirst_ind = find (xi < x(1));
    xlast_ind  = find (xi > x(cz));
    yfirst_ind = find (yi < y(1));
    ylast_ind  = find (yi > y(rz));
    ## Set value outside the table preliminary to min max index.
    xi(xfirst_ind) = x(1);
    xi(xlast_ind) = x(cz);
    yi(yfirst_ind) = y(1);
    yi(ylast_ind) = y(rz);

    x = reshape (x, 1, cz);
    x(cz) *= 1 + sign (x(cz)) * myeps;
    if (x(cz) == 0)
      x(cz) = myeps;
    endif;
    xi = reshape (xi, 1, length (xi));
    [m, i] = sort ([x, xi]);
    o = cumsum (i <= cz);
    xidx = o(find (i > cz));

    y = reshape (y, rz, 1);
    y(rz) *= 1 + sign (y(rz)) * myeps;
    if (y(rz) == 0)
      y(rz) = myeps;
    endif;
    yi = reshape (yi, length (yi), 1);
    [m, i] = sort ([y; yi]);
    o = cumsum (i <= rz);
    yidx = o([find(i > rz)]);

    ## Set s and t used follow codes.
    s = xidx + ((xi .- x(xidx)) ./ (x(xidx+1) .- x(xidx)));
    t = yidx + ((yi  - y(yidx)) ./ (y(yidx+1)  - y(yidx)));

    if (flipx)
      s = fliplr (s);
    endif
    if (flipy)
      t = flipud (t);
    endif
  else
    print_usage ();
  endif

  if (rz < 3 || cz < 3)
    error ("bicubic: Z at least a 3 by 3 matrices");
  endif

  inds = floor (s);
  d = find (s == cz);
  s = s - floor (s);
  inds(d) = cz-1;
  s(d) = 1.0;

  d = [];
  indt = floor (t);
  d = find (t == rz);
  t = t - floor (t);
  indt(d) = rz-1;
  t(d) = 1.0;
  d = [];

  p = zeros (size (z) + 2);
  p(2:rz+1,2:cz+1) = z;
  p(1,:) =    (6*(1-a))*p(2,:)    - 3*p(3,:)  + (6*a-2)*p(4,:);
  p(rz+2,:) = (6*(1-a))*p(rz+1,:) - 3*p(rz,:) + (6*a-2)*p(rz-1,:);
  p(:,1) =    (6*(1-a))*p(:,2)    - 3*p(:,3)  + (6*a-2)*p(:,4);
  p(:,cz+2) = (6*(1-a))*p(:,cz+1) - 3*p(:,cz) + (6*a-2)*p(:,cz-1);

  ## Calculte the C1(t) C2(t) C3(t) C4(t) and C1(s) C2(s) C3(s) C4(s).
  t2 = t.*t;
  t3 = t2.*t;

  ct0 =    -a .* t3 +     (2 * a) .* t2 - a .* t ;      # -a G0
  ct1 = (2-a) .* t3 +      (-3+a) .* t2          + 1 ;  # F0 - a G1
  ct2 = (a-2) .* t3 + (-2 *a + 3) .* t2 + a .* t ;      # F1 + a G0
  ct3 =     a .* t3 -           a .* t2;                # a G1
  t = []; t2 = []; t3 = [];

  s2 = s.*s;
  s3 = s2.*s;

  cs0 =    -a .* s3 +     (2 * a) .* s2 - a .*s ;      # -a G0
  cs1 = (2-a) .* s3 +    (-3 + a) .* s2         + 1 ;  # F0 - a G1
  cs2 = (a-2) .* s3 + (-2 *a + 3) .* s2 + a .*s ;      # F1 + a G0
  cs3 =     a .* s3 -           a .* s2;               # a G1
  s = []; s2 = []; s3 = [];

  cs0 = cs0([1,1,1,1],:);
  cs1 = cs1([1,1,1,1],:);
  cs2 = cs2([1,1,1,1],:);
  cs3 = cs3([1,1,1,1],:);

  lent = length (ct0);
  lens = columns (cs0);
  zi = zeros (lent, lens);

  for i = 1:lent
    it = indt(i);
    int = [it, it+1, it+2, it+3];
    zi(i,:) = ([ct0(i),ct1(i),ct2(i),ct3(i)]
              * (p(int,inds) .* cs0 + p(int,inds+1) .* cs1
                 + p(int,inds+2) .* cs2 + p(int,inds+3) .* cs3));
  endfor

  ## Set points outside the table to extrapval.
  if (! (isempty (xfirst_ind) && isempty (xlast_ind)))
    zi(:, [xfirst_ind, xlast_ind]) = extrapval;
  endif
  if (! (isempty (yfirst_ind) && isempty (ylast_ind)))
    zi([yfirst_ind; ylast_ind], :) = extrapval;
  endif

endfunction


%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,4]+10;
%! y = [-10,-9,-8];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26)';
%! mesh (xi, yi, bicubic (x,y,A,xi,yi));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x(:),y(:),A(:),"b*"); hold off;

%!test
%! x = linspace (1, -1, 10);
%! [xx, yy] = meshgrid (x);
%! z = cos (6 * xx) + sin (6 * yy);
%! x = linspace (1, -1, 30);
%! [xx2, yy2] = meshgrid (x);
%! z1 = interp2 (xx, yy, z, xx2, yy2, "spline");
%! z2 = interp2 (fliplr (xx), flipud (yy), fliplr (flipud(z)),
%!               fliplr (xx2), flipud (yy2), "spline");
%! z2 = fliplr (flipud (z2));
%! assert (z1, z2, 100 * eps ())

