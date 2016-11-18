## Copyright (C) 2004-2015 Paul Kienzle
## Copyright (C) 2012 Jordi Gutiérrez Hermoso
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
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn  {Function File} {} __sprand_impl__ (@var{s}, @var{randfun})
## @deftypefnx {Function File} {} __sprand_impl__ (@var{m}, @var{n}, @var{d}, @var{fcnname}, @var{randfun})
## @deftypefnx {Function File} {} __sprand_impl__ (@var{m}, @var{n}, @var{d}, @var{rc}, @var{fcnname}, @var{randfun})
## Undocumented internal function.
## @end deftypefn

## Actual implementation of sprand and sprandn happens here.

function S = __sprand_impl__ (varargin)

  if (nargin == 2)
    [m, randfun] = deal (varargin{1:2});
    [i, j] = find (m);
    [nr, nc] = size (m);
    S = sparse (i, j, randfun (size (i)), nr, nc);
  else
    if (nargin == 5)
      [m, n, d, fcnname, randfun] = deal (varargin{:});
    else
      [m, n, d, rc, fcnname, randfun] = deal (varargin{:});
    endif

    if (! (isscalar (m) && m == fix (m) && m > 0))
      error ("%s: M must be an integer greater than 0", fcnname);
    endif
    if (! (isscalar (n) && n == fix (n) && n > 0))
      error ("%s: N must be an integer greater than 0", fcnname);
    endif
    if (d < 0 || d > 1)
      error ("%s: density D must be between 0 and 1", fcnname);
    endif

    if (nargin == 5)
      mn = m*n;
      k = round (d*mn);
      if (mn > sizemax ())
        ## randperm will overflow, so use alternative methods

        idx = unique (fix (rand (1.01*k, 1) * mn)) + 1;

        ## idx contains random numbers in [1,mn]
        ## Generate 1% more random values than necessary in order to reduce the
        ## probability that there are less than k distinct values; maybe a
        ## better strategy could be used but I don't think it's worth the price.

        ## actual number of entries in S
        k = min (length (idx), k);
        j = floor ((idx(1:k) - 1) / m);
        i = idx(1:k) - j * m;
        j++;
      else
        idx = randperm (mn, k);
        [i, j] = ind2sub ([m, n], idx);
      endif

      S = sparse (i, j, randfun (k, 1), m, n);

    elseif (nargin == 6)
      ## Create a matrix with specified reciprocal condition number.

      if (! isscalar (rc) && ! isvector (rc))
        error ("%s: RC must be a scalar or vector", fcnname);
      endif

      ## We want to reverse singular valued decomposition A=U*S*V'.
      ## First, first S is constructed and then U = U1*U2*..Un and
      ## V' = V1*V2*..Vn are seen as Jacobi rotation matrices with angles and
      ## planes of rotation randomized.  Repeatedly apply rotations until the
      ## required density for A is achieved.

      if (isscalar (rc))
        if (rc < 0 || rc > 1)
          error ("%s: reciprocal condition number RC must be between 0 and 1", fcnname);
        endif
        ## Reciprocal condition number is ratio of smallest SV to largest SV
        ## Generate singular values randomly and sort them to build S
        ## Random singular values in range [rc, 1].
        v = rand (1, min (m,n)) * (1 - rc) + rc;
        v(1) = 1;
        v(end) = rc;
        v = sort (v, "descend");
        S = sparse (diag (v, m, n));
      else
        ## Only the min (m, n) greater singular values from rc vector are used.
        if (length (rc) > min (m,n))
          rc = rc(1:min(m, n));
        endif
        S = sparse (diag (sort (rc, "descend"), m, n));
      endif

      Uinit = speye (m);
      Vinit = speye (n);
      k = round (d*m*n);
      while (nnz (S) < k)
        if (m > 1)
          ## Construct U randomized rotation matrix
          rot_angleu = 2 * pi * rand ();
          cu = cos (rot_angleu); su = sin (rot_angleu);
          rndtmp = randperm (m, 2);
          i = rndtmp(1); j = rndtmp(2);
          U = Uinit;
          U(i, i) = cu; U(i, j) = -su;
          U(j, i) = su; U(j, j) = cu;
          S = U * S;
        endif
        if (n > 1)
          ## Construct V' randomized rotation matrix
          rot_anglev = 2 * pi * rand ();
          cv = cos (rot_anglev); sv = sin (rot_anglev);
          rndtmp = randperm (n, 2);
          i = rndtmp(1); j = rndtmp(2);
          V = Vinit;
          V(i, i) = cv;  V(i, j) = sv;
          V(j, i) = -sv; V(j, j) = cv;
          S = S * V;
        endif
      endwhile
    endif
  endif

endfunction

