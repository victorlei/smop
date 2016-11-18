## Copyright (C) 2001, 2013 David Billinghurst
## Copyright (C) 2001, 2013 Paul Kienzle
## Copyright (C) 2003, 2013 Jaakko Ruohio
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
## @deftypefn  {Function File} {@var{k} =} ellipke (@var{m})
## @deftypefnx {Function File} {@var{k} =} ellipke (@var{m}, @var{tol})
## @deftypefnx {Function File} {[@var{k}, @var{e}] =} ellipke (@dots{})
## Compute complete elliptic integrals of the first K(@var{m}) and second
## E(@var{m}) kind.
##
## @var{m} must be a scalar or real array with -Inf @leq{} @var{m} @leq{} 1.
##
## The optional input @var{tol} controls the stopping tolerance of the
## algorithm and defaults to @code{eps (class (@var{m}))}.  The tolerance can
## be increased to compute a faster, less accurate approximation.
##
## When called with one output only elliptic integrals of the first kind are
## returned.
##
## Mathematical Note:
##
## Elliptic integrals of the first kind are defined as
##
## @tex
## $$
## {\rm K} (m) = \int_0^1 {dt \over \sqrt{(1 - t^2) (1 - m t^2)}}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##          1
##         /               dt
## K (m) = | ------------------------------
##         / sqrt ((1 - t^2)*(1 - m*t^2))
##        0
## @end group
## @end example
##
## @end ifnottex
##
## Elliptic integrals of the second kind are defined as
##
## @tex
## $$
## {\rm E} (m) = \int_0^1 {\sqrt{1 - m t^2} \over \sqrt{1 - t^2}} dt
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##          1
##         /  sqrt (1 - m*t^2)
## E (m) = |  ------------------ dt
##         /  sqrt (1 - t^2)
##        0
## @end group
## @end example
##
## @end ifnottex
##
## Reference: Milton @nospell{Abramowitz} and Irene A. @nospell{Stegun},
## @cite{Handbook of Mathematical Functions}, Chapter 17, Dover, 1965.
## @seealso{ellipj}
## @end deftypefn

## Author: David Billinghurst <David.Billinghurst@riotinto.com>
## Author: Paul Kienzle <pkienzle@users.sf.net>
## Author: Jaakko Ruohio

function [k, e] = ellipke (m, tol = [])

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  sz = size (m);
  if (! isreal (m))
    error ("ellipke: M must be real");
  elseif (any (m > 1))
    error ("ellipke: M must be <= 1");
  endif

  if (isempty (tol))
    tol = eps (class (m));
  elseif (! (isreal (tol) && isscalar (tol) && tol > 0))
    error ("ellipke: TOL must be a real scalar > 0")
  endif

  k = e = zeros (sz);

  ## Handle extreme values
  idx_1 = (m == 1);
  k(idx_1) = Inf;
  e(idx_1) = 1;

  idx_neginf = (m == -Inf);
  k(idx_neginf) = 0;
  e(idx_neginf) = Inf;

  ## Arithmetic-Geometric Mean (AGM) algorithm
  ## ( Abramowitz and Stegun, Section 17.6 )
  Nmax = 16;
  idx = !idx_1 & !idx_neginf;
  if (any (idx))
    idx_neg = find (m < 0 & !idx_neginf);
    mult_k = 1./sqrt (1 - m(idx_neg));
    mult_e = sqrt (1 - m(idx_neg));
    m(idx_neg) = -m(idx_neg) ./ (1 - m(idx_neg));
    b = sqrt (1 - m(idx));
    a = ones (size (b));
    c = sqrt (m(idx));
    f = 0.5;
    sum = f*c.^2;
    n = 2;
    do
      t = (a + b)/2;
      c = (a - b)/2;
      b = sqrt (a .* b);
      a = t;
      f *= 2;
      sum += f*c.^2;
    until (all (c./a < tol) || (++n > Nmax))
    if (n >= Nmax)
      error ("ellipke: algorithm did not converge in %d iterations", Nmax);
    endif
    k(idx) = 0.5*pi ./ a;
    e(idx) = 0.5*pi*(1 - sum) ./ a;
    k(idx_neg) = mult_k .* k(idx_neg);
    e(idx_neg) = mult_e .* e(idx_neg);
  endif

endfunction


## Test complete elliptic functions of first and second kind
## against "exact" solution from Mathematica 3.0
%!test
%! m = [0.0, 0.01; 0.1, 0.5; 0.9, 0.99; 1.0, 0.0];
%! [k,e] = ellipke (m);
%!
%! k_exp = [1.5707963267948966192, 1.5747455615173559527
%!          1.6124413487202193982, 1.8540746773013719184
%!          2.5780921133481731882, 3.6956373629898746778
%!          Inf                  , 1.5707963267948966192 ];
%! e_exp = [1.5707963267948966192, 1.5668619420216682912
%!          1.5307576368977632025, 1.3506438810476755025
%!          1.1047747327040733261, 1.0159935450252239356
%!          1.0                  , 1.5707963267948966192 ];
%! assert (k, k_exp, 8*eps);
%! assert (e, e_exp, 8*eps);

## Test against A&S Table 17.1
%!test
%! m = [0:5:50]'/100;
%! k_exp = [1.570796326794897;
%!          1.591003453790792;
%!          1.612441348720219;
%!          1.635256732264580;
%!          1.659623598610528;
%!          1.685750354812596;
%!          1.713889448178791;
%!          1.744350597225613;
%!          1.777519371491253;
%!          1.813883936816983;
%!          1.854074677301372 ];
%! e_exp = [1.570796327;
%!          1.550973352;
%!          1.530757637;
%!          1.510121831;
%!          1.489035058;
%!          1.467462209;
%!          1.445363064;
%!          1.422691133;
%!          1.399392139;
%!          1.375401972;
%!          1.350643881 ];
%! [k,e] = ellipke (m);
%! assert (k, k_exp, 1e-15);
%! assert (e, e_exp, 1e-8);

## Test negative values against "exact" solution from Mathematica.
%! m = [-0.01; -1; -5; -100; -1000; -Inf];
%! [k,e] = ellipke (m);
%!
%! k_exp = [1.5668912730681963584;
%!          1.3110287771460599052;
%!          0.9555039270640439337;
%!          0.3682192486091410329;
%!          0.1530293349884987857;
%!          0];
%! e_exp = [1.5747159850169884130;
%!          1.9100988945138560089;
%!          2.8301982463458773125;
%!          10.209260919814572009;
%!          31.707204053711259719;
%!          Inf ];
%! assert (k, k_exp, 8*eps);
%! assert (e, e_exp, 8*eps (e_exp));

## Test input validation
%!error ellipke ()
%!error ellipke (1,2,3)
%!error <M must be real> ellipke (1i)
%!error <M must be .= 1> ellipke (2)
%!error <TOL must be a real scalar . 0> ellipke (1, i)
%!error <TOL must be a real scalar . 0> ellipke (1, [1 1])
%!error <TOL must be a real scalar . 0> ellipke (1, -1)

