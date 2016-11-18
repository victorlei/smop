## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn {Function File} {@var{s} =} spaugment (@var{A}, @var{c})
## Create the augmented matrix of @var{A}.
##
## This is given by
##
## @example
## @group
## [@var{c} * eye(@var{m}, @var{m}), @var{A};
##             @var{A}', zeros(@var{n}, @var{n})]
## @end group
## @end example
##
## @noindent
## This is related to the least squares solution of
## @code{@var{A} \ @var{b}}, by
##
## @example
## @group
## @var{s} * [ @var{r} / @var{c}; x] = [ @var{b}, zeros(@var{n}, columns(@var{b})) ]
## @end group
## @end example
##
## @noindent
## where @var{r} is the residual error
##
## @example
## @var{r} = @var{b} - @var{A} * @var{x}
## @end example
##
## As the matrix @var{s} is symmetric indefinite it can be factorized with
## @code{lu}, and the minimum norm solution can therefore be found without the
## need for a @code{qr} factorization.  As the residual error will be
## @code{zeros (@var{m}, @var{m})} for underdetermined problems, and example
## can be
##
## @example
## @group
## m = 11; n = 10; mn = max (m, n);
## A = spdiags ([ones(mn,1), 10*ones(mn,1), -ones(mn,1)],
##              [-1, 0, 1], m, n);
## x0 = A \ ones (m,1);
## s = spaugment (A);
## [L, U, P, Q] = lu (s);
## x1 = Q * (U \ (L \ (P  * [ones(m,1); zeros(n,1)])));
## x1 = x1(end - n + 1 : end);
## @end group
## @end example
##
## To find the solution of an overdetermined problem needs an estimate of the
## residual error @var{r} and so it is more complex to formulate a minimum norm
## solution using the @code{spaugment} function.
##
## In general the left division operator is more stable and faster than using
## the @code{spaugment} function.
## @seealso{mldivide}
## @end deftypefn

function s = spaugment (A, c)
  if (nargin < 2)
    if (issparse (A))
      c = max (max (abs (A))) / 1000;
    else
      if (ndims (A) != 2)
        error ("spaugment: expecting 2-dimenisional matrix");
      else
        c = max (abs (A(:))) / 1000;
      endif
    endif
  elseif (! isscalar (c))
    error ("spaugment: C must be a scalar");
  endif

  [m, n] = size (A);
  s = [ c * speye(m, m), A; A', sparse(n, n)];
endfunction


%!testif HAVE_UMFPACK
%! m = 11; n = 10; mn = max (m ,n);
%! A = spdiags ([ones(mn,1), 10*ones(mn,1), -ones(mn,1)],[-1,0,1], m, n);
%! x0 = A \ ones (m,1);
%! s = spaugment (A);
%! [L, U, P, Q] = lu (s);
%! x1 = Q * (U \ (L \ (P  * [ones(m,1); zeros(n,1)])));
%! x1 = x1(end - n + 1 : end);
%! assert (x1, x0, 1e-6);

