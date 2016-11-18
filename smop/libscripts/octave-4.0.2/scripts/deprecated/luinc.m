## Copyright (C) 2014-2015 John W. Eaton
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

## @deftypefn  {Built-in Function} {[@var{L}, @var{U}, @var{P}, @var{Q}] =} luinc (@var{A}, '0')
## @deftypefnx {Built-in Function} {[@var{L}, @var{U}, @var{P}, @var{Q}] =} luinc (@var{A}, @var{droptol})
## @deftypefnx {Built-in Function} {[@var{L}, @var{U}, @var{P}, @var{Q}] =} luinc (@var{A}, @var{opts})
##
## @code{luinc} is deprecated and will be removed in Octave version 4.4.
## Please use @code{ilu} or @code{ichol} in all new code.
##
## Produce the incomplete LU@tie{}factorization of the sparse matrix @var{A}.
## Two types of incomplete factorization are possible, and the type
## is determined by the second argument to @code{luinc}.
##
## Called with a second argument of @qcode{'0'}, the zero-level incomplete
## LU@tie{}factorization is produced.  This creates a factorization of @var{A}
## where the position of the nonzero arguments correspond to the same
## positions as in the matrix @var{A}.
##
## Alternatively, the fill-in of the incomplete LU@tie{}factorization can
## be controlled through the variable @var{droptol} or the structure
## @var{opts}.  The @sc{umfpack} multifrontal factorization code by Tim A.
## Davis is used for the incomplete LU@tie{}factorization, (availability
## @url{http://www.cise.ufl.edu/research/sparse/umfpack/})
##
## @var{droptol} determines the values below which the values in the
## LU@tie{} factorization are dropped and replaced by zero.  It must be a
## positive scalar, and any values in the factorization whose absolute value
## are less than this value are dropped, expect if leaving them increase the
## sparsity of the matrix.  Setting @var{droptol} to zero results in a complete
## LU@tie{}factorization which is the default.
##
## @var{opts} is a structure containing one or more of the fields
##
## @table @code
## @item droptol
## The drop tolerance as above.  If @var{opts} only contains @code{droptol}
## then this is equivalent to using the variable @var{droptol}.
##
## @item milu
## A logical variable flagging whether to use the modified incomplete
## LU@tie{} factorization.  In the case that @code{milu} is true, the dropped
## values are subtracted from the diagonal of the matrix @var{U} of the
## factorization.  The default is @code{false}.
##
## @item udiag
## A logical variable that flags whether zero elements on the diagonal of
## @var{U} should be replaced with @var{droptol} to attempt to avoid singular
## factors.  The default is @code{false}.
##
## @item thresh
## Defines the pivot threshold in the interval [0,1].  Values outside that
## range are ignored.
## @end table
##
## All other fields in @var{opts} are ignored.  The outputs from @code{luinc}
## are the same as for @code{lu}.
##
## Given the string argument @qcode{\"vector\"}, @code{luinc} returns the
## values of @var{p} @var{q} as vector values.
## @seealso{ilu, ichol, lu, sparse}
## @end deftypefn

## Deprecated in version 4.0

function [L, U, P, Q] = luinc (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "luinc is obsolete and will be removed from a future version of Octave, please use ilu or ichol instead");
  endif

  [L, U, P, Q] = __luinc__ (varargin{:});

endfunction

