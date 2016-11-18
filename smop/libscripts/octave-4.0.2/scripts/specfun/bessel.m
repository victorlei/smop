## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn  {Loadable Function} {[@var{j}, @var{ierr}] =} besselj (@var{alpha}, @var{x}, @var{opt})
## @deftypefnx {Loadable Function} {[@var{y}, @var{ierr}] =} bessely (@var{alpha}, @var{x}, @var{opt})
## @deftypefnx {Loadable Function} {[@var{i}, @var{ierr}] =} besseli (@var{alpha}, @var{x}, @var{opt})
## @deftypefnx {Loadable Function} {[@var{k}, @var{ierr}] =} besselk (@var{alpha}, @var{x}, @var{opt})
## @deftypefnx {Loadable Function} {[@var{h}, @var{ierr}] =} besselh (@var{alpha}, @var{k}, @var{x}, @var{opt})
## Compute Bessel or Hankel functions of various kinds:
##
## @table @code
## @item besselj
## Bessel functions of the first kind.  If the argument @var{opt} is supplied,
## the result is multiplied by @code{exp (-abs (imag (x)))}.
##
## @item bessely
## Bessel functions of the second kind.  If the argument @var{opt} is supplied,
## the result is multiplied by @w{@code{exp (-abs (imag (x)))}}.
##
## @item besseli
## Modified Bessel functions of the first kind.  If the argument @var{opt} is
## supplied, the result is multiplied by @w{@code{exp (-abs (real (x)))}}.
##
## @item besselk
## Modified Bessel functions of the second kind.  If the argument @var{opt} is
## supplied, the result is multiplied by @w{@code{exp (x)}}.
##
## @item besselh
## Compute Hankel functions of the first (@var{k} = 1) or second (@var{k} = 2)
## kind.  If the argument @var{opt} is supplied, the result is multiplied by
## @w{@code{exp (-I*@var{x})}} for @var{k} = 1 or @w{@code{exp (I*@var{x})}}
## for @var{k} = 2.
## @end table
##
## If @var{alpha} is a scalar, the result is the same size as @var{x}.
## If @var{x} is a scalar, the result is the same size as @var{alpha}.
## If @var{alpha} is a row vector and @var{x} is a column vector, the
## result is a matrix with @code{length (@var{x})} rows and
## @code{length (@var{alpha})} columns.  Otherwise, @var{alpha} and
## @var{x} must conform and the result will be the same size.
##
## The value of @var{alpha} must be real.  The value of @var{x} may be complex.
##
## If requested, @var{ierr} contains the following status information and is
## the same size as the result.
##
## @enumerate 0
## @item
## Normal return.
##
## @item
## Input error, return @code{NaN}.
##
## @item
## Overflow, return @code{Inf}.
##
## @item
## Loss of significance by argument reduction results in less than half of
## machine accuracy.
##
## @item
## Complete loss of significance by argument reduction, return @code{NaN}.
##
## @item
## Error---no computation, algorithm termination condition not met,
## return @code{NaN}.
## @end enumerate
## @end deftypefn

function bessel ()
  error ("bessel: you must use besselj, bessely, besseli, besselk, or besselh\n");
endfunction


%!error bessel ()

