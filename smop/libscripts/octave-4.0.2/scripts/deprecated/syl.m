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

## -*- texinfo -*-
## @deftypefn {Built-in Function} {@var{x} =} syl (@var{A}, @var{B}, @var{C})
##
## @code{syl} is deprecated and will be removed in Octave version 4.4.
## Use @code{sylvester} for the equivalent functionality.
##
## Solve the Sylvester equation
## @tex
## $$
##  A X + X B + C = 0
## $$
## @end tex
## @ifnottex
##
## @example
## A X + X B + C = 0
## @end example
##
## @end ifnottex
## using standard @sc{lapack} subroutines.  For example:
##
## @example
## @group
## syl ([1, 2; 3, 4], [5, 6; 7, 8], [9, 10; 11, 12])
##    @result{} [ -0.50000, -0.66667; -0.66667, -0.50000 ]
## @end group
## @end example
## @end deftypefn

## Deprecated in version 4.0

function x = syl (A, B, C)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "syl is obsolete and will be removed from a future version of Octave, please use sylvester instead");
  endif

  if (nargin != 3 || nargout > 1)
    print_usage ();
  endif

  x = -sylvester (A, B, C);

endfunction

