## Copyright (C) 1995-2015 Kurt Hornik
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
## @deftypefn {Function File} {} cloglog (@var{x})
## Return the complementary log-log function of @var{x}.
##
## The complementary log-log function is defined as
## @tex
## $$
## {\rm cloglog}(x) = - \log (- \log (x))
## $$
## @end tex
## @ifnottex
##
## @example
## cloglog (x) = - log (- log (@var{x}))
## @end example
##
## @end ifnottex
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Complementary log-log function

function y = cloglog (x)

  if (nargin != 1)
    print_usage ();
  endif

  y = - log (- log (x));

endfunction


%!assert (cloglog (0), -Inf)
%!assert (cloglog (1), Inf)
%!assert (cloglog (1/e), 0)

## Test input validation
%!error cloglog ()
%!error cloglog (1, 2)

