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
## @deftypefn {Function File} {} logistic_pdf (@var{x})
## For each element of @var{x}, compute the PDF at @var{x} of the
## logistic distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the logistic distribution

function pdf = logistic_pdf (x)

  if (nargin != 1)
    print_usage ();
  endif

  if (iscomplex (x))
    error ("logistic_pdf: X must not be complex");
  endif

  cdf = logistic_cdf (x);
  pdf = cdf .* (1 - cdf);

endfunction


%!shared x,y
%! x = [-Inf -log(4) 0 log(4) Inf];
%! y = [0, 0.16, 1/4, 0.16, 0];
%!assert (logistic_pdf ([x, NaN]), [y, NaN], eps)

## Test class of input preserved
%!assert (logistic_pdf (single ([x, NaN])), single ([y, NaN]), eps ("single"))

## Test input validation
%!error logistic_pdf ()
%!error logistic_pdf (1,2)
%!error logistic_pdf (i)

