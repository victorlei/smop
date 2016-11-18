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
## @deftypefn {Function File} {} logit (@var{p})
## Compute the logit for each value of @var{p}
##
## The logit is defined as
## @tex
## $$
## {\rm logit}(p) = \log\Big({p \over 1-p}\Big)
## $$
## @end tex
## @ifnottex
##
## @example
## logit (@var{p}) = log (@var{p} / (1-@var{p}))
## @end example
##
## @end ifnottex
## @seealso{probit, logistic_cdf}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Logit transformation

function y = logit (p)

  if (nargin != 1)
    print_usage ();
  endif

  y = logistic_inv (p);

endfunction


%!test
%! p = [0.01:0.01:0.99];
%! assert (logit (p), log (p ./ (1-p)), 25*eps);

%!assert (logit ([-1, 0, 0.5, 1, 2]), [NaN, -Inf, 0, +Inf, NaN])

## Test input validation
%!error logit ()
%!error logit (1, 2)

