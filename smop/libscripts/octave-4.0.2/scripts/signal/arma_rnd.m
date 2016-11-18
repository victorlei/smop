## Copyright (C) 1995-2015 Friedrich Leisch
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
## @deftypefn {Function File} {} arma_rnd (@var{a}, @var{b}, @var{v}, @var{t}, @var{n})
## Return a simulation of the ARMA model.
##
## The ARMA model is defined by
##
## @example
## @group
## x(n) = a(1) * x(n-1) + @dots{} + a(k) * x(n-k)
##      + e(n) + b(1) * e(n-1) + @dots{} + b(l) * e(n-l)
## @end group
## @end example
##
## @noindent
## in which @var{k} is the length of vector @var{a}, @var{l} is the length of
## vector @var{b} and @var{e} is Gaussian white noise with variance @var{v}.
## The function returns a vector of length @var{t}.
##
## The optional parameter @var{n} gives the number of dummy @var{x}(@var{i})
## used for initialization, i.e., a sequence of length @var{t}+@var{n} is
## generated and @var{x}(@var{n}+1:@var{t}+@var{n}) is returned.  If @var{n}
## is omitted, @var{n} = 100 is used.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Simulate an ARMA process

function x = arma_rnd (a, b, v, t, n)

  if (nargin == 4)
    n = 100;
  elseif (nargin == 5)
    if (! isscalar (n))
      error ("arma_rnd: N must be a scalar");
    endif
  else
    print_usage ();
  endif

  if ((min (size (a)) > 1) || (min (size (b)) > 1))
    error ("arma_rnd: A and B must not be matrices");
  endif

  if (! isscalar (t))
    error ("arma_rnd: T must be a scalar");
  endif

  ar = length (a);
  br = length (b);

  a = reshape (a, ar, 1);
  b = reshape (b, br, 1);

  ## Apply our notational convention.
  a = [1; -a];
  b = [1; b];

  n = min (n, ar + br);

  e = sqrt (v) * randn (t + n, 1);

  x = filter (b, a, e);
  x = x(n + 1 : t + n);

endfunction

