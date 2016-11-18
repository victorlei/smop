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
## @deftypefn {Function File} {} arch_rnd (@var{a}, @var{b}, @var{t})
## Simulate an ARCH sequence of length @var{t} with AR coefficients @var{b} and
## CH coefficients @var{a}.
##
## The result @math{y(t)} follows the model
## @c Set example in small font to prevent overfull line
##
## @smallexample
## y(t) = b(1) + b(2) * y(t-1) + @dots{} + b(lb) * y(t-lb+1) + e(t),
## @end smallexample
##
## @noindent
## where @math{e(t)}, given @var{y} up to time @math{t-1}, is
## @math{N(0, h(t))}, with
## @c Set example in small font to prevent overfull line
##
## @smallexample
## h(t) = a(1) + a(2) * e(t-1)^2 + @dots{} + a(la) * e(t-la+1)^2
## @end smallexample
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Simulate an ARCH process

function y = arch_rnd (a, b, t)

  if (nargin != 3)
    print_usage ();
  endif

  if (! ((min (size (a)) == 1) && (min (size (b)) == 1)))
    error ("arch_rnd: A and B must both be scalars or vectors");
  endif
  if (! (isscalar (t) && (t > 0) && (rem (t, 1) == 0)))
    error ("arch_rnd: T must be a positive integer");
  endif

  if (! (a(1) > 0))
    error ("arch_rnd: A(1) must be positive");
  endif
  ## perhaps add a test for the roots of a(z) here ...

  la = length (a);
  a  = reshape (a, 1, la);
  if (la == 1)
    a  = [a, 0];
    la = la + 1;
  endif

  lb = length (b);
  b  = reshape (b, 1, lb);
  if (lb == 1)
    b  = [b, 0];
    lb = lb + 1;
  endif
  m = max ([la, lb]);

  e = zeros (t, 1);
  h = zeros (t, 1);
  y = zeros (t, 1);

  h(1) = a(1);
  e(1) = sqrt (h(1)) * randn;
  y(1) = b(1) + e(1);

  for t = 2:m
    ta   = min ([t, la]);
    h(t) = a(1) + a(2:ta) * e(t-ta+1:t-1).^2;
    e(t) = sqrt (h(t)) * randn;
    tb   = min ([t, lb]);
    y(t) = b(1) + b(2:tb) * y(t-tb+1:t-1) + e(t);
  endfor

  if (t > m)
    for t = m+1:t
      h(t) = a(1) + a(2:la) * e(t-la+1:t-1).^2;
      e(t) = sqrt (h(t)) * randn;
      y(t) = b(1) + b(2:lb) * y(t-tb+1:t-1) + e(t);
    endfor
  endif

  y = y(1:t);

endfunction

