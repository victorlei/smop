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
## @deftypefn {Function File} {} wienrnd (@var{t}, @var{d}, @var{n})
## Return a simulated realization of the @var{d}-dimensional Wiener Process
## on the interval [0, @var{t}].
##
## If @var{d} is omitted, @var{d} = 1 is used.  The first column of the
## return matrix contains time, the remaining columns contain the Wiener
## process.
##
## The optional parameter @var{n} defines the number of summands used for
## simulating the process over an interval of length 1.  If @var{n} is
## omitted, @var{n} = 1000 is used.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Simulate a Wiener process

function retval = wienrnd (t, d, n)

  if (nargin == 1)
    d = 1;
    n = 1000;
  elseif (nargin == 2)
    n = 1000;
  elseif (nargin > 3)
    print_usage ();
  endif

  if (! isscalar (t) || ! isscalar (d) || ! isscalar (n))
    error ("wienrnd: T, D and N must all be positive integers");
  endif

  retval = randn (n * t, d);
  retval = cumsum (retval) / sqrt (n);

  retval = [((1: n*t)' / n), retval];

endfunction

