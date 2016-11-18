## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 1996-2015 Kurt Hornik
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
## @deftypefn  {Function File} {} empirical_rnd (@var{data})
## @deftypefnx {Function File} {} empirical_rnd (@var{data}, @var{r})
## @deftypefnx {Function File} {} empirical_rnd (@var{data}, @var{r}, @var{c}, @dots{})
## @deftypefnx {Function File} {} empirical_rnd (@var{data}, [@var{sz}])
## Return a matrix of random samples from the empirical distribution obtained
## from the univariate sample @var{data}.
##
## When called with a single size argument, return a square matrix with
## the dimension specified.  When called with more than one scalar argument the
## first two arguments are taken as the number of rows and columns and any
## further arguments specify additional matrix dimensions.  The size may also
## be specified with a vector of dimensions @var{sz}.
##
## If no size arguments are given then the result matrix is a random ordering
## of the sample @var{data}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Bootstrap samples from the empirical distribution

function rnd = empirical_rnd (data, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isvector (data))
    error ("empirical_rnd: DATA must be a vector");
  endif

  rnd = discrete_rnd (data, ones (size (data)), varargin{:});

endfunction


%!assert (size (empirical_rnd (ones (3, 1))), [3, 1])
%!assert (size (empirical_rnd (1:2, [4 1])), [4, 1])
%!assert (size (empirical_rnd (1:2, 4, 1)), [4, 1])

## Test class of input preserved
%!assert (class (empirical_rnd (1:2, 1)), "double")
%!assert (class (empirical_rnd (single (1:2), 1)), "single")

## Test input validation
%!error empirical_rnd ()
%!error empirical_rnd (ones (2), 1)
%!error empirical_rnd (ones (2), 1, 1)

