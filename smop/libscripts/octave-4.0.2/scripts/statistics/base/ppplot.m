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
## @deftypefn {Function File} {[@var{p}, @var{y}] =} ppplot (@var{x}, @var{dist}, @var{params})
## Perform a PP-plot (probability plot).
##
## If F is the CDF of the distribution @var{dist} with parameters
## @var{params} and @var{x} a sample vector of length @var{n}, the PP-plot
## graphs ordinate @var{y}(@var{i}) = F (@var{i}-th largest element of
## @var{x}) versus abscissa @var{p}(@var{i}) = (@var{i} - 0.5)/@var{n}.  If
## the sample comes from F, the pairs will approximately follow a straight
## line.
##
## The default for @var{dist} is the standard normal distribution.
##
## The optional argument @var{params} contains a list of parameters of
## @var{dist}.
##
## For example, for a probability plot of the uniform distribution on [2,4]
## and @var{x}, use
##
## @example
## ppplot (x, "uniform", 2, 4)
## @end example
##
## @noindent
## @var{dist} can be any string for which a function @var{dist_cdf} that
## calculates the CDF of distribution @var{dist} exists.
##
## If no output is requested then the data are plotted immediately.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Perform a PP-plot (probability plot)

function [p, y] = ppplot (x, dist, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isvector (x))
    error ("ppplot: X must be a vector");
  endif

  s = sort (x);
  n = length (x);
  p = ((1 : n)' - 0.5) / n;
  if (nargin == 1)
    F = @stdnormal_cdf;
  else
    F = str2func (sprintf ("%scdf", dist));
  endif;
  if (nargin <= 2)
    y = feval (F, s);
  else
    y = feval (F, s, varargin{:});
  endif

  if (nargout == 0)
    plot (p, y);
    axis ([0, 1, 0, 1]);
  endif

endfunction


## Test input validation
%!error ppplot ()
%!error ppplot (ones (2,2))

