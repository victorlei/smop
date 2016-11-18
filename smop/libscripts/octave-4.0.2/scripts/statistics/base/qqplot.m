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
## @deftypefn  {Function File} {[@var{q}, @var{s}] =} qqplot (@var{x})
## @deftypefnx {Function File} {[@var{q}, @var{s}] =} qqplot (@var{x}, @var{y})
## @deftypefnx {Function File} {[@var{q}, @var{s}] =} qqplot (@var{x}, @var{dist})
## @deftypefnx {Function File} {[@var{q}, @var{s}] =} qqplot (@var{x}, @var{y}, @var{params})
## @deftypefnx {Function File} {} qqplot (@dots{})
## Perform a QQ-plot (quantile plot).
##
## If F is the CDF of the distribution @var{dist} with parameters
## @var{params} and G its inverse, and @var{x} a sample vector of length
## @var{n}, the QQ-plot graphs ordinate @var{s}(@var{i}) = @var{i}-th
## largest element of x versus abscissa @var{q}(@var{i}f) = G((@var{i} -
## 0.5)/@var{n}).
##
## If the sample comes from F, except for a transformation of location
## and scale, the pairs will approximately follow a straight line.
##
## If the second argument is a vector @var{y} the empirical CDF of @var{y}
## is used as @var{dist}.
##
## The default for @var{dist} is the standard normal distribution.  The
## optional argument @var{params} contains a list of parameters of
## @var{dist}.  For example, for a quantile plot of the uniform
## distribution on [2,4] and @var{x}, use
##
## @example
## qqplot (x, "unif", 2, 4)
## @end example
##
## @noindent
## @var{dist} can be any string for which a function @var{distinv} or
## @var{dist_inv} exists that calculates the inverse CDF of distribution
## @var{dist}.
##
## If no output arguments are given, the data are plotted directly.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Perform a QQ-plot (quantile plot)

function [qout, sout] = qqplot (x, dist, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) && isvector (x)))
    error ("qqplot: X must be a numeric vector");
  endif

  if (nargin == 1)
    f = @stdnormal_inv;
  else
    if (isnumeric (dist))
      f = @(y) empirical_inv (y, dist);
    elseif (ischar (dist) && (exist (invname = [dist "inv"])
                              || exist (invname = [dist "%s_inv"])))
      f = str2func (invname);
    else
      error ("qqplot: no inverse CDF found for distribution DIST");
    endif
  endif;

  s = sort (x);
  n = length (x);
  t = ((1 : n)' - .5) / n;
  if (nargin <= 2)
    q = f (t);
    q_label = func2str (f);
  else
    q = f (t, varargin{:});
    if (nargin == 3)
      q_label = sprintf ("%s with parameter %g", func2str (f), varargin{1});
    else
      q_label = sprintf ("%s with parameters %g", func2str (f), varargin{1});
      param_str = sprintf (", %g", varargin{2:end});
      q_label = [q_label param_str];
    endif
  endif

  if (nargout == 0)
    plot (q, s, "-x");
    q_label = strrep (q_label, '_inv', '\_inv');
    if (q_label(1) == '@')
      q_label = q_label(6:end);  # Strip "@(y) " from anon. function
    endif
    xlabel (q_label);
    ylabel ("sample points");
  else
    qout = q;
    sout = s;
  endif

endfunction

