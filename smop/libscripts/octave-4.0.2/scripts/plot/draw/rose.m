## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Function File} {} rose (@var{th})
## @deftypefnx {Function File} {} rose (@var{th}, @var{nbins})
## @deftypefnx {Function File} {} rose (@var{th}, @var{bins})
## @deftypefnx {Function File} {} rose (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} rose (@dots{})
## @deftypefnx {Function File} {[@var{thout} @var{rout}] =} rose (@dots{})
## Plot an angular histogram.
##
## With one vector argument, @var{th}, plot the histogram with 20 angular bins.
## If @var{th} is a matrix then each column of @var{th} produces a separate
## histogram.
##
## If @var{nbins} is given and is a scalar, then the histogram is produced with
## @var{nbin} bins.  If @var{bins} is a vector, then the center of each bin is
## defined by the values of @var{bins} and the number of bins is
## given by the number of elements in @var{bins}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to the
## line objects representing each histogram.
##
## If two output arguments are requested then no plot is made and
## the polar vectors necessary to plot the histogram are returned instead.
##
## @example
## @group
## [th, r] = rose ([2*randn(1e5,1), pi + 2*randn(1e5,1)]);
## polar (th, r);
## @end group
## @end example
##
## @seealso{hist, polar}
## @end deftypefn

function [thout, rout] = rose (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("rose", varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  ## Force theta to [0,2*pi] range
  th = varargin{1};
  th = atan2 (sin (th), cos (th)) + pi;

  if (nargin > 1)
    x = varargin{2};
    if (isscalar (x))
      x = [0.5/x : 1/x : 1] * 2*pi;
    else
      ## Force theta to [0,2*pi] range
      x = atan2 (sin (x), cos (x)) + pi;
    endif
  else
    x = [1/40 : 1/20 : 1] * 2*pi;
  endif

  [nn, xx] = hist (th, x);
  xx = xx(:).';
  if (isvector (nn))
    nn = nn(:);
  endif
  x1 = xx(1:end-1) + diff (xx, 1) / 2;
  x1 = [x1 ; x1; x1; x1](:);
  th = [0; 0; x1; 2*pi ; 2*pi];
  r = zeros (4 * rows (nn), columns (nn));
  r(2:4:end, :) = nn;
  r(3:4:end, :) = nn;

  if (nargout < 2)
    oldfig = [];
    if (! isempty (hax))
      oldfig = get (0, "currentfigure");
    endif
    unwind_protect
      hax = newplot (hax);
      htmp = polar (th, r);
    unwind_protect_cleanup
      if (! isempty (oldfig))
        set (0, "currentfigure", oldfig);
      endif
    end_unwind_protect

    if (nargout > 0)
      thout = htmp;
    endif
  else
    thout = th;
    rout = r;
  endif

endfunction


%!demo
%! clf;
%! rose (2*randn (1e5, 1), 8);
%! title ('rose() angular histogram plot with 8 bins');

%!demo
%! clf;
%! rose ([2*randn(1e5, 1), pi + 2*randn(1e5, 1)]);
%! title ('rose() angular histogram plot with 2 data series');

