## Copyright (C) 2000-2015 Teemu Ikonen
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
## @deftypefn  {Function File} {} semilogyerr (@var{y}, @var{ey})
## @deftypefnx {Function File} {} semilogyerr (@var{y}, @dots{}, @var{fmt})
## @deftypefnx {Function File} {} semilogyerr (@var{x}, @var{y}, @var{ey})
## @deftypefnx {Function File} {} semilogyerr (@var{x}, @var{y}, @var{err}, @var{fmt})
## @deftypefnx {Function File} {} semilogyerr (@var{x}, @var{y}, @var{lerr}, @var{uerr}, @var{fmt})
## @deftypefnx {Function File} {} semilogyerr (@var{x}, @var{y}, @var{ex}, @var{ey}, @var{fmt})
## @deftypefnx {Function File} {} semilogyerr (@var{x}, @var{y}, @var{lx}, @var{ux}, @var{ly}, @var{uy}, @var{fmt})
## @deftypefnx {Function File} {} semilogyerr (@var{x1}, @var{y1}, @dots{}, @var{fmt}, @var{xn}, @var{yn}, @dots{})
## @deftypefnx {Function File} {} semilogyerr (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} semilogyerr (@dots{})
## Produce 2-D plots using a logarithmic scale for the y-axis and errorbars
## at each data point.
##
## Many different combinations of arguments are possible.  The most common
## form is
##
## @example
## semilogyerr (@var{x}, @var{y}, @var{ey}, @var{fmt})
## @end example
##
## @noindent
## which produces a semi-logarithmic plot of @var{y} versus @var{x}
## with errors in the @var{y}-scale defined by @var{ey} and the plot
## format defined by @var{fmt}.  @xref{XREFerrorbar,,errorbar}, for available
## formats and additional information.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## @seealso{errorbar, semilogxerr, loglogerr}
## @end deftypefn

## Created: 20.2.2001
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function h = semilogyerr (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("semilogyerr", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    set (hax, "yscale", "log");
    if (! ishold (hax))
      set (hax, "yminortick", "on");
    endif

    htmp = __errcomm__ ("semilogyerr", hax, varargin{:});

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! x = 0.25:0.25:10;
%! y = wblpdf (x, 4, 2);
%! eyu = rand (size (y));
%! eyl = 1.0 - 1./(1+eyu);
%! semilogyerr (x, y, eyl.*y, eyu.*y, '~-d');
%! xlim ([0 10]);
%! title ({'semilogyerr(): semilogy() plot with errorbars', ...
%!         'Y-axis is logarithmic'});

