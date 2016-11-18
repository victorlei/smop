## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn  {Function File} {} semilogx (@var{y})
## @deftypefnx {Function File} {} semilogx (@var{x}, @var{y})
## @deftypefnx {Function File} {} semilogx (@var{x}, @var{y}, @var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} semilogx (@var{x}, @var{y}, @var{fmt})
## @deftypefnx {Function File} {} semilogx (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} semilogx (@dots{})
## Produce a 2-D plot using a logarithmic scale for the x-axis.
##
## See the documentation of @code{plot} for a description of the
## arguments that @code{semilogx} will accept.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created plot.
## @seealso{plot, semilogy, loglog}
## @end deftypefn

## Author: jwe

function h = semilogx (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("semilogx", varargin{:});

  if (nargs < 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    set (hax, "xscale", "log");
    if (! ishold (hax))
      set (hax, "xminortick", "on");
    endif

    htmp = __plt__ ("semilogx", hax, varargin{:});

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
%! x = 1:0.01:10;
%! y = (x .* (1 + rand (size (x)))) .^ 2;
%! semilogx (y, x);
%! title ({'semilogx() plot', 'X-axis is logarithmic'});

%!demo
%! clf;
%! x = logspace (-5, 1, 10);
%! y = logspace (-5, 1, 10);
%!
%! subplot (1,2,1);
%!  semilogx (x, y);
%!  xlabel ('semilogx (x, y)');
%!
%! subplot (1,2,2);
%!  semilogx (-x, y);
%!  xlabel ('semilogx (-x, y)');

%!demo
%! clf;
%! x = logspace (-5, 1, 10);
%! y = logspace (-5, 1, 10);
%!
%! subplot (1,2,1);
%!  semilogx (x, y);
%!  set (gca, 'xdir', 'reverse', 'activepositionproperty', 'outerposition');
%!  xlabel ({'semilogx (x, y)', 'xdir = reversed'});
%!
%! subplot (1,2,2);
%!  semilogx (-x, y);
%!  set (gca, 'xdir', 'reverse', 'activepositionproperty', 'outerposition');
%!  xlabel ({'semilogx (-x, y)', 'xdir = reversed'});

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   a = logspace (-5, 1, 10);
%!   b = logspace (-5, 1, 10);
%!   semilogx (a, b);
%!   assert (get (gca, "xscale"), "log");
%!   assert (get (gca, "yscale"), "linear");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   a =-logspace (-5, 1, 10);
%!   b = logspace (-5, 1, 10);
%!   semilogx (a, b);
%!   axis tight;
%!   assert (all (get (gca, "xtick") < 0));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

