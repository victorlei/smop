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
## @deftypefn  {Function File} {} semilogy (@var{y})
## @deftypefnx {Function File} {} semilogy (@var{x}, @var{y})
## @deftypefnx {Function File} {} semilogy (@var{x}, @var{y}, @var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} semilogy (@var{x}, @var{y}, @var{fmt})
## @deftypefnx {Function File} {} semilogy (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} semilogy (@dots{})
## Produce a 2-D plot using a logarithmic scale for the y-axis.
##
## See the documentation of @code{plot} for a description of the
## arguments that @code{semilogy} will accept.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created plot.
## @seealso{plot, semilogx, loglog}
## @end deftypefn

## Author: jwe

function h = semilogy (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("semilogy", varargin{:});

  if (nargs < 1)
    print_usage ();
  endif

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

    htmp = __plt__ ("semilogy", hax, varargin{:});

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
%! semilogy (x, y);
%! title ({'semilogx() plot', 'Y-axis is logarithmic'});

%!demo
%! clf;
%! x = logspace (-5, 1, 10);
%! y = logspace (-5, 1, 10);
%!
%! subplot (2,1,1);
%!  semilogy (x, y);
%!  ylabel ('semilogy (x, y)');
%!
%! subplot (2,1,2);
%!  semilogy (x, -y);
%!  ylabel ('semilogy (x, -y)');

%!demo
%! clf;
%! x = logspace (-5, 1, 10);
%! y = logspace (-5, 1, 10);
%!
%! subplot (2,1,1);
%!  semilogy (x, y);
%!  set (gca, 'ydir', 'reverse', 'activepositionproperty', 'outerposition');
%!  ylabel ({'semilogy (x, y)', 'ydir = reversed'});
%!
%! subplot (2,1,2);
%!  semilogy (x, -y);
%!  set (gca, 'ydir', 'reverse', 'activepositionproperty', 'outerposition');
%!  ylabel ({'semilogy (x, -y)', 'ydir = reversed'});

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   a = logspace (-5, 1, 10);
%!   b = logspace (-5, 1, 10);
%!   semilogy (a, b);
%!   assert (get (gca, "yscale"), "log");
%!   assert (get (gca, "xscale"), "linear");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   a = logspace (-5, 1, 10);
%!   b =-logspace (-5, 1, 10);
%!   semilogy (a, b);
%!   axis tight;
%!   assert (all (get (gca, "ytick") < 0));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

