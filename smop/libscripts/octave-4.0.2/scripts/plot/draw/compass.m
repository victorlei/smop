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
## @deftypefn  {Function File} {} compass (@var{u}, @var{v})
## @deftypefnx {Function File} {} compass (@var{z})
## @deftypefnx {Function File} {} compass (@dots{}, @var{style})
## @deftypefnx {Function File} {} compass (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} compass (@dots{})
##
## Plot the @code{(@var{u}, @var{v})} components of a vector field emanating
## from the origin of a polar plot.
##
## The arrow representing each vector has one end at the origin and the tip at
## [@var{u}(i), @var{v}(i)].  If a single complex argument @var{z} is given,
## then @code{@var{u} = real (@var{z})} and @code{@var{v} = imag (@var{z})}.
##
## The style to use for the plot can be defined with a line style @var{style}
## of the same format as the @code{plot} command.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to the
## line objects representing the drawn vectors.
##
## @example
## @group
## a = toeplitz ([1;randn(9,1)], [1,randn(1,9)]);
## compass (eig (a));
## @end group
## @end example
##
## @seealso{polar, feather, quiver, rose, plot}
## @end deftypefn

function h = compass (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("compass", varargin{:});

  if (nargin == 0 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 1 || (nargin == 2 && ! isnumeric (varargin{2})))
    z = varargin{1}(:).';
    u = real (z);
    v = imag (z);
    have_line_spec = (nargin == 2);
  elseif (nargin >= 2 && isnumeric (varargin{2}))
    u = varargin{1}(:).';
    v = varargin{2}(:).';
    have_line_spec = (nargin == 3);
  else
    print_usage ();
  endif

  arrowsize = 0.20;
  line_spec = "-b";

  if (have_line_spec)
    arg = varargin{end};
    if (ischar (arg) || iscellstr (arg))
      [~, valid] = __pltopt__ ("compass", arg, false);
      if (valid)
        line_spec = arg;
      else
        error ("compass: invalid linestyle STYLE");
      endif
    else
      error ("compass: invalid linestyle STYLE");
    endif
  endif

  ## Matlab draws compass plots with the arrow head as one continous line,
  ## and each arrow separately.  This is completely different from quiver
  ## and quite ugly.
  n = length (u);
  xend = u;
  xtmp = u .* (1 - arrowsize);
  yend = v;
  ytmp = v .* (1 - arrowsize);
  x = [zeros(1, n); xend; xtmp - v * arrowsize / 3; xend; ...
       xtmp + v * arrowsize / 3];
  y = [zeros(1, n); yend; ytmp + u * arrowsize / 3; yend; ...
       ytmp - u * arrowsize / 3];
  [r, p] = cart2pol (x, y);

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    hlist = polar (r, p, line_spec);
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = hlist;
  endif

endfunction


%!demo
%! clf;
%! randn_9x1_data = [-2.555884; 0.394974; -0.191871; -1.147024; 1.355425; -0.437335; -0.014370; -0.941312; 1.240300];
%! randn_1x9_data = [1.42934, -1.10821, -1.70404, 0.63357, -0.68337, -1.19771, -0.96502, -1.12810, 0.22457];
%! a = toeplitz ([1;randn_9x1_data], [1,randn_1x9_data]);
%! compass (eig (a));

## Test input validation
%!error compass ()
%!error compass (1,2,3,4)
%!error compass (1, "-r", 2)
%!error <invalid linestyle STYLE> compass (1, "abc")
%!error <invalid linestyle STYLE> compass (1, {1})

