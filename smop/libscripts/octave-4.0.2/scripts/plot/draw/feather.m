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
## @deftypefn  {Function File} {} feather (@var{u}, @var{v})
## @deftypefnx {Function File} {} feather (@var{z})
## @deftypefnx {Function File} {} feather (@dots{}, @var{style})
## @deftypefnx {Function File} {} feather (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} feather (@dots{})
##
## Plot the @code{(@var{u}, @var{v})} components of a vector field emanating
## from equidistant points on the x-axis.
##
## If a single complex argument @var{z} is given, then
## @code{@var{u} = real (@var{z})} and @code{@var{v} = imag (@var{z})}.
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
## phi = [0 : 15 : 360] * pi/180;
## feather (sin (phi), cos (phi));
## @end group
## @end example
##
## @seealso{plot, quiver, compass}
## @end deftypefn

function h = feather (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("feather", varargin{:});

  if (nargin == 0 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 1 || (nargin == 2 && ! isnumeric (varargin{2})))
    z = varargin{1}(:).';
    u = real (z);
    v = imag (z);
    have_line_spec = (nargin == 2);
  elseif (nargin >= 2 && isnumeric (varargin{2}))
    ioff = 3;
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
      [~, valid] = __pltopt__ ("feather", arg, false);
      if (valid)
        line_spec = arg;
      else
        error ("feather: invalid linestyle STYLE");
      endif
    else
      error ("feather: invalid linestyle STYLE");
    endif
  endif

  ## Matlab draws feather plots, with the arrow head as one continous
  ## line, and each arrow separately. This is completely different from
  ## quiver and quite ugly.
  n = length (u);
  xend = [1 : n] + u;
  xtmp = [1 : n] + u .* (1 - arrowsize);
  yend = v;
  ytmp = v .* (1 - arrowsize);
  x = [[1 : n]; xend; xtmp - v * arrowsize / 3; xend; ...
       xtmp + v * arrowsize / 3];
  y = [zeros(1, n); yend; ytmp + u * arrowsize / 3; yend; ...
       ytmp - u * arrowsize / 3];

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    hlist = plot (x, y, line_spec, [1, n], [0, 0], line_spec);
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
%! phi = [0 : 15 : 360] * pi/180;
%! feather (sin (phi), cos (phi));
%! axis tight;
%! title ('feather plot');

## Test input validation
%!error feather ()
%!error feather (1,2,3,4)
%!error feather (1, "-r", 2)
%!error <invalid linestyle STYLE> feather (1, "abc")
%!error <invalid linestyle STYLE> feather (1, {1})

