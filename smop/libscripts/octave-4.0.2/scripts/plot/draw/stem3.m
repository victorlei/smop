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
## @deftypefn  {Function File} {} stem3 (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} stem3 (@dots{}, @var{linespec})
## @deftypefnx {Function File} {} stem3 (@dots{}, "filled")
## @deftypefnx {Function File} {} stem3 (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} stem3 (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} stem3 (@dots{})
## Plot a 3-D stem graph.
##
## Stems are drawn from the height @var{z} to the location in the x-y plane
## determined by @var{x} and @var{y}.  The default color is @qcode{"b"} (blue),
## the default line style is @qcode{"-"}, and the default marker is @qcode{"o"}.
##
## The line style can be altered by the @code{linespec} argument in the same
## manner as the @code{plot} command.  If the @qcode{"filled"} argument is
## present the markers at the top of the stems will be filled in.
##
## Optional property/value pairs may be specified to control the appearance
## of the plot.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a handle to the @nospell{"stem series"}
## hggroup containing the line and marker objects used for the plot.
## @xref{XREFstem,,stem}, for a description of the @nospell{"stem series"}
## object.
##
## Example:
##
## @example
## @group
## theta = 0:0.2:6;
## stem3 (cos (theta), sin (theta), theta);
## @end group
## @end example
##
## @noindent
## plots 31 stems with heights from 0 to 6 lying on a circle.
##
## Implementation Note: Color definitions with RGB-triples are not valid.
## @seealso{stem, bar, hist, plot}
## @end deftypefn

function h = stem3 (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  htmp = __stem__ (true, varargin{:});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! theta = 0:0.2:6;
%! stem3 (cos (theta), sin (theta), theta);
%! title ('stem3() plot');

%!error stem3 ()
%!error <must define X, Y, and Z> stem3 (1,2)
%!error <X, Y, and Z must be numeric> stem3 ({1}, 1, 1)
%!error <X, Y, and Z must be numeric> stem3 (1, {1}, 1)
%!error <X, Y, and Z must be numeric> stem3 (1, 1, {1})
%!error <inconsistent sizes for X, Y, and Z> stem3 (ones (2,2), 1, 1);
%!error <inconsistent sizes for X, Y, and Z> stem3 (1, ones (2,2), 1);
%!error <inconsistent sizes for X, Y, and Z> stem3 (1, 1, ones (2,2));
%!error <No value specified for property "FOO"> stem3 (1, "FOO")
