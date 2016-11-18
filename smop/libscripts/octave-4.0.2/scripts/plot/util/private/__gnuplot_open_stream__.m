## Copyright (C) 2009-2015 Ben Abbott
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
## @deftypefn {Function File} {@var{stream} =} __gnuplot_open_stream__ (@var{npipes}, @var{h})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2009-04-11

function plot_stream = __gnuplot_open_stream__ (npipes, h)
  [prog, args] = gnuplot_binary ();
  if (npipes > 1)
    [plot_stream(1), plot_stream(2), pid] = popen2 (prog, args{:});
    if (pid < 0)
      error ("__gnuplot_open_stream__: failed to open connection to gnuplot");
    else
      plot_stream(3) = pid;
    endif
  else
    plot_stream = popen (sprintf ("%s ", prog, args{:}), "w");
    if (plot_stream < 0)
      error ("__gnuplot_open_stream__: failed to open connection to gnuplot");
    endif
  endif
  if (nargin > 1)
    set (h, "__plot_stream__", plot_stream);
  endif
endfunction

