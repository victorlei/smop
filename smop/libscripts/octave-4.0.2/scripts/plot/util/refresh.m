## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn  {Function File} {} refresh ()
## @deftypefnx {Function File} {} refresh (@var{h})
## Refresh a figure, forcing it to be redrawn.
##
## When called without an argument the current figure is redrawn.  Otherwise,
## the figure with graphic handle @var{h} is redrawn.
## @seealso{drawnow}
## @end deftypefn

function refresh (h)

  if (nargin > 1)
    print_usage ();
  elseif (nargin == 1)
    if (! isfigure (h))
      error ("refresh: H must be a valid figure handle");
    endif
  else
    h = gcf ();
  endif

  set (h, "__modified__", "on");
  drawnow ();

endfunction

