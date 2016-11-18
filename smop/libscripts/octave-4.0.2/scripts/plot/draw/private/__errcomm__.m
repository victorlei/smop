## Copyright (C) 2001-2015 Teemu Ikonen
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
## @deftypefn {Function File} {} __errcomm__ (@var{caller}, @var{hax}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Created: 20.02.2001
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function retval = __errcomm__ (caller, hax, varargin)

  if (nargin < 4)
    print_usage (caller);
  endif

  retval = [];
  data = cell (6,1);
  nargs = numel (varargin);
  k = 1;
  while (k <= nargs)
    arg = varargin{k++};
    if (! isnumeric (arg))
      error ("%s: data argument %d must be numeric", caller, k-1);
    endif
    if (isvector (arg))
      arg = arg(:);
    endif
    sz = size (arg);
    ndata = 1;
    data{ndata} = arg;
    while (k <= nargs)
      arg = varargin{k++};
      if (ischar (arg) || iscellstr (arg))
        retval = [retval; __errplot__(arg, hax, data{1:ndata})];
        break;
      endif
      if (! isnumeric (arg))
        error ("%s: data argument %d must be numeric", caller, k-1);
      endif
      if (isvector (arg))
        arg = arg(:);
      endif
      if (! isscalar (arg) && ((isvector (arg) && numel (arg) != prod (sz))
          || any (size (arg) != sz)))
        error ("%s: size of argument %d does not match others", caller, k-1);
      endif
      data{++ndata} = arg;
      if (ndata > 6)
        error ("%s: too many arguments to plot", caller);
      endif
    endwhile
  endwhile

  ## No format code found, use yerrorbar
  if (! (ischar (arg) || iscellstr (arg)))
    retval = [retval; __errplot__("~", hax, data{1:ndata})];
  endif

  drawnow ();

endfunction

