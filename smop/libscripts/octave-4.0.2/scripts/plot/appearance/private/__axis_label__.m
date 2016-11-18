## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn {Function File} {} __axis_label__ (@var{caller}, @var{h}, @var{txt}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function retval = __axis_label__ (hax, caller, txt, varargin)

  h = get (hax, caller);

  set (h, "fontangle", get (hax, "fontangle"),
          "fontname", get (hax, "fontname"),
          "fontunits", get (hax, "fontunits"),   # must precede fontsize
          "fontsize", get (hax, "fontsize"),
          "fontweight", get (hax, "fontweight"),
          "string", txt,
          varargin{:});

  if (nargout > 0)
    retval = h;
  endif

endfunction

