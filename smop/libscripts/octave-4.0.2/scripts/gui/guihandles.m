## Copyright (C) 2012-2015 Michael Goffioul
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
## @deftypefn  {Function File} {@var{hdata} =} guihandles (@var{h})
## @deftypefnx {Function File} {@var{hdata} =} guihandles
## Return a structure of object handles for the figure associated with
## handle @var{h}.
##
## If no handle is specified the current figure returned by @code{gcf} is used.
##
## The fieldname for each entry of @var{hdata} is taken from the @qcode{"tag"}
## property of the graphic object.  If the tag is empty then the handle is not
## returned.  If there are multiple graphic objects with the same tag then
## the entry in @var{hdata} will be a vector of handles.  @code{guihandles}
## includes all possible handles, including those for
## which @qcode{"HandleVisibility"} is @qcode{"off"}.
## @seealso{guidata, findobj, findall, allchild}
## @end deftypefn

## Author: goffioul

function hdata = guihandles (h)

  if (nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    if (! ishandle (h))
      error ("guidata: H must be a valid object handle");
    endif
    h = ancestor (h, "figure");
    if (isempty (h))
      error ("guidata: no ancestor figure of H found");
    endif
  else
    h = gcf ();
  endif

  hdata = __make_guihandles_struct__ (h, []);

endfunction

function hdata = __make_guihandles_struct__ (h, hdata)

  tag = get (h, "tag");
  if (! isempty (tag))
    if (isfield (hdata, tag))
      hdata.(tag) = [hdata.(tag), h];
    else
      try
        hdata.(tag) = h;
      catch
      end_try_catch
    endif
  endif

  kids = allchild (h);
  for hkid = kids'
    hdata = __make_guihandles_struct__ (hkid, hdata);
  endfor

endfunction

