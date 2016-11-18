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
## @deftypefn  {Function File} {@var{data} =} guidata (@var{h})
## @deftypefnx {Function File} {} guidata (@var{h}, @var{data})
## Query or set user-custom GUI data.
##
## The GUI data is stored in the figure handle @var{h}.  If @var{h} is not a
## figure handle then it's parent figure will be used for storage.
##
## @var{data} must be a single object which means it is usually preferable
## for it to be a data container such as a cell array or struct so that
## additional data items can be added easily.
##
## @seealso{getappdata, setappdata, get, set, getpref, setpref}
## @end deftypefn

## Author: goffioul

function dataout = guidata (h, data)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! ishandle (h))
    error ("guidata: H must be a valid object handle");
  endif
  h = ancestor (h, "figure");
  if (isempty (h))
    error ("guidata: no ancestor figure of H found");
  endif

  if (nargin == 1)
    dataout = get (h, "__guidata__");
  else
    set (h, "__guidata__", data);
    if (nargout == 1)
      dataout = data;
    endif
  endif

endfunction


## Test input validation
%!error guidata ()
%!error guidata (1,2,3)
%!error <H must be a valid object handle> guidata ({1})
%!error <no ancestor figure of H found> guidata (0)

