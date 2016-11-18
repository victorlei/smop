## Copyright (C) 2013-2015 Vytautas Jančauskas
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
## @deftypefn  {Function File} {@var{value} =} get (@var{recorder}, @var{name})
## @deftypefnx {Function File} {@var{values} =} get (@var{recorder})
## Return the @var{value} of the property identified by @var{name}.
##
## If @var{name} is a cell array, return the values of the properties
## corresponding to the elements of the cell array.  Given only the recorder
## object, return a scalar structure with values of all properties of
## @var{recorder}.  The field names of the structure correspond to property
## names.
## @end deftypefn

function retval = get (varargin)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  properties = __get_properties__ (varargin{1});

  if (nargin == 1)
    retval = properties;
  elseif (nargin == 2)
    pnames = varargin{2};
    if (ischar (pnames))
      retval = getfield (properties, pnames);
    elseif (iscellstr (pnames))
      retval = cell (size (pnames));
      for i = 1:numel (pnames)
        retval{i} = getfield (properties, pnames{i});
      endfor
    else
      error ("@audiorecorder/get: invalid name argument");
    endif
  endif

endfunction
