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
## @deftypefn {Function File} {@var{newargs} =} __add_datasource__ (@var{fcn}, @var{h}, @var{data}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

function newargs = __add_datasource__ (fcn, h, data, varargin)

  if (ischar (data))
    data = {data};
  endif

  for i = 1 : numel (data)
    addproperty (strcat (data{i}, "datasource"), h, "string", "");
  endfor

  i = 0;
  newargs = {};
  while (i < numel (varargin))
    arg = varargin{++i};
    if (i != numel (varargin) && ischar (arg)
        && length (arg) > 9 && strcmpi (arg(end-9:end), "datasource"))
      arg = tolower (arg);
      val = varargin{++i};
      if (ischar (val))
        set (h, arg, val);
      else
        error ("%s: expecting data source to be a string", fcn);
      endif
    else
      newargs{end + 1} = arg;
    endif
  endwhile
endfunction

