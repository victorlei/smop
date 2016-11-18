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
## @deftypefn {Function File} {[@var{h}, @var{varargin}, @var{narg}] =} __plt_get_axis_arg__ (@var{caller}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function [h, varargin, narg] = __plt_get_axis_arg__ (caller, varargin)

  h = [];
  parent = find (strcmpi (varargin, "parent"), 1);

  ## Look for a scalar which is a graphics handle but not the
  ## Root Figure (0) or an ordinary figure (integer).
  if (numel (varargin) > 0 && numel (varargin{1}) == 1
      && ishandle (varargin{1}) && varargin{1} != 0 && ! isfigure (varargin{1}))
    htmp = varargin{1};
    if (! isaxes (htmp))
      error ("%s: expecting first argument to be axes handle", caller);
    endif
    if (! strcmp (get (htmp, "tag"), "legend"))
      h = htmp;
      varargin(1) = [];
    endif
  ## Look for "parent"/axis prop/value pair
  elseif (numel (varargin) > 1 && ! isempty (parent))
    if (parent < numel (varargin) && ishandle (varargin{parent+1}))
      htmp = varargin{parent+1};
      if (isaxes (htmp) && ! strcmp (get (htmp, "tag"), "legend"))
        h = htmp;
        varargin(parent:parent+1) = [];
      else
        ## 'parent' property for some other type like hggroup
        h = [ancestor(htmp, "axes"), htmp];
      endif
    else
      error ("%s: expecting parent value to be axes handle", caller);
    endif
  endif

  narg = length (varargin);

endfunction


## No test needed for internal helper function.
%!assert (1)

