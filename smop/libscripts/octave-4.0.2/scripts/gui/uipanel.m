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
## @deftypefn  {Function File} {@var{hui} =} uipanel (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{hui} =} uipanel (@var{parent}, "@var{property}, @var{value}, @dots{})
##
## Create a uipanel object and return a handle to it.
##
## uipanels are used as containers to group other uicontrol objects.
##
## If @var{parent} is omitted then a uipanel for the current figure is
## created. If no figure is available, a new figure is created first. 
##
## If @var{parent} is given then a uipanel relative to @var{parent} is created. 
## 
## Any provided property value pairs will override the default values of the created 
## uipanel object.
##
## Examples:
##
## @example
## @group
## % create figure and panel on it
## f = figure;
## p = uipanel ("title", "Panel Title", "position", [.25 .25 .5 .5]);
##
## % add two buttons to the panel
## b1 = uicontrol ("parent", p, "string", "A Button", "position",[18 10 150 36]);
## b2 = uicontrol ("parent", p, "string", "Another Button", "position",[18 60 150 36]);
##
## @end group
## @end example
## @seealso{figure, uicontrol}
## @end deftypefn

## Author: goffioul

function hui = uipanel (varargin)

  [h, args] = __uiobject_split_args__ ("uipanel", varargin,
                                       {"figure", "uipanel", "uibuttongroup"});
  hui = __go_uipanel__ (h, args{:});

endfunction

