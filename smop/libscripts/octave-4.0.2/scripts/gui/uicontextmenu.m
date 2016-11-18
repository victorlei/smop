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
## @deftypefn {Function File} {@var{hui} =} uicontextmenu (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{hui} =} uicontextmenu (@var{h}, @var{property}, @var{value}, @dots{})
##
## Create a uicontextmenu object and return a handle to it.
##
## If @var{h} is omitted then a uicontextmenu for the current figure is
## created. If no figure is available, a new figure is created first. 
##
## If @var{h} is given then a uicontextmenu relative to @var{h} is created. 
## 
## Any provided property value pairs will override the default values of the created 
## uicontextmenu object.
##
## Examples:
##
## @example
## @group
## % create figure and uicontextmenu
## f = figure;
## c = uicontextmenu (f);
##
## % create menus in the context menu
## m1 = uimenu ("parent",c,"label","Menu item 1","callback","disp('menu item 1')");
## m2 = uimenu ("parent",c,"label","Menu item 2","callback","disp('menu item 2')");
##
## % set the context menu for the figure
## set (f, "uicontextmenu", c);
## @end group
## @end example
## @seealso{figure, uimenu}
## @end deftypefn

## Author: goffioul

function hui = uicontextmenu (varargin)

  [h, args] = __uiobject_split_args__ ("uicontextmenu", varargin, {"figure"});
  hui = __go_uicontextmenu__ (h, args{:});

endfunction

