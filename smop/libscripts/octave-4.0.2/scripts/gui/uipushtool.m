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
## @deftypefn  {Function File} {@var{hui} =} uipushtool (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{hui} =} uipushtool (@var{parent}, @var{property}, @var{value}, @dots{})
##
## Create a uipushtool object and return a handle to it.
##
## uipushtools are buttons that appear on a figure toolbar. The button is created with a border that
## is shown when the user hovers over the button. An image can be set using the cdata property.
##
## If @var{parent} is omitted then a uipushtool for the current figure is
## created. If no figure is available, a new figure is created first.  If a figure is
## available, but does not contain a uitoolbar, a uitoolbar will be created.
##
## If @var{parent} is given then a uipushtools is created on the @var{parent} uitoolbar. 
## 
## Any provided property value pairs will override the default values of the created 
## uipushtool object.
##
## Examples:
##
## @example
## @group
## % create figure without a default toolbar
## f = figure ("toolbar", "none");
## % create empty toolbar
## t = uitoolbar (f);
## % create a 19x19x3 black square
## img=zeros(19,19,3);
## % add pushtool button to toolbar
## b = uipushtool (t, "cdata", img);
## @end group
## @end example
## @seealso{figure, uitoolbar, uitoggletool}
## @end deftypefn

## Author: goffioul

function hui = uipushtool (varargin)

  [h, args] = __uiobject_split_args__ ("uipushtool", varargin,
                                       {"uitoolbar"}, 0);
  if (isempty (h))
    h = findobj (gcf, "-depth", 1, "type", "uitoolbar");
    if (isempty (h))
      h = uitoolbar ();
    else
      h = h(1);
    endif
  endif
  hui = __go_uipushtool__ (h, args{:});

endfunction

