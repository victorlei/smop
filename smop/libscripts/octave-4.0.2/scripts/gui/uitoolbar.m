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
## @deftypefn  {Function File} {@var{hui} =} uitoolbar (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{hui} =} uitoolbar (@var{parent}, @var{property}, @var{value}, @dots{})
##
## Create a uitoolbar object and return a handle to it. A uitoolbar displays uitoggletool and uipushtool buttons.
##
## If @var{parent} is omitted then a uitoolbar for the current figure is
## created. If no figure is available, a new figure is created first. 
##
## If @var{parent} is given then a uitoolbar relative to @var{parent} is created. 
## 
## Any provided property value pairs will override the default values of the created 
## uitoolbar object. 
##
## Examples:
##
## @example
## @group
## % create figure without a default toolbar 
## f = figure ("toolbar", "none");
## % create empty toolbar
## t = uitoolbar (f);
## @end group
## @end example
## @seealso{figure, uitoggletool, uipushtool}
## @end deftypefn

## Author: goffioul

function hui = uitoolbar (varargin)

  [h, args] = __uiobject_split_args__ ("uitoolbar", varargin, {"figure"});
  hui = __go_uitoolbar__ (h, args{:});

endfunction

