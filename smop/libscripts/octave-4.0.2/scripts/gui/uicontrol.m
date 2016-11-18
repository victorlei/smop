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
## @deftypefn  {Function File} {@var{hui} =} uicontrol (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{hui} =} uicontrol (@var{parent}, @var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} uicontrol (@var{h})
##
## Create a uicontrol object and return a handle to it.
##
## uicontrols are used to create simple interactive controls such as push buttons, checkboxes, edit and list controls.
##
## If @var{parent} is omitted then a uicontrol for the current figure is
## created. If no figure is available, a new figure is created first. 
##
## If @var{parent} is given then a uicontrol relative to @var{parent} is created. 
## 
## Any provided property value pairs will override the default values of the created 
## uicontrol object. 
##
## Control of the type of uicontrol created is through the use of the @var{style} property.
## If no style property is provided, a push button will be created.
##
## Valid styles for uicontrol are:
##
## @table @asis
## @item @qcode{"checkbox"}
## Create a checkbox control that allows user on/off selection.
##
## @item @qcode{"edit"}
## Create a edit control that allows user input of single or multiple lines of text.
##
## @item @qcode{"listbox"}
## Create a listbox control that displays a lit of items and allows user slelection of 
## single or multiple items.
##
## @item @qcode{"popupmenu"}
## Create a popupmenu control that displays a list of options that can be selected 
## when the user clicks on the control.
##
## @item @qcode{"pushbutton"}
## Create a push button control that allows user to press to cause an action.
##
## @item @qcode{"radiobutton"}
## Create a radio button control intended to be used for mutually exclusive input in a group of
## of radiobutton controls.
##
## @item @qcode{"slider"}
## Create a slider control that allows user selection from a range of values by sliding 
## knob on the control.
##
## @item @qcode{"text"}
## Create a static text control to display single or multiple lines of text.
##
## @item @qcode{"togglebutton"}
## Create a toggle button control that appears like a push button but allows the user to 
## select between two states.
##
## @end table
##
## Examples:
##
## @example
## @group
## % create figure and panel on it
## f = figure;
## % create a button (default style)
## b1 = uicontrol (f, "string", "A Button", "position",[10 10 150 40]);
## % create an edit control
## e1 = uicontrol (f, "style", "edit", "string", "editable text", "position",[10 60 300 40]);
## % create a checkbox
## c1 = uicontrol (f, "style", "checkbox", "string", "a checkbox", "position",[10 120 150 40]);
## @end group
## @end example
## @seealso{figure, uipanel}
## @end deftypefn

## Author: goffioul

function hui = uicontrol (varargin)

  if (nargin == 1 && ishandle (varargin{1})
      && strcmpi (get (varargin{1}, "type"), "uicontrol"))
    error ("uicontrol focusing not implemented yet.");
  endif

  [h, args] = __uiobject_split_args__ ("uicontrol", varargin,
                                       {"figure", "uipanel", "uibuttongroup"});
  hui = __go_uicontrol__ (h, args{:});

endfunction

