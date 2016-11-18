## Copyright (C) 2010-2015 Kai Habel
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
## @deftypefn  {Function File} {@var{hui} =} uimenu (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{hui} =} uimenu (@var{h}, @var{property}, @var{value}, @dots{})
## Create a uimenu object and return a handle to it.
##
## If @var{h} is omitted then a top-level menu for the current figure is
## created.  If @var{h} is given then a submenu relative to @var{h} is created.
##
## uimenu objects have the following specific properties:
##
## @table @asis
## @item @qcode{"accelerator"}
## A string containing the key combination together with CTRL to execute this
## menu entry (e.g., @qcode{"x"} for CTRL+x).
##
## @item @qcode{"callback"}
## Is the function called when this menu entry is executed.  It can be either a
## function string (e.g., @qcode{"myfun"}), a function handle (e.g., @@myfun)
## or a cell array containing the function handle and arguments for the
## callback function (e.g., @{@@myfun, arg1, arg2@}).
##
## @item @qcode{"checked"}
## Can be set @qcode{"on"} or @qcode{"off"}.  Sets a mark at this menu entry.
##
## @item @qcode{"enable"}
## Can be set @qcode{"on"} or @qcode{"off"}.  If disabled the menu entry
## cannot be selected and it is grayed out.
##
## @item @qcode{"foregroundcolor"}
## A color value setting the text color for this menu entry.
##
## @item @qcode{"label"}
## A string containing the label for this menu entry.  A @qcode{"&"}-symbol
## can be used to mark the @qcode{"accelerator"} character (e.g.,
## @nospell{@qcode{"E&xit"}})
##
## @item @qcode{"position"}
## An scalar value containing the relative menu position.  The entry with the
## lowest value is at the first position starting from left or top.
##
## @item @qcode{"separator"}
## Can be set @qcode{"on"} or @qcode{"off"}.  If enabled it draws a separator
## line above the current position.  It is ignored for top level entries.
##
## @end table
##
## Examples:
##
## @example
## @group
## f = uimenu ("label", "&File", "accelerator", "f");
## e = uimenu ("label", "&Edit", "accelerator", "e");
## uimenu (f, "label", "Close", "accelerator", "q", ...
##            "callback", "close (gcf)");
## uimenu (e, "label", "Toggle &Grid", "accelerator", "g", ...
##            "callback", "grid (gca)");
## @end group
## @end example
## @seealso{figure}
## @end deftypefn

## Author: Kai Habel

function hui = uimenu (varargin)

  [h, args] = __uiobject_split_args__ ("uimenu", varargin,
                                       {"figure", "uicontextmenu", "uimenu"});

  tmp = __go_uimenu__ (h, args{:});

  if (nargout > 0)
    hui = tmp;
  endif

endfunction


%!demo
%! clf;
%! surfl (peaks);
%! colormap (copper (64));
%! shading ('interp');
%! f = uimenu ('label', '&File', 'accelerator', 'f');
%! e = uimenu ('label', '&Edit', 'accelerator', 'e');
%! uimenu (f, 'label', 'Close', 'accelerator', 'q', 'callback', 'close (gcf)');
%! uimenu (e, 'label', 'Toggle &Grid', 'accelerator', 'g', 'callback', 'grid (gca)');

%!testif HAVE_FLTK
%! toolkit = graphics_toolkit ("fltk");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ui = uimenu ("label", "mylabel");
%!   assert (findobj (hf, "type", "uimenu"), ui);
%!   assert (get (ui, "label"), "mylabel");
%!   assert (get (ui, "checked"), "off");
%!   assert (get (ui, "separator"), "off");
%!   assert (get (ui, "enable"), "on");
%!   assert (get (ui, "position"), 0);
%! unwind_protect_cleanup
%!   close (hf);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

## check for top level menus file and edit
%!testif HAVE_FLTK
%! toolkit = graphics_toolkit ("fltk");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   uif = findall (hf, "label", "&file");
%!   assert (ishghandle (uif));
%!   uie = findall (hf, "label", "&edit");
%!   assert (ishghandle (uie));
%! unwind_protect_cleanup
%!   close (hf);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

%!testif HAVE_FLTK
%! toolkit = graphics_toolkit ("fltk");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   uie = findall (hf, "label", "&edit");
%!   myui = uimenu (uie, "label", "mylabel");
%!   assert (ancestor (myui, "uimenu", "toplevel"), uie);
%! unwind_protect_cleanup
%!   close (hf);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

