## Copyright (C) 2010, 2013 Martin Hepperle
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
## @deftypefn {Function File} {[@var{sel}, @var{ok}] =} listdlg (@var{key}, @var{value}, @dots{})
## Return user inputs from a list dialog box in a vector of selection indices
## @var{sel} and a flag @var{ok} indicating how the user closed the dialog
## box.
##
## The value of @var{ok} is 1 if the user closed the box with the OK button,
## otherwise it is 0 and @var{sel} is empty.
##
## The indices in @var{sel} are 1-based.
##
## The arguments are specified in form of @var{key}, @var{value} pairs.
## The @qcode{"ListString"} argument pair must be specified.
##
## Valid @var{key} and @var{value} pairs are:
##
## @table @asis
## @item @qcode{"ListString"}
## a cell array of strings comprising the content of the list.
##
## @item @qcode{"SelectionMode"}
## can be either @qcode{"Single"} or @qcode{"Multiple"} (default).
##
## @item @qcode{"ListSize"}
## a vector with two elements @var{width} and @var{height} defining the size
## of the list field in pixels.  Default is [160 300].
##
## @item @qcode{"InitialValue"}
## a vector containing 1-based indices of preselected elements.
## Default is 1 (first item).
##
## @item @qcode{"Name"}
## a string to be used as the dialog caption.  Default is "".
##
## @item @qcode{"PromptString"}
## a cell array of strings to be displayed above the list field.
## Default is @{@}.
##
## @item @qcode{"OKString"}
## a string used to label the OK button.  Default is @qcode{"OK"}.
##
## @item @qcode{"CancelString"}
## a string used to label the Cancel button.  Default is @qcode{"Cancel"}.
## @end table
##
## Example:
##
## @example
## @group
## [sel, ok] = listdlg ("ListString", @{"An item", "another", "yet another"@},
##                      "SelectionMode", "Multiple");
## if (ok == 1)
##   for i = 1:numel (sel)
##     disp (sel(i));
##   endfor
## endif
## @end group
## @end example
##
## @seealso{menu, errordlg, helpdlg, inputdlg, msgbox, questdlg, warndlg}
## @end deftypefn

function [sel, ok] = listdlg (varargin)

  if (nargin < 2)
    print_usage ();
  endif

  listcell = {""};
  selmode = "Multiple";
  listsize = [160, 300];
  initialvalue = 1;
  name = "";
  prompt = {};
  okstring = "OK";
  cancelstring = "Cancel";

  ## handle key, value pairs
  for i = 1:2:nargin-1
    if (strcmpi (varargin{i}, "ListString"))
      listcell = varargin{i+1};
    elseif (strcmpi (varargin{i}, "SelectionMode"))
      selmode = tolower (varargin{i+1});
    elseif (strcmpi (varargin{i}, "ListSize"))
      listsize = varargin{i+1};
    elseif (strcmpi (varargin{i}, "InitialValue"))
      initialvalue = varargin{i+1};
    elseif (strcmpi (varargin{i}, "Name"))
      name = varargin{i+1};
    elseif (strcmpi (varargin{i}, "PromptString"))
      prompt = varargin{i+1};
    elseif (strcmpi (varargin{i}, "OKString"))
      okstring = varargin{i+1};
    elseif (strcmpi (varargin{i}, "CancelString"))
      cancelstring = varargin{i+1};
    endif
  endfor

  ## make sure prompt strings are a cell array
  if (! iscell (prompt))
    prompt = {prompt};
  endif

  ## make sure listcell strings are a cell array
  if (! iscell (listcell))
    listcell = {listcell};
  elseif (iscellstr (listcell{1}))
    listcell = listcell{1};
  endif

  ## make sure valid selection mode
  if (! strcmpi (selmode, "multiple") && ! strcmpi (selmode, "single"))
    error ("listdlg: invalid SelectionMode");
  endif

  if (__octave_link_enabled__ ())
    [sel, ok] = __octave_link_list_dialog__ (listcell, selmode, listsize,
                                             initialvalue, name, prompt,
                                             okstring, cancelstring);
  elseif (__have_feature__ ("JAVA"))
    ## transform matrices to cell arrays of strings
    ## swap width and height to correct calling format for JDialogBox
    listsize = {num2str(listsize(2)), num2str(listsize(1))};
    initialvalue = arrayfun (@num2str, initialvalue, "UniformOutput", false);
    if (isempty (prompt))
      prompt = {""};
    endif

    ret = javaMethod ("listdlg", "org.octave.JDialogBox", listcell,
                      selmode, listsize, initialvalue, name, prompt,
                      okstring, cancelstring);

    if (numel (ret) > 0)
      sel = zeros (1, numel (ret));
      ## for loop needed to convert Java array ret into Octave double sel
      for i = 1:numel (ret)
        sel(i) = ret(i);
      endfor
      ok = 1;
    else
      sel = [];
      ok = 0;
    endif
  else
    error ("listdlg is not available in this version of Octave");
  endif

endfunction


%!demo
%! disp ("- test listdlg with selectionmode single. No caption, no prompt.");
%! itemlist = {"An item \\alpha", "another", "yet another"};
%! s = listdlg ("ListString", itemlist, "SelectionMode", "Single");
%! imax = numel (s);
%! for i=1:1:imax
%!   disp (["Selected: ", num2str(i), ": ", itemlist{s(i)}]);
%! end

%!demo
%! disp ("- test listdlg with selectionmode and preselection. Has caption and two lines prompt.");
%! itemlist = {"An item \\alpha", "another", "yet another"};
%! s = listdlg ("ListString", itemlist, ...
%!              "SelectionMode", "Multiple", ...
%!              "Name", "Selection Dialog", ...
%!              "InitialValue", [1,2,3,4],
%!              "PromptString", {"Select <b>an</b> item...", "...or <b>multiple</b> items"});
%! imax = numel (s);
%! for i=1:1:imax
%!   disp (["Selected: ", num2str(i), ": ", itemlist{s(i)}]);
%! end

%!demo
%! disp ("- test listdlg with listsize.");
%! itemlist = {"Neutron", "Electron", "Quark", "Proton", "Neutrino"};
%! s = listdlg ("ListString", itemlist,
%!              "Name", "Bits and Pieces",
%!              "ListSize", [200 75]);
%! imax = numel (s);
%! for i=1:1:imax
%!   disp (["Selected: ", num2str(i), ": ", itemlist{s(i)}]);
%! end
