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
## @deftypefn {Function File} {@var{h} =} message_dialog (@var{caller}, @var{msg}, @var{title}, @var{icon}, @var{createmode})
## Undocumented internal function.
## @end deftypefn

function retval = message_dialog (caller, msg, title = "", icon, createmode)

  if (! ischar (msg))
    if (iscell (msg))
      msg = sprintf ("%s\n", msg{:});
      msg(end) = "";
    else
      error ("%s: MSG must be a character string or cellstr array", caller);
    endif
  endif

  if (! ischar (title))
    error ("%s: TITLE must be a character string", caller);
  endif

  dlg = "emptydlg";
  if (nargin >= 4)
    switch (icon)
      case "error"
        dlg = "errordlg";
      case "help"
        dlg = "helpdlg";
      case "warn"
        dlg = "warndlg";
      case "none"
        dlg = "emptydlg";
      case "custom"
        icon = "emptydlg";
        warning ("%s: custom icons not yet supported", caller);
      otherwise
        error ("%s: ICON is not a valid type", caller);
    endswitch
  else
    icon = "none";
  endif

  if (nargin == 5)
    if ((isstruct (createmode)) && (isfield (createmode, "WindowStyle")))
      createmode = createmode.WindowStyle;
    endif
    switch (createmode)
      case {"nonmodal", "non-modal", "modal", "replace"}
        warning ("%s: %s is not yet supported", caller, createmode);
      otherwise
        error ("%s: CREATEMODE is not a valid type", caller);
    endswitch
  endif

  if (__octave_link_enabled__ ())
    retval = __octave_link_message_dialog__ (icon, msg, title);
  elseif (__have_feature__ ("JAVA"))
    retval = javaMethod (dlg, "org.octave.JDialogBox", msg, title);
  else
    error ("%s is not available in this version of Octave", dlg);
  endif

endfunction

