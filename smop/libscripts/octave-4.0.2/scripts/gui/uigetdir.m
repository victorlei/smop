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
## @deftypefn  {Function File} {@var{dirname} =} uigetdir ()
## @deftypefnx {Function File} {@var{dirname} =} uigetdir (@var{init_path})
## @deftypefnx {Function File} {@var{dirname} =} uigetdir (@var{init_path}, @var{dialog_name})
## Open a GUI dialog for selecting a directory.
##
## If @var{init_path} is not given the current working directory is used.
##
## @var{dialog_name} may be used to customize the dialog title.
## @seealso{uigetfile, uiputfile}
## @end deftypefn

## Author: Kai Habel

function dirname = uigetdir (init_path = pwd, dialog_name = "Select Directory to Open")

  if (nargin > 2)
    print_usage ();
  endif

  if (! ischar (init_path) || ! ischar (dialog_name))
    error ("uigetdir: INIT_PATH and DIALOG_NAME must be string arguments");
  endif

  if (! isdir (init_path))
    init_path = fileparts (init_path);
  endif

  if (__octave_link_enabled__ ())
    file_filter = cell (0, 2);
    default_file_name = "";
    dialog_position = [240, 120];
    dialog_mode = "dir";

    [filename, dirname, filterindex] ...
      = __octave_link_file_dialog__ (file_filter, dialog_name,
                                     default_file_name, dialog_position,
                                     dialog_mode, init_path);
  else
    funcname = __get_funcname__ (mfilename ());
    dirname = feval (funcname, init_path, dialog_name);
  endif

endfunction


%!demo
%! uigetdir (pwd, 'Select Directory');

## Remove from test statistics.  No real tests possible.
%!assert (1)

