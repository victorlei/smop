## Copyright (C) 2014-2015 John W. Eaton
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
## @deftypefn  {Function File} {@var{output} =} open @var{file}
## @deftypefnx {Function File} {@var{output} =} open (@var{file})
## Open the file @var{file} in Octave or in an external application based on
## the file type as determined by the file name extension.
##
## Recognized file types are
##
## @table @code
## @item .m
## Open file in the editor.
##
## @item .mat
## Load the file in the base workspace.
##
## @item .exe
## Execute the program (on Windows systems only).
## @end table
##
## Other file types are opened in the appropriate external application.
## @end deftypefn

function output = open (file)

  if (nargin != 1)
    print_usage ();
  endif

  if (! ischar (file))
    error ("expecting argument to be a file name");
  endif

  [~, ~, ext] = fileparts (file);

  if (strcmpi (ext, ".m"))
    edit (file);
  elseif (strcmpi (ext, ".mat"))
    if (nargout > 0)
      output = load (file);
    else
      evalin ("base", sprintf ("load ('%s');", file));
    endif
  elseif (any (strcmpi (ext, {".fig", ".mdl", ".slx", ".prj"})))
    error ("opening file type '%s' is not supported", ext);
  elseif (strcmpi (ext, ".exe"))
    if (ispc ())
      dos (file);
    else
      error ("executing .exe files is only supported on Windows systems");
    endif
  else
    __open_with_system_app__ (file);
  endif

endfunction

## Test input validation
%!error open
%!error open (1)
%!error output = open (1)
