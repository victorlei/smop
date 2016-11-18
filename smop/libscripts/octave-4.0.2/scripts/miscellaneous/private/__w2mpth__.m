## Copyright (C) 2015 Philip Nienhuis
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
## @deftypefn {Function File} {@var{mingwpath} =} __w2mpth__ (@var{winpath})
## Convert a Windows-style relative or full path name to MinGW style.
##
## @strong{Caution:} __w2mpth__ does not check the validity of the path.
##
## Examples:
##
## @example
## @group
##   mpth = __w2mpth__ ('D:\full\path\to\file.dat')
##   @result{} '/D/full/path/to/file.dat'
## @end group
## @end example
##
## @example
## @group
##   mpth = __w2mpth__ ('relative\path\to\file.dat')
##   @result{} 'relative/path/to/file.dat'
## @end group
## @end example
##
## @end deftypefn

## Author: Philip Nienhuis <prnienhuis@users.sf.net>
## Created: 2015-01-16

function mingwpath = __w2mpth__ (winpath)

  ## Check for platform
  if (! ispc)
    error ("__w2mpth__ should only be called on Windows platforms\n");
  endif

  ## Replace backslash file separators by forward slashes
  mingwpath = strrep (winpath, '\', '/');
  ## Also treat drive letter but beware of relative filenames
  mingwpath = regexprep (mingwpath, '^([a-zA-Z]):', '/$1');

endfunction


## Use single quote strings for winpaths to cope with backslashes.
## These tests are commented out until a better place is found (bug #44581)
##%!test
##%! if (ispc)
##%!   assert (__w2mpth__ ('file.fil'), 'file.fil');
##%!   assert (__w2mpth__ ('\file.fil'), '/file.fil');
##%!   assert (__w2mpth__ ('G:\file.fil'), '/G/file.fil');
##%!   assert (__w2mpth__ ('r:\subdir\file.fil'), '/r/subdir/file.fil');
##%!   assert (__w2mpth__ ('relative\path\to\file.dat'),
##%!                       'relative/path/to/file.dat')
##%! endif

