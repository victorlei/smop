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
## @deftypefn  {Built-in Function} {@var{fname} =} octave_tmp_file_name ()
## @deftypefnx {Built-in Function} {@var{fname} =} octave_tmp_file_name (@var{dir})
## @deftypefnx {Built-in Function} {@var{fname} =} octave_tmp_file_name (@var{dir}, @var{prefix})
##
## @code{octave_tmp_file_name} is deprecated and will be removed in Octave
## version 4.4.  Use @code{tempname} for equivalent functionality.
##
## Return a unique temporary file name as a string.
##
## If @var{prefix} is omitted, a value of @qcode{"oct-"} is used.
## If @var{dir} is also omitted, the default directory for temporary files
## (@code{P_tmpdir} is used.  If @var{dir} is provided, it must exist,
## otherwise the default directory for temporary files is used.
## @seealso{tempname, tmpnam, mkstemp, tempdir, P_tmpdir, tmpfile}
## @end deftypefn

## Deprecated in version 4.0

function filename = octave_tmp_file_name (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "octave_tmp_file_name is obsolete and will be removed from a future version of Octave, please use tempname instead");
  endif

  filename = tmpnam (varargin{:});

endfunction


%!assert (1)

