## Copyright (C) 2007, 2013 Michael Goffioul
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
## @deftypefn  {Function File} {} javarmpath (@var{clspath})
## @deftypefnx {Function File} {} javarmpath (@var{clspath1}, @dots{})
## Remove @var{clspath} from the dynamic class path of the Java virtual
## machine.
##
## @var{clspath} may either be a directory where @file{.class} files are found,
## or a @file{.jar} file containing Java classes.  Multiple paths may be
## removed at once by specifying additional arguments.
## @seealso{javaaddpath, javaclasspath}
## @end deftypefn

function javarmpath (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  for i = 1:numel (varargin)
    clspath = varargin{i};
    if (! ischar (clspath))
      error ("javarmpath: CLSPATH must be a string");
    endif

    old_path = canonicalize_file_name (tilde_expand (clspath));
    if (exist (old_path, "dir"))
      if (old_path(end) != filesep ())
        old_path = [old_path, filesep()];
      endif
    endif

    success = javaMethod ("removeClassPath", "org.octave.ClassHelper",
                           old_path);

    if (! success)
      warning ("javarmpath: %s: not found in Java classpath", old_path);
    endif
  endfor

endfunction

