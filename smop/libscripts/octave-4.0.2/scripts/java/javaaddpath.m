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
## @deftypefn  {Function File} {} javaaddpath (@var{clspath})
## @deftypefnx {Function File} {} javaaddpath (@var{clspath1}, @dots{})
## Add @var{clspath} to the dynamic class path of the Java virtual machine.
##
## @var{clspath} may either be a directory where @file{.class} files are
## found, or a @file{.jar} file containing Java classes.  Multiple paths may
## be added at once by specifying additional arguments.
## @seealso{javarmpath, javaclasspath}
## @end deftypefn

function javaaddpath (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  for i = 1:numel (varargin)
    clspath = varargin{i};
    if (! ischar (clspath))
      error ("javaaddpath: CLSPATH must be a string");
    endif

    new_path = canonicalize_file_name (tilde_expand (clspath));
    if (exist (new_path, "dir"))
      if (new_path(end) != filesep ())
        new_path = [new_path, filesep()];
      endif
    elseif (! exist (new_path, "file"))
      error ("javaaddpath: CLSPATH does not exist: %s", clspath);
    endif

    success = javaMethod ("addClassPath", "org.octave.ClassHelper", new_path);

    if (! success)
      warning ("javaaddpath: failed to add '%s' to Java classpath", new_path);
    endif
  endfor

endfunction

