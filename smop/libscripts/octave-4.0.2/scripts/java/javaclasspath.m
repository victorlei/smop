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
## @deftypefn  {Function File} {} javaclasspath ()
## @deftypefnx {Function File} {@var{dpath} =} javaclasspath ()
## @deftypefnx {Function File} {[@var{dpath}, @var{spath}] =} javaclasspath ()
## @deftypefnx {Function File} {@var{clspath} =} javaclasspath (@var{what})
## Return the class path of the Java virtual machine in the form of a cell
## array of strings.
##
## If called with no inputs:
##
## @itemize
## @item If no output is requested, the dynamic and static classpaths are
## printed to the standard output.
##
## @item If one output value @var{dpath} is requested, the result is the
## dynamic classpath.
##
## @item If two output values@var{dpath} and @var{spath} are requested, the
## first variable will contain the dynamic classpath and the second will
## contain the static classpath.
## @end itemize
##
## If called with a single input parameter @var{what}:
##
## @table @asis
## @item @qcode{"-dynamic"}
## Return the dynamic classpath.
##
## @item @qcode{"-static"}
## Return the static classpath.
##
## @item @qcode{"-all"}
## Return both the static and dynamic classpath in a single cellstr.
## @end table
## @seealso{javaaddpath, javarmpath}
## @end deftypefn

function [path1, path2] = javaclasspath (which)

  if (nargin > 1)
    print_usage ();
  endif

  ## dynamic classpath
  dynamic_path = javaMethod ("getClassPath", "org.octave.ClassHelper");
  dynamic_path_list = ostrsplit (dynamic_path, pathsep ());

  ## static classpath
  static_path = javaMethod ("getProperty",
                            "java.lang.System", "java.class.path");
  static_path_list = ostrsplit (static_path, pathsep ());
  if (numel (static_path_list) > 1)
    ## remove first element (which is .../octave.jar)
    static_path_list(1) = [];
  else
    static_path_list = {};
  endif

  if (nargout == 0)
    if (! nargin)
      which = "-all";
    endif
    switch (tolower (which))
      case "-dynamic", disp_path_list ("DYNAMIC", dynamic_path_list);
      case "-static",  disp_path_list ("STATIC", static_path_list);
      case "-all"
        disp_path_list ("STATIC", static_path_list);
        disp ("");
        disp_path_list ("DYNAMIC", dynamic_path_list);
      otherwise
        error ("javaclasspath: invalid value for WHAT");
    endswitch

  else
    if (! nargin)
      ## This is to allow retrieval of both paths in separate variables with
      ## a single call to javaclasspath(). Matlab returns only the -dynamic
      ## path in this case but this won't break compatibility.
      path1 = cellstr (dynamic_path_list);
      path2 = cellstr (static_path_list);
    else
      switch (tolower (which))
        case "-all",     path1 = cellstr ([static_path_list,dynamic_path_list]);
        case "-dynamic", path1 = cellstr (dynamic_path_list);
        case "-static",  path1 = cellstr (static_path_list);
        otherwise
          error ("javaclasspath: invalid value for WHAT");
      endswitch
    endif
  endif

endfunction

## Display cell array of paths

function disp_path_list (which, path_list)
  printf ("   %s JAVA PATH\n\n", which);
  if (numel (path_list) > 0)
    printf ("      %s\n", path_list{:});
  else
    printf ("      - empty -\n");
  endif
endfunction

