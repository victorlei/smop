## Copyright (C) 2005-2015 William Poetra Yoga Hadisoeseno
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
## @deftypefn  {Function File} {} ver
## @deftypefnx {Function File} {} ver Octave
## @deftypefnx {Function File} {} ver @var{package}
## @deftypefnx {Function File} {v =} ver (@dots{})
##
## Display a header containing the current Octave version number, license
## string, and operating system.  The header is followed by a list of installed
## packages, versions, and installation directories.
##
## Use the package name @var{package} or Octave to limit the listing to a
## desired component.
##
## When called with an output argument, return a vector of structures
## describing Octave and each installed package.  The structure includes the
## following fields.
##
## @table @code
## @item Name
## Package name.
##
## @item Version
## Version of the package.
##
## @item Revision
## Revision of the package.
##
## @item Date
## Date of the version/revision.
## @end table
##
## @seealso{version, octave_config_info, usejava, pkg}
## @end deftypefn

## Author: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function retval = ver (package = "")

  if (nargin > 1)
    print_usage ();
  endif

  if (nargout == 0)
    [unm, err] = uname ();

    if (err)
      os_string = "unknown";
    else
      os_string = sprintf ("%s %s %s %s",
                           unm.sysname, unm.release, unm.version, unm.machine);
    endif

    hbar(1:70) = "-";
    desc = {hbar
            ["GNU Octave Version: " OCTAVE_VERSION]
            ["GNU Octave License: " license]
            ["Operating System: " os_string]
            hbar};

    printf ("%s\n", desc{:});

    if (isempty (package))
      pkg ("list");
    elseif (strcmpi (package, "Octave"))
      ## Nothing to do, Octave version was already reported
    else
      pkg ("list", package);
    endif
  else
    ## Get the installed packages
    if (isempty (package))
      lst = pkg ("list");
      ## Start with the version info for Octave
      retval = struct ("Name", "Octave", "Version", version,
                       "Release", [], "Date", []);
      for i = 1:numel (lst)
        retval(end+1) = struct ("Name", lst{i}.name, "Version", lst{i}.version,
                                "Release", [], "Date", lst{i}.date);
      endfor
    elseif (strcmpi (package, "Octave"))
      retval = struct ("Name", "Octave", "Version", version,
                       "Release", [], "Date", []);
    else
      lst = pkg ("list", package);
      if (isempty (lst))
        retval = struct ("Name", "", "Version", [],
                         "Release", [], "Date", []);
      else
        retval = struct ("Name", lst{1}.name, "Version", lst{1}.version,
                         "Release", [], "Date", lst{1}.date);
      endif
    endif
  endif

endfunction


%!test
%! result = ver;
%! assert (result(1).Name, "Octave");
%! assert (result(1).Version, version);
%! result = ver ("octave");
%! assert (result(1).Name, "Octave");
%! assert (result(1).Version, version);

%!test
%! lst = pkg ("list");
%! for n = 1:numel (lst)
%!   expected = lst{n}.name;
%!   result = ver (expected);
%!   assert (result.Name, expected);
%!   assert (isfield (result, "Version"), true);
%!   assert (isfield (result, "Release"), true);
%!   assert (isfield (result, "Date"), true);
%! endfor

