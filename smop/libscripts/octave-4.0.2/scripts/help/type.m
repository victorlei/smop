## Copyright (C) 2009-2015 Søren Hauberg
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
## @deftypefn  {Command} {} type @var{name} @dots{}
## @deftypefnx {Command} {} type -q @var{name} @dots{}
## @deftypefnx {Function File} {text =} type ("@var{name}", @dots{})
## Display the contents of @var{name} which may be a file, function (m-file),
## variable, operator, or keyword.
##
## @code{type} normally prepends a header line describing the category of
## @var{name} such as function or variable; The @option{-q} option suppresses
## this behavior.
##
## If no output variable is used the contents are displayed on screen.
## Otherwise, a cell array of strings is returned, where each element
## corresponds to the contents of each requested function.
## @end deftypefn

function text = type (varargin)

  if (nargin == 0)
    print_usage ();
  elseif (! iscellstr (varargin))
    error ("type: input arguments must be strings");
  endif

  quiet = false;
  idx = strcmpi (varargin, "-q") | strcmpi (varargin, "-quiet");
  if (any (idx))
    quiet = true;
    varargin(idx) = [];
  endif

  if (nargout > 0)
    text = cell (size (varargin));
  endif

  for n = 1:length (varargin)
    name = varargin{n};

    ## Find function and get its code
    txt = "";
    cmd = sprintf ("exist ('%s')", name);
    e = evalin ("caller", cmd);
    if (e == 1)
      ## Variable
      cmd = sprintf ("disp (%s);", name);
      desc = evalin ("caller", cmd);
      if (quiet)
        txt = desc;
      else
        txt = sprintf ("%s is a variable\n%s", name, desc);
      endif
    elseif (e == 2)
      ## m-file or ordinary file
      file = which (name);
      if (length (file) > 2)
        ext = file(end-1:end);
      endif
      if (isempty (file) || ! strcmpi (ext, ".m"))
        ## 'name' is an ordinary file, and not a function name.
        file = file_in_loadpath (name);
        quiet = true;
      endif

      ## Read the file
      fid = fopen (file, "r");
      if (fid < 0)
        error ("type: couldn't open '%s' for reading", file);
      endif
      contents = char (fread (fid).');
      fclose (fid);

      if (quiet)
        txt = contents;
      else
        txt = sprintf ("%s is the user-defined function defined from: %s\n\n%s",
                        name, file, contents);
      endif
    elseif (e == 3)
      txt = sprintf ("%s is a dynamically-linked function", name);
    elseif (e == 5)
      txt = sprintf ("%s is a built-in function", name);
    elseif (any (strcmp (__operators__ (), name)))
      txt = sprintf ("%s is an operator", name);
    elseif (any (strcmp (__keywords__ (), name)))
      txt = sprintf ("%s is a keyword", name);
    else
      error ("type: '%s' undefined\n", name);
    endif

    if (nargout == 0)
      disp (txt);
    else
      text{n} = txt;
    endif
  endfor
endfunction


%!test
%! var = 1;
%! text = type ("var");
%! typestr = text{1}(1:17);
%! assert (typestr, "var is a variable");

%!test
%! text = type ("ls");
%! typestr = text{1}(1:31);
%! assert (typestr, "ls is the user-defined function");

%!test
%! text = type ("ls", "-q");
%! typestr = text{1}(1:21);
%! assert (typestr, "## Copyright (C) 2006");

%!assert (type ("amd"){1}, "amd is a dynamically-linked function")
%!assert (type ("cat"){1}, "cat is a built-in function")
%!assert (type ("+"){1}, "+ is an operator")
%!assert (type ("end"){1}, "end is a keyword")

%!error type ()
%!error <'__NO_NAME__' undefined> type ('__NO_NAME__')

