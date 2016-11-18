## Copyright (C) 2003-2015 John W. Eaton
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
## @deftypefn {Function File} {[@var{dir}, @var{name}, @var{ext}] =} fileparts (@var{filename})
## Return the directory, name, and extension components of @var{filename}.
##
## The input @var{filename} is a string which is parsed.  There is no attempt
## to check whether the filename or directory specified actually exists.
## @seealso{fullfile, filesep}
## @end deftypefn

function [dir, name, ext] = fileparts (filename)

  if (nargin != 1)
    print_usage ();
  endif

  if (! ischar (filename) || rows (filename) > 1)
    error ("fileparts: FILENAME must be a single string");
  endif

  ds = strchr (filename, filesep ("all"), 1, "last");
  if (isempty (ds))
    ds = 0;
  endif
  es = rindex (filename, ".");
  ## These can be the same if they are both 0 (no dir or ext).
  if (es <= ds)
    es = length (filename)+1;
  endif

  if (ds == 0)
    dir = "";
  elseif (ds == 1)
    dir = filename(1);
  else
    dir = filename(1:ds-1);
  endif

  name = filename(ds+1:es-1);
  if (isempty (name))
    name = "";
  endif

  if (es > 0 && es <= length (filename))
    ext = filename(es:end);
  else
    ext = "";
  endif

endfunction


%!test
%! [d, n, e] = fileparts ("file");
%! assert (strcmp (d, "") && strcmp (n, "file") && strcmp (e, ""));

%!test
%! [d, n, e] = fileparts ("file.ext");
%! assert (strcmp (d, "") && strcmp (n, "file") && strcmp (e, ".ext"));

%!test
%! [d, n, e] = fileparts ("/file.ext");
%! assert (strcmp (d, "/") && strcmp (n, "file") && strcmp (e, ".ext"));

%!test
%! [d, n, e] = fileparts ("dir/file.ext");
%! assert (strcmp (d, "dir") && strcmp (n, "file") && strcmp (e, ".ext"));

%!test
%! [d, n, e] = fileparts ("./file.ext");
%! assert (strcmp (d, ".") && strcmp (n, "file") && strcmp (e, ".ext"));

%!test
%! [d, n, e] = fileparts ("d1/d2/file.ext");
%! assert (strcmp (d, "d1/d2") && strcmp (n, "file") && strcmp (e, ".ext"));

%!test
%! [d, n, e] = fileparts ("/d1/d2/file.ext");
%! assert (strcmp (d, "/d1/d2") && strcmp (n, "file") && strcmp (e, ".ext"));

%!test
%! [d, n, e] = fileparts ("/.ext");
%! assert (strcmp (d, "/") && strcmp (n, "") && strcmp (e, ".ext"));

%!test
%! [d, n, e] = fileparts (".ext");
%! assert (strcmp (d, "") && strcmp (n, "") && strcmp (e, ".ext"));

## Test input validation
%!error fileparts ()
%!error fileparts (1,2)
%!error <FILENAME must be a single string> fileparts (1)
%!error <FILENAME must be a single string> fileparts (["a"; "b"])

