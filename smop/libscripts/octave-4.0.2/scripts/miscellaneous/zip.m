## Copyright (C) 2006-2015 Sylvain Pelissier
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
## @deftypefn  {Function File} {@var{filelist} =} zip (@var{zipfile}, @var{files})
## @deftypefnx {Function File} {@var{filelist} =} zip (@var{zipfile}, @var{files}, @var{rootdir})
## Compress the list of files and directories specified in @var{files} into the
## ZIP archive @var{zipfile}.
##
## @var{files} is a character array or cell array of strings.  Shell
## wildcards in the filename such as @samp{*} or @samp{?} are accepted and
## expanded.  Directories are recursively traversed and all files are
## compressed and added to the archive.
##
## If @var{rootdir} is defined then any files without absolute pathnames are
## located relative to @var{rootdir} rather than the current directory.
##
## The optional output @var{filelist} is a list of the files that were included
## in the archive.
## @seealso{unzip, unpack, bzip2, gzip, tar}
## @end deftypefn

## Author: Sylvain Pelissier <sylvain.pelissier@gmail.com>

function filelist = zip (zipfile, files, rootdir = ".")

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! ischar (zipfile))
    error ("zip: ZIPFILE must be a string");
  elseif (ischar (files))
    files = cellstr (files);
  elseif (! iscellstr (files))
    error ("zip: FILES must be a character array or cellstr");
  endif

  rootdir = tilde_expand (rootdir);

  zipfile = make_absolute_filename (zipfile);

  cmd = sprintf ("zip -r %s %s", zipfile, sprintf (" %s", files{:}));

  origdir = pwd ();
  cd (rootdir);
  [status, output] = system (cmd);
  cd (origdir);

  if (status)
    error ("zip: zip failed with exit status = %d", status);
  endif

  if (nargout > 0)
    cmd = ["unzip -Z -1 " zipfile];
    [status, filelist] = system (cmd);
    if (status)
      error ("zip: zipinfo failed with exit status = %d", status);
    endif
    filelist = ostrsplit (filelist, "\r\n", true);
  endif

endfunction


%!xtest
%! ## test zip together with unzip
%! unwind_protect
%!   filename = tempname ();
%!   tmp_var  = pi;
%!   save (filename, "tmp_var");
%!   dirname = tempname ();
%!   mkdir (dirname);
%!   zipfile = tempname ();
%!   [~, basename, ext] = fileparts (filename);
%!   filelist = zip (zipfile, [basename ext], tempdir);
%!   filelist = filelist{1};
%!   if (! strcmp (filelist, [basename ext]))
%!     error ("zip archive does not contain expected name!");
%!   endif
%!   if (! exist ([zipfile ".zip"], "file"))
%!     error ("zip file cannot be found!");
%!   endif
%!   unzip ([zipfile ".zip"], dirname);
%!   fid = fopen (filename, "rb");
%!   assert (fid >= 0);
%!   orig_data = fread (fid);
%!   fclose (fid);
%!   fid = fopen ([dirname filesep basename ext], "rb");
%!   assert (fid >= 0);
%!   new_data = fread (fid);
%!   fclose (fid);
%!   if (orig_data != new_data)
%!     error ("unzipped file not equal to original file!");
%!   endif
%! unwind_protect_cleanup
%!   unlink (filename);
%!   unlink ([dirname, filesep, basename, ext]);
%!   rmdir (dirname);
%! end_unwind_protect

## Test input validation
%!error zip ()
%!error zip (1)
%!error zip (1,2,3,4)
%!error <ZIPFILE must be a string> zip (1, "foobar")
%!error <FILES must be a character array or cellstr> zip ("foobar", 1)

