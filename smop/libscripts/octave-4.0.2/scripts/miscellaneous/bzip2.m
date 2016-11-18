## Copyright (C) 2008-2015 Thorsten Meyer
## (based on gzip.m by David Bateman)
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
## @deftypefn  {Function File} {@var{filelist} =} bzip2 (@var{files})
## @deftypefnx {Function File} {@var{filelist} =} bzip2 (@var{files}, @var{dir})
## Compress the list of files specified in @var{files}.
##
## @var{files} is a character array or cell array of strings.  Shell wildcards
## in the filename such as @samp{*} or @samp{?} are accepted and expanded.
## Each file is compressed separately and a new file with a @file{".bz2"}
## extension is created.  The original files are not modified, but existing
## compressed files will be silently overwritten.
##
## If @var{dir} is defined the compressed files are placed in this directory,
## rather than the original directory where the uncompressed file resides.
## If @var{dir} does not exist it is created.
##
## The optional output @var{filelist} is a list of the compressed files.
## @seealso{bunzip2, unpack, gzip, zip, tar}
## @end deftypefn

function filelist = bzip2 (varargin)

  if (nargin < 1 || nargin > 2 || nargout > 1)
    print_usage ();
  endif

  if (nargout == 0)
    __xzip__ ("bzip2", "bz2", "bzip2 %s", varargin{:});
  else
    filelist = __xzip__ ("bzip2", "bz2", "bzip2 %s", varargin{:});
  endif

endfunction


%!xtest
%! ## test bzip2 together with bunzip2
%! unwind_protect
%!   filename = tempname;
%!   dummy    = pi;
%!   save (filename, "dummy");
%!   dirname  = tempname;
%!   mkdir (dirname);
%!   filelist = bzip2 (filename, dirname);
%!   filelist = filelist{1};
%!   [~, basename, extension] = fileparts (filename);
%!   if (! strcmp (filelist, [dirname, filesep, basename, extension, ".bz2"]))
%!     error ("bzipped file does not match expected name!");
%!   endif
%!   if (! exist (filelist, "file"))
%!     error ("bzipped file cannot be found!");
%!   endif
%!   bunzip2 (filelist);
%!   fid = fopen (filename, "rb");
%!   assert (fid >= 0);
%!   orig_data = fread (fid);
%!   fclose (fid);
%!   fid = fopen ([dirname filesep basename extension], "rb");
%!   assert (fid >= 0);
%!   new_data = fread (fid);
%!   fclose (fid);
%!   if (orig_data != new_data)
%!     error ("bunzipped file not equal to original file!");
%!   endif
%! unwind_protect_cleanup
%!   delete (filename);
%!   delete ([dirname, filesep, basename, extension]);
%!   rmdir (dirname);
%! end_unwind_protect

