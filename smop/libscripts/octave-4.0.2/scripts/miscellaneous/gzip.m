## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Function File} {@var{filelist} =} gzip (@var{files})
## @deftypefnx {Function File} {@var{filelist} =} gzip (@var{files}, @var{dir})
## Compress the list of files and directories specified in @var{files}.
##
## @var{files} is a character array or cell array of strings.  Shell wildcards
## in the filename such as @samp{*} or @samp{?} are accepted and expanded.
## Each file is compressed separately and a new file with a @file{".gz"}
## extension is created.  The original files are not modified, but existing
## compressed files will be silently overwritten.  If a directory is
## specified then @code{gzip} recursively compresses all files in the
## directory.
##
## If @var{dir} is defined the compressed files are placed in this directory,
## rather than the original directory where the uncompressed file resides.
## If @var{dir} does not exist it is created.
##
## The optional output @var{filelist} is a list of the compressed files.
## @seealso{gunzip, unpack, bzip2, zip, tar}
## @end deftypefn

function filelist = gzip (varargin)

  if (nargin < 1 || nargin > 2 || nargout > 1)
    print_usage ();
  endif

  if (nargout == 0)
    __xzip__ ("gzip", "gz", "gzip -r %s", varargin{:});
  else
    filelist = __xzip__ ("gzip", "gz", "gzip -r %s", varargin{:});
  endif

endfunction


%!xtest
%! ## test gzip together with gunzip
%! unwind_protect
%!   filename = tempname;
%!   dummy    = pi;
%!   save (filename, "dummy");
%!   dirname  = tempname;
%!   mkdir (dirname);
%!   filelist = gzip (filename, dirname);
%!   filelist = filelist{1};
%!   [~, basename, extension] = fileparts (filename);
%!   if (! strcmp (filelist, [dirname, filesep, basename, extension, ".gz"]))
%!     error ("gzipped file does not match expected name!");
%!   endif
%!   if (! exist (filelist, "file"))
%!     error ("gzipped file cannot be found!");
%!   endif
%!   gunzip (filelist);
%!   fid = fopen (filename, "rb");
%!   assert (fid >= 0);
%!   orig_data = fread (fid);
%!   fclose (fid);
%!   fid = fopen ([dirname filesep basename extension], "rb");
%!   assert (fid >= 0);
%!   new_data = fread (fid);
%!   fclose (fid);
%!   if (orig_data != new_data)
%!     error ("gunzipped file not equal to original file!");
%!   endif
%! unwind_protect_cleanup
%!   delete (filename);
%!   delete ([dirname, filesep, basename, extension]);
%!   rmdir (dirname);
%! end_unwind_protect

%!error gzip ()
%!error gzip ("1", "2", "3")
%!error <FILES must be a character array or cellstr> gzip (1)

