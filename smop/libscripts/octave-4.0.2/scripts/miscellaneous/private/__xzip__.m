## Copyright (C) 2008-2015 Thorsten Meyer
## based on the original gzip function by David Bateman
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
## @deftypefn {Function File} {@var{filelist} =} __xzip__ (@var{commandname}, @var{extension}, @var{commandtemplate}, @var{files}, @var{outdir})
## Undocumented internal function.
## @end deftypefn

## Compress the list of files and/or directories specified in @var{files}
## with the external compression command @var{commandname}. The template
## @var{commandtemplate} is used to actually start the command. Each file
## is compressed separately and a new file with the extension @var{extension}
## is created and placed into the directory @var{outdir}. The original files
## are not touched. Existing compressed files are silently overwritten.
## This is an internal function. Do not use directly.

function filelist = __xzip__ (commandname, extension, commandtemplate,
                              files, outdir)

  if (nargin == 5 && ! exist (outdir, "dir"))
    r = mkdir (outdir);
    if (! r)
      error ("%s: Failed to create output directory DIR", commandname);
    endif
  endif

  if (ischar (files))
    files = cellstr (files);
  elseif (! iscellstr (files))
    error ("%s: FILES must be a character array or cellstr", commandname);
  endif

  if (nargin == 4)
    outdir = tempname ();
    r = mkdir (outdir);
    if (! r)
      error ("%s: Failed to create temporary output directory", commandname);
    endif
  endif

  cwd = pwd ();
  unwind_protect
    files = glob (files);

    ## Ignore any file with the compress extension
    files(cellfun (@(x) (length (x) > length (extension)
                         && strcmp (x((end - length (extension) + 1):end),
                                    extension)),
                   files)) = [];

    copyfile (files, outdir);

    fnames = fname_only (files);

    cd (outdir);

    cmd = sprintf (commandtemplate, sprintf (" %s", fnames{:}));

    [status, output] = system (cmd);
    if (status)
      fcn = evalin ("caller", "mfilename ()");
      error ("%s: %s command failed with exit status = %d",
             fcn, commandname, status);
    endif

    if (nargin == 5)
      if (nargout > 0)
        ## FIXME: This doesn't work if a directory is compressed
        filelist = cellfun (
            @(x) fullfile (outdir, sprintf ("%s.%s", x, extension)),
            fnames, "uniformoutput", false);
      endif
    else
      ## FIXME: This does not work when you try to compress directories
      ##        The resulting name is dir/.gz which is totally wrong.
      ##        See bug #43431.
      movefile (cellfun (@(x) sprintf ("%s.%s", x, extension),
                         fnames, "uniformoutput", false), cwd);
      if (nargout > 0)
        filelist  = cellfun (@(x) sprintf ("%s.%s", x, extension),
                             files, "uniformoutput", false);
      endif
    endif

  unwind_protect_cleanup
    cd (cwd);
    if (nargin == 4)
      confirm_recursive_rmdir (false, "local");
      rmdir (outdir, "s");
    endif
  end_unwind_protect

endfunction

function f = fname_only (files)
  [~, f, ext] = cellfun ("fileparts", files, "uniformoutput", false);
  f = cellfun (@(x, y) sprintf ("%s%s", x, y), f, ext, "uniformoutput", false);
  idx = cellfun ("isdir", files);
  f(idx) = files(idx);
endfunction

