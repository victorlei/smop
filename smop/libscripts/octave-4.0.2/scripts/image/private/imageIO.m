## Copyright (C) 2013-2015 Carnë Draug
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

## This function the image input functions imread() and imfinfo() to the
## functions that will actually be used, based on their format.  See below
## on the details on how it identifies the format, and to what it defaults.
##
## It will change the input arguments that were passed to imread() and
## imfinfo().  It will change the filename to provide the absolute filepath
## for the file, it will extract the possible format name from the rest of
## the input arguments (in case there was one), and will give an error if
## the file can't be found.
##
## Usage:
##
## func      - Function name to use on error message.
## core_func - Function handle for the default function to use if we can't
##             find the format in imformats.
## fieldname - Name of the field in the struct returned by imformats that
##             has the function to use.
## filename  - Most likely the first input argument from the function that
##             called this. May be missing the file extension which can be
##             on varargin.
## varargin  - Followed by all the OTHER arguments passed to imread and
##             imfinfo.

function varargout = imageIO (func, core_func, fieldname, filename, varargin)

  ## First thing: figure out the filename and possibly download it.
  ## The first attempt is to try the filename and see if it exists.  If it
  ## does not, we try to add the next argument since the file extension can
  ## be given as a separate argument.  If we still can't find the file, it
  ## can be a URL.  Lastly, if we still didn't found a file, try adding the
  ## extension to the URL

  file_2_delete = false;  # will we have to remove the file in the end?
  persistent abs_path = @(x) file_in_path (IMAGE_PATH, tilde_expand (x));

  ## Filename was given with file extension
  fn = abs_path (filename);
  if (isempty (fn) && ! isempty (varargin))
    ## Maybe if we add a file extension
    fn = abs_path ([filename "." varargin{1}]);
  endif

  ## Maybe we have an URL
  if (isempty (fn))
    file_2_delete = true; # mark file for deletion
    [fn, ~] = urlwrite (filename, tempname ());
    ## Maybe the URL is missing the file extension
    if (isempty (fn) && ! isempty (varargin))
      [fn, ~] = urlwrite ([filename "." varargin{1}], tempname ());
    endif

    if (isempty (fn))
      error ("%s: unable to find file %s", func, filename);
    endif
  endif

  ## unwind_protect block because we may have a file to remove in the end
  unwind_protect

    ## When guessing the format to use, we first check if the second
    ## argument is a format defined in imformats.  If so, we remove it
    ## from the rest of arguments before passing them on.  If not, we
    ## try to guess the format from the file extension.  Finally, if
    ## we still don't know the format, we use the Octave core functions
    ## which is the same for all formats.
    foo = []; # the function we will use

    ## We check if the call to imformats (ext) worked using
    ## "numfields (fmt) > 0 because when it fails, the returned
    ## struct is not considered empty.

    ## try the second input argument
    if (! isempty (varargin) && ischar (varargin{1}))
      fmt = imformats (varargin{1});
      if (numfields (fmt) > 0)
        foo = fmt.(fieldname);
        varargin(1) = []; # remove format name from arguments
      endif
    endif

    ## try extension from file name
    if (isempty (foo))
      [~, ~, ext] = fileparts (fn);
      if (! isempty (ext))
        ## remove dot from extension
        ext = ext(2:end);
      endif
      fmt = imformats (ext);
      if (numfields (fmt) > 0)
        foo = fmt.(fieldname);
      endif
    endif

    ## use the core function
    if (isempty (foo))
      foo = core_func;
    endif

    [varargout{1:nargout}] = foo (fn, varargin{:});

  unwind_protect_cleanup
    if (file_2_delete)
      unlink (fn);
    endif
  end_unwind_protect

endfunction

