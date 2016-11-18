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

## The input check for imwrite needs to be done twice, once when imwrite
## is called the first time to find where the filename is, and a second
## time by __imwrite__ after imformats decides what function to use.
## Because a user can, and is encouraged to, get a function handle to
## __imwrite__, the input check is also done there.
## In addition, the input check for imwrite is not that straightforward
## in order to support the multiple ways the function can be called,
## and interpretations of Matlab documentation.
##
## Anyway, this will only do the input check until it finds the filename
## to be used, the only part that imwrite actually needs.

function [filename, ext, cmap, options] = imwrite_filename (varargin)

  ## First, we check if the first argument is a colormap or a filename.
  cmap = [];
  if (ischar (varargin{1}))
    filename_idx = 1;
  elseif (numel (varargin) >= 2
          && iscolormap (varargin{1}) && ischar (varargin{2}))
    filename_idx = 2;
    cmap = varargin{1};
  else
    error ("imwrite: no FILENAME specified");
  endif
  filename = tilde_expand (varargin{filename_idx});

  ## Next, we get the file extension.
  ## if we have an odd number of leftover arguments, and the next argument
  ## is a string, we consider it the file extension. Otherwise we will
  ## extract what we can from the previously found filename.
  options_idx = filename_idx + 1;
  if (numel (varargin) > filename_idx
      && rem (length (varargin) - filename_idx, 2) != 0
      && ischar (varargin{filename_idx + 1}))
    ext = varargin{filename_idx + 1};
    options_idx++;
  else
    [~, ~, ext] = fileparts (filename);
    if (! isempty (ext))
      ## remove dot from extension
      ext = ext(2:end);
    endif
  endif

  ## After all the work finding where the filename was, we might as well
  ## send the leftovers list (they should be in key value pairs)
  options = varargin(options_idx:end);

endfunction

