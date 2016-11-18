## Copyright (C) 2013-2015 Carnë Draug
## Copyright (C) 2008-2015 Thomas L. Scofield
## Copyright (C) 2008 Kristian Rumberg
## Copyright (C) 2006 Thomas Weber
## Copyright (C) 2005 Stefan van der Walt
## Copyright (C) 2002 Andy Adler
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

## This function does all the work of imread. It exists here as private
## function so that imread can use other functions if imformats is
## configured to. It is also needed so that imformats can create a
## function handle for it.

## Author: Carnë Draug <carandraug@octave.org>
## Author: Thomas L. Scofield <scofield@calvin.edu>
## Author: Kristian Rumberg <kristianrumberg@gmail.com>
## Author: Thomas Weber <thomas.weber.mail@gmail.com>
## Author: Stefan van der Walt <stefan@sun.ac.za>
## Author: Andy Adler

function varargout = __imread__ (filename, varargin)

  if (nargin < 1)
    print_usage ("imread");
  elseif (! ischar (filename))
    error ("imread: FILENAME must be a string");
  endif

  ## keep track of the varargin offset we're looking at each moment
  offset    = 1;

  ## It is possible for an file with multiple pages to have very different
  ## images on each page. Specifically, they may have different sizes. Because
  ## of this, we need to first find out the index of the images to read so
  ## we can set up defaults for things such as PixelRegion later on.
  options = struct ("index", 1);  # default image index

  ## Index is the only option that can be defined without the parameter/value
  ## pair style. When defining it here, the string "all" is invalid though.
  ## Also, for matlab compatibility, if index is defined both as an option here
  ## and parameter/value pair, silently ignore the first.
  if (nargin >= offset + 1 && ! ischar (varargin{offset}))
    if (! is_valid_index_option (options.index))
      error ("imread: IDX must be a numeric vector");
    endif
    options.index = varargin{offset};
    offset++;
  endif

  if (rem (numel (varargin) - offset + 1, 2) != 0)
    error ("imread: no pair for all arguments (odd number left over)");
  endif

  ## Check key/value options.
  indexes = cellfun ("isclass", varargin, "char");
  indexes(indexes) &= ismember (tolower (varargin(indexes)),
                                {"frames", "index"});
  indexes = find (indexes);
  if (indexes)
    options.index = varargin{indexes+1};
    if (! is_valid_index_option (options.index)
        && ! (ischar (options.index) && strcmpi (options.index, "all")))
      error ("imread: value for %s must be a vector or the string `all'");
    endif
  endif

  ## Use information from the first image to be read to set defaults.
  if (ischar (options.index) && strcmpi (options.index, "all"))
    info = __magick_ping__ (filename, 1);
  else
    info = __magick_ping__ (filename, options.index(1));
  endif

  ## Set default for options.
  options.region = {1:1:info.rows 1:1:info.columns};

  for idx = offset:2:(numel (varargin) - offset + 1)
    switch (tolower (varargin{idx}))

      case {"frames", "index"}
        ## Do nothing. This options were already processed before the loop.

      case "pixelregion",
        options.region = varargin{idx+1};
        if (! iscell (options.region) || numel (options.region) != 2)
          error ("imread: value for %s must be a 2 element cell array",
                 varargin{idx});
        endif
        for reg_idx = 1:2
          if (numel (options.region{reg_idx}) == 3)
            ## do nothing
          elseif (numel (options.region{reg_idx}) == 2)
            options.region{reg_idx}(3) = options.region{reg_idx}(2);
            options.region{reg_idx}(2) = 1;
          else
            error ("imread: range for %s must be a 2 or 3 element vector",
                   varargin{idx});
          endif
          options.region{reg_idx} = floor (options.region{reg_idx}(1)): ...
                                    floor (options.region{reg_idx}(2)): ...
                                    floor (options.region{reg_idx}(3));
        endfor
        if (options.region{1}(end) > info.rows)
          error ("imread: end ROWS for PixelRegions option is larger than image height");
        elseif (options.region{2}(end) > info.columns)
          error ("imread: end COLS for PixelRegions option is larger than image width");
        endif

      case "info",
        ## We ignore this option. This parameter exists in Matlab to
        ## speed up the reading of multipage TIFF by passing a structure
        ## that contains information about the start on the file of each
        ## page.  We can't control it through GraphicsMagic but at least
        ## we allow to load multiple pages with one command.

      otherwise
        error ("imread: invalid PARAMETER `%s'", varargin{idx});

    endswitch
  endfor

  [varargout{1:nargout}] = __magick_read__ (filename, options);

endfunction

## Tests if the value passed to the Index or Frames is valid. This option
## can be defined in two places, but only in one place can it also be the
## string "all"
function bool = is_valid_index_option (arg)
  bool = isvector (arg) && isnumeric (arg) && isreal (arg);
endfunction

