## Copyright (C) 2008-2015 John W. Eaton
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

## This function does all the work of imwrite. It exists here as private
## function so that imwrite can use other functions if imformats is
## configured to. It is also needed so that imformats can create a
## function handle for it.

function __imwrite__ (img, varargin)

  if (nargin < 2 || ! (isnumeric (img) || islogical (img)))
    print_usage ("imwrite");
  endif

  [filename, ext, map, param_list] = imwrite_filename (varargin{:});

  if (isempty (img))
    error ("imwrite: invalid empty image");
  elseif (issparse (img) || issparse (map))
    error ("imwrite: sparse images are not supported");
  endif

  if (rem (numel (param_list), 2) != 0)
    error ("imwrite: no pair for all arguments (odd number left)");
  endif

  ## set default for options
  options = struct ("writemode", "overwrite",
                    "disposalmethod", {repmat({"doNotSpecify"}, 1, size (img, 4))},
                    "quality",   75,
                    "delaytime", ones (1, size (img, 4)) *500, # 0.5 seconds
                    "loopcount", 0, ## this is actually Inf
                    "alpha",     cast ([], class (img)));

  for idx = 1:2:numel (param_list)

    switch (tolower (param_list{idx}))

      case "alpha"
        options.alpha = param_list{idx+1};
        if (! isnumeric (options.alpha))
          error ("imwrite: value for %s option must be a numeric matrix",
                 param_list{idx});
        elseif (size (options.alpha, 3) != 1)
          error ("imwrite: 3rd dimension of matrix for %s must be singleton",
                 param_list{idx});
        elseif (ndims (options.alpha) > 4
                || any (size (options.alpha)([1 2]) != size (img)([1 2]))
                || size (options.alpha, 4) != size (img, 4))
          error ("imwrite: matrix for %s must have same dimension as image",
                 param_list{idx});
        endif

      case "delaytime"
        options.delaytime = param_list{idx+1};
        if (! isnumeric (options.delaytime))
          error ("imwrite: value for %s option must be numeric",
                 param_list{idx});
        endif
        options.delaytime *= 100; # convert to 1/100ths of second
        if (isscalar (options.delaytime))
          options.delaytime(1:size (img, 4)) = options.delaytime;
        elseif (size (img, 4) != numel (options.delaytime))
          error ("imwrite: value for %s must either be a scalar or the number of frames",
                 param_list{idx});
        endif
        if (any (options.delaytime(:) < 0)
            || any (options.delaytime(:) > 65535))
          error ("imwrite: value for %s must be between 0 and 655.35 seconds",
                 param_list{idx});
        endif

      case "disposalmethod"
        options.disposalmethod = param_list{idx+1};
        if (! ischar (options.disposalmethod)
            && ! iscellstr (options.disposalmethod))
          error ("imwrite: value for %s must be a string or cell array of strings",
                 param_list{idx});
        elseif (! iscell (options.disposalmethod))
          options.disposalmethod = {options.disposalmethod};
        endif
        options.disposalmethod = tolower (options.disposalmethod);
        matches = ismember (options.disposalmethod,
                            {"donotspecify", "leaveinplace", ...
                             "restorebg", "restoreprevious"});
        if (any (! matches))
          error ("imwrite: unknown method %s for option %s",
                 options.disposalmethod{find (! matches, 1)},
                 param_list{idx});
        endif
        if (isscalar (options.disposalmethod))
          options.disposalmethod = repmat (options.disposalmethod, ...
                                           1, size (img, 4));
        elseif (numel (options.disposalmethod) != size (img, 4))
          error ("imwrite: if value %s is a cell array must have same length as number of frames",
                 param_list{idx});
        endif

      case "loopcount"
        options.loopcount = param_list{idx+1};
        if (! isscalar (options.loopcount) || ! isnumeric (options.loopcount)
            || (! isinf (options.loopcount)
                && (options.loopcount < 0 || options.loopcount > 65535)))
          error ("imwrite: value for %s must be Inf or between 0 and 65535",
                 param_list{idx});
        endif
        ## Graphics Magick is a bit weird here. A value of 0 will be an
        ## infinite loop, a value of 1, will really be no loop, while a
        ## value of 2 or more will be that number of loops (checked
        ## with GNOME image viewer). This means that there is no way
        ## to make it loop only once. See
        ## https://sourceforge.net/p/graphicsmagick/bugs/249/
        ## There is also the problem of setting this when there is only
        ## a single frame. See
        ## https://sourceforge.net/p/graphicsmagick/bugs/248/
        if (isinf (options.loopcount))
          options.loopcount = 0;
        elseif (options.loopcount == 0 || options.loopcount == 1)
          options.loopcount++;
        endif
        options.loopcount = floor (options.loopcount);

      case "quality",
        options.quality = param_list{idx+1};
        if (! isnumeric (options.quality) || ! isscalar (options.quality)
            || options.quality < 0 || options.quality > 100)
          error ("imwrite: value for %s option must be a scalar between 0 and 100",
                 param_list{idx});
        endif
        options.quality = round (options.quality);

      case "writemode",
        options.writemode = param_list{idx+1};
        if (! ischar (options.writemode)
            || ! any (strcmpi (options.writemode, {"append", "overwrite"})))
          error ('imwrite: value for %s option must be "append" or "overwrite"',
                 param_list{idx});
        endif
        options.writemode = tolower (options.writemode);

      otherwise
        error ("imwrite: invalid PARAMETER `%s'", param_list{idx});

    endswitch
  endfor

  if (! isempty (map))
    if (! iscolormap (map))
      error ("imwrite: invalid MAP for indexed image");
    elseif (ndims (img) != 2 && ndims (img) != 4)
      error ("imwrite: indexed image must have 2 or 4 dimensions (found %i)", ndims (img));
    endif

    ## Fill in the colormap as required with rgb (0, 0, 0) (bug #33615)
    nColors = rows (map);
    if (any (strcmp (class (img), {"uint8", "uint16", "logical"})))
      required_colors = max (img(:)) +1;
    else
      required_colors = max (img(:));
    endif
    if (nColors < required_colors)
      warning ("imwrite: MAP has not enough colors. Filling with black");
      map(nColors+1:required_colors,:) = 0;
    endif

    ## If the image is floating point, then we convert it to integer (makes
    ## it easier in __magick_write__ since it only handles integers. Also,
    ## if it's floating point, it has an offset of 1
    if (isfloat (img))
      if (rows (map) <= 256)
        img = uint8 (img - 1);
      else
        img = uint16 (img - 1);
      endif
    endif
  endif

  if (ndims (img) > 4)
    error ("imwrite: invalid %d-dimensional image data", ndims (img));
  elseif (all (size (img, 3) != [1 3 4]))
    ## 1, 3, or 4 for grayscle, RGB, and CMYK respectively
    error ("imwrite: IMG 3rd dimension must be 1, 3, or 4");
  endif

  __magick_write__ (filename, ext, img, map, options);

endfunction

