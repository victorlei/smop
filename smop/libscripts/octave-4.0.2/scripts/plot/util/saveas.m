## Copyright (C) 2010-2015 Kai Habel
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
## @deftypefn  {Function File} {} saveas (@var{h}, @var{filename})
## @deftypefnx {Function File} {} saveas (@var{h}, @var{filename}, @var{fmt})
## Save graphic object @var{h} to the file @var{filename} in graphic format
## @var{fmt}.
##
## @var{fmt} should be one of the following formats:
##
## @table @code
##   @item ps
##     PostScript
##
##   @item eps
##     Encapsulated PostScript
##
##   @item jpg
##     JPEG Image
##
##   @item png
##     PNG Image
##
##   @item emf
##     Enhanced Meta File
##
##   @item pdf
##     Portable Document Format
## @end table
##
## All device formats specified in @code{print} may also be used.  If
## @var{fmt} is omitted it is extracted from the extension of @var{filename}.
## The default format is @qcode{"pdf"}.
##
## @example
## @group
## clf ();
## surf (peaks);
## saveas (1, "figure1.png");
## @end group
## @end example
##
## @seealso{print, hgsave, orient}
## @end deftypefn

## Author: Kai Habel

function saveas (h, filename, fmt = "pdf")

  if (nargin != 2 && nargin != 3)
    print_usage ();
  endif

  if (ishandle (h))
    if (isfigure (h))
      fig = h;
    else
      fig = ancestor (h, "figure");
    endif
  else
    error ("saveas: first argument H must be a graphics handle");
  endif

  if (! ischar (filename))
    error ("saveas: FILENAME must be a string");
  endif

  if (nargin == 2)
    [~, ~, ext] = fileparts (filename);
    if (! isempty (ext))
      fmt = ext(2:end);
    endif
  endif

  if (nargin == 3)
    if (! ischar (filename))
      error ("saveas: EXT must be a string");
    endif

    [~, ~, ext] = fileparts (filename);

    if (isempty (ext))
      filename = strcat (filename, ".", fmt);
    endif
  endif

  prt_opt = strcat ("-d", tolower (fmt));

  print (fig, filename, prt_opt);

endfunction

