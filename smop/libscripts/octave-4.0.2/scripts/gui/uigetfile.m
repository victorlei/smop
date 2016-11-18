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
## @deftypefn  {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uigetfile ()
## @deftypefnx {Function File} {[@dots{}] =} uigetfile (@var{flt})
## @deftypefnx {Function File} {[@dots{}] =} uigetfile (@var{flt}, @var{dialog_name})
## @deftypefnx {Function File} {[@dots{}] =} uigetfile (@var{flt}, @var{dialog_name}, @var{default_file})
## @deftypefnx {Function File} {[@dots{}] =} uigetfile (@dots{}, "Position", [@var{px} @var{py}])
## @deftypefnx {Function File} {[@dots{}] =} uigetfile (@dots{}, "MultiSelect", @var{mode})
##
## Open a GUI dialog for selecting a file and return the filename @var{fname},
## the path to this file @var{fpath}, and the filter index @var{fltidx}.
##
## @var{flt} contains a (list of) file filter string(s) in one of the following
## formats:
##
## @table @asis
## @item @qcode{"/path/to/filename.ext"}
## If a filename is given then the file extension is extracted and used as
## filter.  In addition, the path is selected as current path and the filename
## is selected as default file.  Example: @code{uigetfile ("myfun.m")}
##
## @item A single file extension @qcode{"*.ext"}
## Example: @code{uigetfile ("*.ext")}
##
## @item A 2-column cell array
## containing a file extension in the first column and a brief description in
## the second column.
## Example: @code{uigetfile (@{"*.ext", "My Description";"*.xyz",
## "XYZ-Format"@})}
##
## The filter string can also contain a semicolon separated list of filter
## extensions.
## Example: @code{uigetfile (@{"*.gif;*.png;*.jpg", "Supported Picture
## Formats"@})}
## @end table
##
## @var{dialog_name} can be used to customize the dialog title.
##
## If @var{default_file} is given then it will be selected in the GUI dialog.
## If, in addition, a path is given it is also used as current path.
##
## The screen position of the GUI dialog can be set using the
## @qcode{"Position"} key and a 2-element vector containing the pixel
## coordinates.  Two or more files can be selected when setting the
## @qcode{"MultiSelect"} key to @qcode{"on"}.  In that case @var{fname} is a
## cell array containing the files.
## @seealso{uiputfile, uigetdir}
## @end deftypefn

## Author: Kai Habel

function [retfile, retpath, retindex] = uigetfile (varargin)

  if (nargin > 7)
    error ("uigetfile: number of input arguments must be less than eight");
  endif

  ## Preset default values
  outargs = {cell(0, 2),         # File Filter
             "Open File",        # Dialog Title
             "",                 # Default file name
             [240, 120],         # Dialog Position (pixel x/y)
             "off",              # MultiSelect on/off
             pwd};               # Default directory

  idx1 = idx2 = [];
  if (length (varargin) > 0)
    for i = 1 : length (varargin)
      val = varargin{i};
      if (ischar (val))
        val = tolower (val);
        if (strcmp (val, "multiselect"))
          idx1 = i;
        elseif (strcmp (val, "position"))
          idx2 = i;
        endif
      endif
    endfor
  endif

  stridx = [idx1, idx2, 0];
  if (length (stridx) > 1)
    stridx = min (stridx(1 : end - 1));
  endif

  args = varargin;
  if (stridx)
    args = varargin(1 : stridx - 1);
  endif

  len = length (args);
  if (len > 0)
    file_filter = args{1};
    [outargs{1}, outargs{3}, defdir] = __file_filter__ (file_filter);
    if (length (defdir) > 0)
      outargs{6} = defdir;
    endif
  else
    outargs{1} = __file_filter__ (outargs{1});
  endif

  if (len > 1)
    if (ischar (args{2}))
      if (length (args{2}) > 0)
        outargs{2} = args{2};
      endif
    elseif (! isempty (args{2}))
      print_usage ();
    endif
  endif

  if (len > 2)
    if (ischar (args{3}))
      if (isdir (args{3}))
        fdir = args{3};
        fname = fext = "";
      else
        [fdir, fname, fext] = fileparts (varargin{3});
      endif
      if (length (fdir) > 0)
        outargs{6} = fdir;
      endif
      if (length (fname) > 0 || length (fext) > 0)
        outargs{3} = strcat (fname, fext);
      endif
    elseif (! isempty (args{3}))
      print_usage ();
    endif
  endif

  if (stridx)
    ## string arguments ("position" or "multiselect")

    ## check for even number of remaining arguments, prop/value pair(s)
    if (rem (nargin - stridx + 1, 2))
      error ("uigetfile: expecting property/value pairs");
    endif

    for i = stridx : 2 : nargin
      prop = varargin{i};
      val = varargin{i + 1};
      if (strcmpi (prop, "position"))
        if (isnumeric (val) && length (val) == 2)
          outargs{4} = val;
        else
          error ("uigetfile: expecting 2-element vector for position argument");
        endif
      elseif (strcmpi (prop, "multiselect"))
        if (ischar (val))
          outargs{5} = tolower (val);
        else
          error ("uigetfile: expecting string argument (on/off) for multiselect");
        endif
      else
        error ("uigetfile: unknown argument");
      endif
    endfor
  endif

  if (__octave_link_enabled__ ())
    [retfile, retpath, retindex] = __octave_link_file_dialog__ (outargs{:});
  else
    funcname = __get_funcname__ (mfilename ());
    [retfile, retpath, retindex] = feval (funcname, outargs{:});
  endif

endfunction


%!demo
%! uigetfile ({'*.gif;*.png;*.jpg', 'Supported Picture Formats'});

## Remove from test statistics.  No real tests possible.
%!assert (1)

