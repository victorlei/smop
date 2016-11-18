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
## @deftypefn  {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uiputfile ()
## @deftypefnx {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uiputfile (@var{flt})
## @deftypefnx {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uiputfile (@var{flt}, @var{dialog_name})
## @deftypefnx {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uiputfile (@var{flt}, @var{dialog_name}, @var{default_file})
## Open a GUI dialog for selecting a file.
##
## @var{flt} contains a (list of) file filter string(s) in one of the following
## formats:
##
## @table @asis
## @item @qcode{"/path/to/filename.ext"}
## If a filename is given the file extension is extracted and used as filter.
## In addition the path is selected as current path and the filename is
## selected as default file.  Example: @code{uiputfile ("myfun.m")}
##
## @item @qcode{"*.ext"}
## A single file extension.
## Example: @code{uiputfile ("*.ext")}
##
## @item @code{@{"*.ext", "My Description"@}}
## A 2-column cell array containing the file extension in the 1st column and
## a brief description in the 2nd column.
## Example: @code{uiputfile (@{"*.ext","My Description";"*.xyz",
## "XYZ-Format"@})}
## @end table
##
## The filter string can also contain a semicolon separated list of filter
## extensions.
## Example: @code{uiputfile (@{"*.gif;*.png;*.jpg",
## "Supported Picture Formats"@})}
##
## @var{dialog_name} can be used to customize the dialog title.
## If @var{default_file} is given it is preselected in the GUI dialog.
## If, in addition, a path is given it is also used as current path.
## @seealso{uigetfile, uigetdir}
## @end deftypefn

## Author: Kai Habel

function [retfile, retpath, retindex] = uiputfile (varargin)

  if (nargin > 3)
    print_usage ();
  endif

  ## Preset default values
  outargs = {cell(0, 2),     # File Filter
             "Save File",    # Dialog Title
             "",             # Default file name
             [240, 120],     # Dialog Position (pixel x/y)
             "create",
             pwd};           # Default directory

  if (nargin > 0)
    file_filter = varargin{1};
    [outargs{1}, outargs{3}, defdir] = __file_filter__ (file_filter);
    if (length (defdir) > 0)
      outargs{6} = defdir;
    endif
  else
    outargs{1} = __file_filter__ (outargs{1});
  endif

  if (nargin > 1)
    if (ischar (varargin{2}))
      outargs{2} = varargin{2};
    elseif (! isempty (varargin{2}))
      print_usage ();
    endif
  endif

  if (nargin > 2)
    if (ischar (varargin{3}))
      if (isdir (varargin{3}))
        fdir = varargin{3};
        fname = fext = "";
      else
        [fdir, fname, fext] = fileparts (varargin{3});
      endif
      if (! isempty (fdir))
        outargs{6} = fdir;
      endif
      if (! isempty (fname) || ! isempty (fext))
        outargs{3} = strcat (fname, fext);
      endif
    elseif (! isempty (varargin{3}))
      print_usage ();
    endif
  endif

  if (__octave_link_enabled__ ())
    [retfile, retpath, retindex] = __octave_link_file_dialog__ (outargs{:});
  else
    funcname = __get_funcname__ (mfilename ());
    [retfile, retpath, retindex] = feval (funcname, outargs{:});
  endif

  ## Append extension to the name if it isn't already added.
  if (ischar (retfile))
    ext = outargs{1}{retindex};
    ext = strrep (ext, "*", "");

    if (length (retfile) >= length (ext))
      if (! strcmp (retfile(end-length (ext)+1:end), ext))
        retfile = [retfile ext];
      endif
    endif
  endif

endfunction


%!demo
%! uiputfile ({'*.gif;*.png;*.jpg', 'Supported Picture Formats'});

## Remove from test statistics.  No real tests possible.
%!assert (1)

