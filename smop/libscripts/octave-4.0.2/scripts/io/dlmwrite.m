## Copyright (C) 2002-2015 Paul Kienzle
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
## @deftypefn  {Function File} {} dlmwrite (@var{file}, @var{M})
## @deftypefnx {Function File} {} dlmwrite (@var{file}, @var{M}, @var{delim}, @var{r}, @var{c})
## @deftypefnx {Function File} {} dlmwrite (@var{file}, @var{M}, @var{key}, @var{val} @dots{})
## @deftypefnx {Function File} {} dlmwrite (@var{file}, @var{M}, "-append", @dots{})
## @deftypefnx {Function File} {} dlmwrite (@var{fid}, @dots{})
## Write the matrix @var{M} to the named file using delimiters.
##
## @var{file} should be a file name or writable file ID given by @code{fopen}.
##
## The parameter @var{delim} specifies the delimiter to use to separate values
## on a row.
##
## The value of @var{r} specifies the number of delimiter-only lines to add to
## the start of the file.
##
## The value of @var{c} specifies the number of delimiters to prepend to each
## line of data.
##
## If the argument @qcode{"-append"} is given, append to the end of @var{file}.
##
## In addition, the following keyword value pairs may appear at the end of
## the argument list:
##
## @table @asis
## @item @qcode{"append"}
## Either @qcode{"on"} or @qcode{"off"}.  See @qcode{"-append"} above.
##
## @item @qcode{"delimiter"}
## See @var{delim} above.
##
## @item @qcode{"newline"}
## The character(s) to use to separate each row.  Three special cases exist
## for this option.  @qcode{"unix"} is changed into
## @qcode{"@xbackslashchar{}n"}, @qcode{"pc"} is changed into
## @qcode{"@xbackslashchar{}r@xbackslashchar{}n"}, and @qcode{"mac"} is
## changed into @qcode{"@xbackslashchar{}r"}.  Any other value is used
## directly as the newline separator.
##
## @item @qcode{"roffset"}
## See @var{r} above.
##
## @item @qcode{"coffset"}
## See @var{c} above.
##
## @item @qcode{"precision"}
## The precision to use when writing the file.  It can either be a format
## string (as used by fprintf) or a number of significant digits.
## @end table
##
## @example
## dlmwrite ("file.csv", reshape (1:16, 4, 4));
## @end example
##
## @example
## dlmwrite ("file.tex", a, "delimiter", "&", "newline", "\n")
## @end example
##
## @seealso{dlmread, csvread, csvwrite}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
##
## This program was originally granted to the public domain
##
## 2002-03-08 Paul Kienzle <pkienzle@users.sf.net>
## * Initial revision
## 2005-11-27 Bill Denney <bill@givebillmoney.com>
## * Significant modifications of the input arguments for additional
## functionality.

function dlmwrite (file, M, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  ## set defaults
  delim = ",";
  r = c = 0;
  newline = "\n";
  if (ischar (M))
    precision = "%c";
  else
    precision = "%.16g";
  endif
  opentype = "wt";

  ## process the input arguments
  i = 0;
  while (i < length (varargin))
    i++;
    if (strcmpi (varargin{i}, "delimiter"))
      delim = varargin{++i};
    elseif (strcmpi (varargin{i}, "newline"))
      newline = varargin{++i};
      if (strcmpi (newline, "unix"))
        newline = "\n";
      elseif (strcmpi (newline, "pc"))
        newline = "\r\n";
      elseif (strcmpi (newline, "mac"))
        newline = "\r";
      endif
    elseif (strcmpi (varargin{i}, "roffset"))
      r = varargin{++i};
    elseif (strcmpi (varargin{i}, "coffset"))
      c = varargin{++i};
    elseif (strcmpi (varargin{i}, "precision"))
      precision = varargin{++i};
      if (! strcmpi (class (precision), "char"))
        precision = sprintf ("%%.%gg", precision);
      endif
    elseif (strcmpi (varargin{i}, "-append"))
      opentype = "at";
    elseif (strcmpi (varargin{i}, "append"))
      i++;
      if (strcmpi (varargin{i}, "on"))
        opentype = "at";
      elseif (strcmpi (varargin{i}, "off"))
        opentype = "wt";
      else
        error ('dlmwrite: append must be "on" or "off"');
      endif
    else
      if (i == 1)
        delim = varargin{i};
      elseif (i == 2)
        r = varargin{i};
      elseif (i == 3)
        c = varargin{i};
      else
        print_usage ();
      endif
    endif
  endwhile

  ## Expand '\t' to TAB for Matlab compatibility
  if (strcmp (delim, '\t'))
    delim = "\t";
  endif

  if (ischar (file))
    [fid, msg] = fopen (file, opentype);
  elseif (isscalar (file) && isnumeric (file))
    [fid, msg] = deal (file, "invalid file number");
  else
    error ("dlmwrite: FILE must be a filename string or numeric FID");
  endif

  if (fid < 0)
    error (["dlmwrite: " msg]);
  else
    if (r > 0)
      fprintf (fid, "%s",
               repmat ([repmat(delim, 1, c + columns(M)-1), newline], 1, r));
    endif
    if (iscomplex (M))
      cprecision = regexprep (precision, '^%([-\d.])', '%+$1');
      template = [precision, cprecision, "i", ...
                  repmat([delim, precision, cprecision, "i"], 1, ...
                  columns(M) - 1), newline ];
    else
      template = [precision, repmat([delim, precision], 1, columns(M)-1),...
                  newline];
    endif
    if (c > 0)
      template = [repmat(delim, 1, c), template];
    endif
    if (iscomplex (M))
      M = M.';
      b = zeros (2*rows (M), columns (M));
      b(1: 2 : end, :) = real (M);
      b(2: 2 : end, :) = imag (M);
      fprintf (fid, template, b);
    else
      fprintf (fid, template, M.');
    endif
    if (! isscalar (file))
      fclose (fid);
    endif
  endif

endfunction


%!test
%! f = tempname ();
%! dlmwrite (f,[1,2;3,4],"precision","%5.2f","newline","unix","roffset",1,"coffset",1);
%! fid = fopen (f,"rt");
%! f1 = char (fread (fid,Inf,"char")');
%! fclose (fid);
%! dlmwrite (f,[5,6],"precision","%5.2f","newline","unix","coffset",1,"delimiter",",","-append");
%! fid = fopen (f,"rt");
%! f2 = char (fread (fid,Inf,"char")');
%! fclose (fid);
%! unlink (f);
%!
%! assert (f1,",,\n, 1.00, 2.00\n, 3.00, 4.00\n");
%! assert (f2,",,\n, 1.00, 2.00\n, 3.00, 4.00\n, 5.00, 6.00\n");

