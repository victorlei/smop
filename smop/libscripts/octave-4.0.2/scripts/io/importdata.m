## Copyright (C) 2012-2015 Erik Kjellson
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
## @deftypefn  {Function File} {@var{A} =} importdata (@var{fname})
## @deftypefnx {Function File} {@var{A} =} importdata (@var{fname}, @var{delimiter})
## @deftypefnx {Function File} {@var{A} =} importdata (@var{fname}, @var{delimiter}, @var{header_rows})
## @deftypefnx {Function File} {[@var{A}, @var{delimiter}] =} importdata (@dots{})
## @deftypefnx {Function File} {[@var{A}, @var{delimiter}, @var{header_rows}] =} importdata (@dots{})
## Import data from the file @var{fname}.
##
## Input parameters:
##
## @itemize
## @item @var{fname}
## The name of the file containing data.
##
## @item @var{delimiter}
## The character separating columns of data.  Use @code{\t} for tab.
## (Only valid for ASCII files)
##
## @item @var{header_rows}
## The number of header rows before the data begins.  (Only valid for ASCII
## files)
## @end itemize
##
## Different file types are supported:
##
## @itemize
## @item ASCII table
##
## Import ASCII table using the specified number of header rows and the
## specified delimiter.
##
## @item Image file
##
## @item @sc{matlab} file
##
## @item Spreadsheet files (depending on external software)
##
## @item WAV file
##
## @end itemize
##
## @seealso{textscan, dlmread, csvread, load}
## @end deftypefn

## Author: Erik Kjellson <erikiiofph7@users.sourceforge.net>

function [output, delimiter, header_rows] = importdata (fname, delimiter = "", header_rows = -1)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! ischar (fname))
    error ("importdata: FNAME must be a string");
  elseif (strcmpi (fname, "-pastespecial"))
    error ("importdata: option -pastespecial not implemented");
  endif

  if (nargin > 1)
    if (! ischar (delimiter)
        || (length (delimiter) > 1 && ! strcmp (delimiter, '\t')))
      error("importdata: DELIMITER must be a single character");
    endif
    if (strcmp (delimiter, '\t'))
      delimiter = "\t";
    endif
  endif

  if (nargin > 2)
    if (! isnumeric (header_rows) || header_rows < 0
        || header_rows != fix (header_rows))
      error ("importdata: HEADER_ROWS must be an integer >= 0");
    endif
  endif

  ## Check file format
  ## Get the extension from the file name.
  [~, ~, ext, ~] = fileparts (fname);
  ext = lower (ext);

  switch (ext)
    case {".au", ".snd", ".flac", ".ogg"}
      error ("importdata: not implemented for file format %s", ext);
    case {".avi", ".mj2", ".mpg", ".asf", ".asx", ".wmv", ".mp4", ".m4v", ...
          ".mov"}
      error ("importdata: not implemented for file format %s", ext);
    case {".bmp", ".cur", ".gif", ".hdf", ".ico", ".jpe", ".jpeg", ".jpg", ...
          ".jp2", ".jpf", ".jpx", ".j2c", ".j2k", ".pbm", ".pcx", ".pgm", ...
          ".png", ".pnm", ".ppm", ".ras", ".tif", ".tiff", ".xwd"}
      delimiter = NaN;
      header_rows = 0;
      [output.cdata, output.colormap, output.alpha] = imread (fname);
    case ".mat"
      delimiter = NaN;
      header_rows = 0;
      output = load (fname);
    case {".xls", ".xlsx", ".wk1", ".dbf", ".pxl"}
      ## If there's no Excel file support simply fall back to unimplemented.m
      output = xlsread (fname);
    case {".ods", ".sxc", ".fods", ".uos", ".xml"}
      ## unimplemented.m only knows ML functions; odsread isn't one but is in OF
      try
        output = odsread (fname);
      catch
        ## Fall back to unimplemented.m.
        output = xlsread (fname);
      end_try_catch
    case {".wav", ".wave"}
      delimiter = NaN;
      header_rows = 0;
      [output.data, output.fs] = wavread (fname);
    otherwise
      ## Assume the file is in ASCII format.
      [output, delimiter, header_rows] = ...
        importdata_ascii (fname, delimiter, header_rows);
  endswitch

  ## If there are any empty fields in the output structure, then remove them
  if (isstruct (output) && numel (output) == 1)
    fields = fieldnames (output);
    for i=1:length (fields)
      if (isempty (output.(fields{i})))
        output = rmfield (output, fields{i});
      endif
    endfor

    ## If only one field is left, replace the structure with the field,
    ## i.e., output = output.onlyFieldLeft

    ## Update the list of fields
    if (numfields (output) == 1)
      output = output.(fields{1});
    endif
  endif

endfunction

function [output, delimiter, header_rows] = importdata_ascii (fname, delimiter, num_header_rows)

  ## Define fields in the output structure so that the order will be correct.
  output.data       = [];
  output.textdata   = {};
  output.rowheaders = {};
  output.colheaders = {};

  [fid, msg] = fopen (fname, "r");
  if (fid == -1)
    error (msg);
  endif

  header_rows = 0;
  header_cols = 0;

  ## Work through first few rows line by line until a delimiter is found.
  while (ischar (row = fgetl (fid)))

    ## If no delimiter determined yet, make a guess.
    if (isempty (delimiter))
      ## This pattern can be fooled, but mostly does the job just fine.
      delim = regexpi (row, '[-+\d.e*ij ]+([^-+\de.ij])[-+\de*.ij ]',
                      'tokens', 'once');
      #delim = regexp (row, '[+-\d.eE\*ij ]+([^+-\d.ij])[+-\d.ij]',
      #                     'tokens', 'once');
      if (! isempty (delim))
        delimiter = delim{1};
      endif
    endif

    if (delimiter == " ")
      row_entries = regexp (strtrim (row), ' +', 'split');
    else
      row_entries = ostrsplit (row, delimiter);
    endif
    row_data = str2double (row_entries);
    if (all (isnan (row_data)) || header_rows < num_header_rows)
      header_rows++;
      output.textdata{end+1, 1} = row;
    else
      if (! isempty (output.textdata))
        if (delimiter == " ")
          output.colheaders = regexp (strtrim (output.textdata{end}),
                                      ' +', 'split');
        else
          output.colheaders = ostrsplit (output.textdata{end}, delimiter);
        endif
      endif
      header_cols = find (! isnan (row_data), 1) - 1;
      ## The number of header rows and header columns is now known.
      break;
    endif

  endwhile

  if (row == -1)
    ## No numeric data found => return file as cellstr array
    ## 1. Read as char string
    fseek (fid, 0, "bof");
    output = fread (fid, Inf, "*char")';
    fclose (fid);
    ## 2. Find EOL type
    idx = find (output(1:min (4096, length (output))) == "\n", 1) - 1;
    if (isindex (idx) && output(idx) == "\r")
      dlm = "\r\n";
    else
      dlm = "\n";
    endif
    ## 3. Split each line into a cell (column vector)
    output = strsplit (output, dlm)';
    ## 4. Remove last cell (for files with -proper- EOL before EOF)
    if (isempty (output{end}))
      output(end) = [];
    endif
    ## 5. Return after setting some output data
    delimiter = "";
    header_rows = numel (output);
    return;
  else
    fclose (fid);
  endif

  if (num_header_rows >= 0)
    header_rows = num_header_rows;
  endif

  ## Now, let the efficient built-in routine do the bulk of the work.
  if (delimiter == " ")
    output.data = dlmread (fname, "", header_rows, header_cols,
                           "emptyvalue", NA);
  else
    output.data = dlmread (fname, delimiter, header_rows, header_cols,
                           "emptyvalue", NA);
  endif

  ## Go back and correct any individual values that did not convert.
  na_idx = isna (output.data);
  if (header_cols > 0)
    na_idx = [(true (rows (na_idx), header_cols)), na_idx];
  endif
  if (any (na_idx(:)))

    file_content = ostrsplit (fileread (fname), "\n");

    na_rows = find (any (na_idx, 2));
    for ridx = na_rows(:)'
      row = file_content{ridx+header_rows};
      if (delimiter == " ")
        fields = regexp (strtrim (row), ' +', 'split');
      else
        fields = ostrsplit (row, delimiter);
      endif

      text = fields(na_idx(ridx,:));
      text = text(! strcmpi (text, "NA"));  #  Remove valid "NA" entries
      if (! isempty (text))
        output.textdata(end+1:end+numel (text), 1) = text;
      endif
      if (header_cols)
        output.rowheaders(end+1, :) = fields(1:header_cols);
      endif
    endfor

  endif

  ## Final cleanup to satisfy output configuration
  if (all (cellfun ("isempty", output.textdata)))
    output = output.data;
  elseif (! isempty (output.rowheaders) && ! isempty (output.colheaders))
    output = struct ("data", {output.data}, "textdata", {output.textdata});
  endif

endfunction


%!test
%! ## Comma separated values
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1,-7.2,0\n0.012,6.5,128");
%! fclose (fid);
%! [a1,d1,h1] = importdata (fn, ",");
%! [a2,d2,h2] = importdata (fn);
%! unlink (fn);
%! assert (a1, A);
%! assert (d1, ",");
%! assert (h1, 0);
%! assert (a2, A);
%! assert (d2, ",");
%! assert (h2, 0);

%!test
%! ## Tab separated values
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t-7.2\t0\n0.012\t6.5\t128");
%! fclose (fid);
%! [a1,d1,h1] = importdata (fn, "\t");
%! [a2,d2,h2] = importdata (fn);
%! unlink (fn);
%! assert (a1, A);
%! assert (d1, "\t");
%! assert (h1, 0);
%! assert (a2, A);
%! assert (d2, "\t");
%! assert (h2, 0);

%!test
%! ## Space separated values, using multiple spaces to align in columns.
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%10.3f %10.3f %10.3f\n", A');
%! fclose (fid);
%! [a1,d1,h1] = importdata (fn, " ");
%! [a2,d2,h2] = importdata (fn);
%! unlink (fn);
%! assert (a1, A);
%! assert (d1, " ");
%! assert (h1, 0);
%! assert (a2, A);
%! assert (d2, " ");
%! assert (h2, 0);

%!test
%! ## No separator, 1 column of data only
%! A = [3.1;-7.2;0;0.012;6.5;128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%f\n", A);
%! fclose (fid);
%! [a1,d1,h1] = importdata (fn, "");
%! [a2,d2,h2] = importdata (fn);
%! unlink (fn);
%! assert (a1, A);
%! assert (d1, "");
%! assert (h1, 0);
%! assert (a2, A);
%! assert (d2, "");
%! assert (h2, 0);

%!test
%! ## Header text
%! A.data = [3.1 -7.2 0; 0.012 6.5 128];
%! A.textdata = {"This is a header row."; ...
%!               "this row does not contain any data, but the next one does."};
%! A.colheaders = A.textdata (2);
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%s\n", A.textdata{:});
%! fputs (fid, "3.1\t-7.2\t0\n0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 2);

%!test
%! ## Column headers, only last row is returned in colheaders
%! A.data = [3.1 -7.2 0; 0.012 6.5 128];
%! A.textdata = {"Label1\tLabel2\tLabel3";
%!               "col 1\tcol 2\tcol 3"};
%! A.colheaders = {"col 1", "col 2", "col 3"};
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%s\n", A.textdata{:});
%! fputs (fid, "3.1\t-7.2\t0\n0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 2);

%!test
%! ## Row headers
%! A.data = [3.1 -7.2 0; 0.012 6.5 128];
%! A.textdata = {"row1"; "row2"};
%! A.rowheaders = A.textdata;
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "row1\t3.1\t-7.2\t0\nrow2\t0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## Row/Column headers and Header Text
%! A.data = [3.1 -7.2 0; 0.012 6.5 128];
%! A.textdata = {"This is introductory header text"
%!               "      col1 col2 col3"
%!               "row1"
%!               "row2"};
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%s\n", A.textdata{1:2});
%! fputs (fid, "row1\t3.1\t-7.2\t0\nrow2\t0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 2);

%!test
%! ## Ignore empty rows containing only spaces
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%10.3f %10.3f %10.3f\n", A(1,:));
%! fputs (fid, "      ");
%! fprintf (fid, "%10.3f %10.3f %10.3f\n", A(2,:));
%! fclose (fid);
%! [a,d,h] = importdata (fn, " ");
%! unlink (fn);
%! assert (a, A);
%! assert (d, " ");
%! assert (h, 0);

%!test
%! ## Exponentials
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "+3.1e0\t-72E-1\t0\n12e-3\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## Complex numbers
%! A = [3.1 -7.2 0-3.4i; 0.012 -6.5+7.2i 128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t-7.2\t0-3.4i\n0.012\t-6.5+7.2i\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## Exceptional values (Inf, NaN, NA)
%! A = [3.1 Inf NA; -Inf NaN 128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\tInf\tNA\n-Inf\tNaN\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## Missing values and Text Values
%! A.data = [3.1 NA 0; 0.012 NA 128];
%! A.textdata = {char(zeros(1,0))
%!               "NO DATA"};
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t\t0\n0.012\tNO DATA\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!#test
%! ## CRLF for line breaks
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t-7.2\t0\r\n0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!#test
%! ## CR for line breaks
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t-7.2\t0\r0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## Distinguish double from complex when no delimiter is supplied (bug #43393)
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "2.0000e+02   4.0000e-04");
%! fclose (fid);
%! [a, d, h] = importdata (fn);
%! unlink (fn);
%! assert (a, [2e2, 4e-4]);
%! assert (d, " ");
%! assert (h, 0);

%!test
%! ## Only text / no numeric data; \n as EOL
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "aaaa 11\nbbbbb 22\nccccc 3\n");
%! fclose (fid);
%! [a, d, h] = importdata (fn);
%! unlink (fn);
%! assert (a, {"aaaa 11"; "bbbbb 22"; "ccccc 3"});
%! assert (d, "");
%! assert (h, 3);

%!test
%! ## Only text / no numeric data; \r\n as EOL; missing last EOL before EOF
%! fn  = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "aaaa 11\r\nbbbbb 22\r\nccccc 3");
%! fclose (fid);
%! [a, d, h] = importdata (fn);
%! unlink (fn);
%! assert (a, {"aaaa 11"; "bbbbb 22"; "ccccc 3"});
%! assert (d, "");
%! assert (h, 3);

%!error importdata ()
%!error importdata (1,2,3,4)
%!error <FNAME must be a string> importdata (1)
%!error <option -pastespecial not implemented> importdata ("-pastespecial")
%!error <DELIMITER must be a single character> importdata ("foo", 1)
%!error <DELIMITER must be a single character> importdata ("foo", "ab")
%!error <HEADER_ROWS must be an integer> importdata ("foo", " ", "1")
%!error <HEADER_ROWS must be an integer> importdata ("foo", " ", 1.5)
%!error <not implemented for file format .au> importdata ("foo.au")
%!error <not implemented for file format .avi> importdata ("foo.avi")

