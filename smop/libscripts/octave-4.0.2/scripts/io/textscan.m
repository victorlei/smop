## Copyright (C) 2010-2014 Ben Abbott
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
## @deftypefn  {Function File} {@var{C} =} textscan (@var{fid}, @var{format})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{fid}, @var{format}, @var{n})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{fid}, @var{format}, @var{param}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{fid}, @var{format}, @var{n}, @var{param}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{str}, @dots{})
## @deftypefnx {Function File} {[@var{C}, @var{position}] =} textscan (@var{fid}, @dots{})
## Read data from a text file or string.
##
## The string @var{str} or file associated with @var{fid} is read from and
## parsed according to @var{format}.  The function behaves like @code{strread}
## except it can also read from file instead of a string.  See the documentation
## of @code{strread} for details.
##
## In addition to the options supported by @code{strread}, this function
## supports a few more:
##
## @itemize
## @item @qcode{"collectoutput"}:
## A value of 1 or true instructs textscan to concatenate consecutive columns
## of the same class in the output cell array.  A value of 0 or false (default)
## leaves output in distinct columns.
##
## @item @qcode{"endofline"}:
## Specify @qcode{"@xbackslashchar{}r"}, @qcode{"@xbackslashchar{}n"} or
## @qcode{"@xbackslashchar{}r@xbackslashchar{}n"} (for CR, LF, or CRLF).  If no
## value is given, it will be inferred from the file.  If set to "" (empty
## string) EOLs are ignored as delimiters and added to whitespace.
##
## @item @qcode{"headerlines"}:
## The first @var{value} number of lines of @var{fid} are skipped.
##
## @item @qcode{"returnonerror"}:
## If set to numerical 1 or true (default), return normally when read errors
## have been encountered.  If set to 0 or false, return an error and no data.
## As the string or file is read by columns rather than by rows, and because
## textscan is fairly forgiving as regards read errors, setting this option
## may have little or no actual effect.
## @end itemize
##
## When reading from a character string, optional input argument @var{n}
## specifies the number of times @var{format} should be used (i.e., to limit
## the amount of data read).
## When reading from file, @var{n} specifies the number of data lines to read;
## in this sense it differs slightly from the format repeat count in strread.
##
## The output @var{C} is a cell array whose second dimension is determined
## by the number of format specifiers.
##
## The second output, @var{position}, provides the position, in characters,
## from the beginning of the file.
##
## If the format string is empty (not: omitted) and the file contains only
## numeric data (excluding headerlines), textscan will return data in a number
## of columns matching the number of numeric fields on the first data line of
## the file.
##
## @seealso{dlmread, fscanf, load, strread, textread}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>

function [C, position] = textscan (fid, format = "%f", varargin)

  BUFLENGTH = 4096;               # Read buffer
  emptfmt = 0;                    # Signals deliberately empty format string

  ## Check input
  if (nargin < 1)
    print_usage ();
  endif

  if (isempty (format))
    emptfmt = 1;
    format = "%f";
  endif

  if (! ischar (format))
    error ("textscan: FORMAT must be a string");
  endif

  ## Determine the number of data fields & initialize output array
  num_fields = numel (regexp (format, '(%(\d*|\d*\.\d*)?[nfduscq]|%\[)', "match"));
  if (! num_fields)
    error ("textscan.m: no valid format conversion specifiers found\n");
  endif
  C = cell (1, num_fields);

  if (! (isa (fid, "double") && fid > 0) && ! ischar (fid))
    error ("textscan: first argument must be a file id or character string");
  endif

  args = varargin;
  if (nargin > 2 && isnumeric (args{1}))
    nlines = args{1};
  else
    nlines = Inf;
  endif
  if (nlines < 1)
    printf ("textscan: N = 0, no data read\n");
    return;  endif

  if (! any (strcmpi (args, "emptyvalue")))
    ## Matlab returns NaNs for missing values
    args(end+1:end+2) = {'emptyvalue', NaN};
  endif

  ## Check default parameter values that differ for strread & textread

  ipos = find (strcmpi (args, "whitespace"));
  if (isempty (ipos))
    ## Matlab default whitespace = " \b\t"
    args(end+1:end+2) = {'whitespace', " \b\t"};
    whitespace = " \b\t";
  else
    ## Check if there's at least one string format specifier
    has_str_fmt = regexp (format, '%[*]?\d*s', "once");
    ## If there is a string format AND whitespace value = empty,
    ## don't add a space (char(32)) to whitespace
    if (! (isempty (args{ipos+1}) && has_str_fmt))
      args{ipos+1} = unique ([" ", args{ipos+1}]);
    endif
    whitespace = args{ipos+1};
  endif

  if (! any (strcmpi (args, "delimiter")))
    ## Matlab says default delimiter = whitespace.
    ## strread() will pick this up further
    args(end+1:end+2) = {'delimiter', ""};
    delimiter = "";
  else
    delimiter = args{find (strcmpi (args, "delimiter")) + 1};
  endif

  collop = false;
  ipos = find (strcmpi (args, "collectoutput"));
  if (! isempty (ipos))
    ## Search & concatenate consecutive columns of same class requested
    if (isscalar (args{ipos+1})
        && (islogical (args{ipos+1}) || isnumeric (args{ipos+1})))
      collop = args{ipos+1};
    else
      warning ("textscan: illegal value for CollectOutput parameter - ignored");
    endif
    ## Remove argument before call to strread() below
    args(ipos:ipos+1) = [];
  endif

  if (any (strcmpi (args, "returnonerror")))
    ## Because of the way strread() reads data (columnwise) this parameter
    ## can't be neatly implemented.  strread() will pick it up anyway
    warning ('textscan: ReturnOnError is not fully implemented');
  else
    ## Set default value (=true)
    args(end+1:end+2) = {"returnonerror", 1};
  endif

  ## Check if a headerlines argument is specified
  headerlines = find (strcmpi (args, "headerlines"), 1);
  if (! isempty (headerlines))
    ## Yep. But it is stray when reading from strings...
    if (ischar (fid))
      warning ("textscan: 'headerlines' ignored when reading from strings");
    endif
  endif

  if (ischar (fid))
    ## Read from a text string
    if (nargout == 2)
      error ("textscan: cannot provide position information for character input");
    endif
    str = fid;
  else
    st_pos = ftell (fid);
    ## Skip header lines if requested
    if (! isempty (headerlines))
      ## Beware of missing or wrong headerline value
      if (headerlines  == numel (args)
         || ! isnumeric (args{headerlines + 1}))
        error ("Missing or illegal value for 'headerlines'" );
      endif
      ## Avoid conveying floats to fskipl
      args{headerlines + 1} = round (args{headerlines + 1});
      if (args{headerlines + 1} > 0)
        ## Beware of zero valued headerline, fskipl would skip to EOF
        fskipl (fid, args{headerlines + 1});
        st_pos = ftell (fid);
      elseif (args{headerlines + 1} < 0)
        warning ("textscan.m: negative headerline value ignored");
      endif
      args(headerlines:headerlines+1) = [];
    endif
    ## Read a first file chunk. Rest follows after endofline processing
    [str, count] = fscanf (fid, "%c", BUFLENGTH);

  endif

  ## Check for empty result
  if (isempty (str))
    warning ("textscan: no data read");
    return;
  endif

  ## Check value of 'endofline'.  String or file doesn't seem to matter
  endofline = find (strcmpi (args, "endofline"), 1);
  if (! isempty (endofline))
    if (ischar (args{endofline + 1}))
      eol_char = args{endofline + 1};
      if (strcmp (typeinfo (eol_char), "sq_string"))
        eol_char = do_string_escapes (eol_char);
      endif
      if (! any (strcmp (eol_char, {"", "\n", "\r", "\r\n"})))
        error ("textscan: illegal EndOfLine character value specified");
      endif
    else
      error ("textscan: character value required for EndOfLine");
    endif
  else
    if (! ischar (fid))
    ## Determine EOL from file.
    ## Search for EOL candidates in the first BUFLENGTH chars
    eol_srch_len = min (length (str), BUFLENGTH);
    ## First try DOS (CRLF)
    if (! isempty (strfind (str(1 : eol_srch_len), "\r\n")))
      eol_char = "\r\n";
    ## Perhaps old Macintosh? (CR)
    elseif (! isempty (strfind (str(1 : eol_srch_len), "\r")))
      eol_char = "\r";
    ## Otherwise, use plain UNIX (LF)
    else
      eol_char = "\n";
    endif
    else
      eol_char = "\n";
    endif
    ## Set up the default endofline param value
    args(end+1:end+2) = {"endofline", eol_char};
  endif

  if (! ischar (fid))
    ## Now that we know what EOL looks like, we can process format_repeat_count.
    ## FIXME The below isn't ML-compatible: counts lines, not format string uses
    if (isfinite (nlines) && (nlines >= 0))
      l_eol_char = length (eol_char);
      eoi = findstr (str, eol_char);
      n_eoi = length (eoi);
      nblks = 0;
      ## Avoid slow repeated str concatenation, first seek requested end of data
      while (n_eoi < nlines && count == BUFLENGTH)
        [nstr, count] = fscanf (fid, "%c", BUFLENGTH);
        if (count > 0)
          ## Watch out for multichar EOL being missed across buffer boundaries
          if (l_eol_char > 1)
            str = [str(end - length (eol_char) + 2 : end) nstr];
          else
            str = nstr;
          endif
          eoi = findstr (str, eol_char);
          n_eoi += numel (eoi);
          ++nblks;
        endif
      endwhile
      ## Handle case of missing trailing EOL
      if (! strcmp (str(end - length (eol_char) + 1 : end), eol_char))
        eoi = [ eoi (length (str)) ];
        ++n_eoi;
      endif
      ## OK, found EOL delimiting last requested line. Compute ptr (incl. EOL)
      if (isempty (eoi))
        disp ("textscan: format repeat count specified but no endofline found");
        data_size = nblks * BUFLENGTH + count;
      else
        ## Compute data size to read incl complete EOL
        data_size = (nblks * BUFLENGTH) ...
                    + eoi(end + min (nlines, n_eoi) - n_eoi) ...
                    + l_eol_char - 1;
      endif
      fseek (fid, st_pos, "bof");
      str = fscanf (fid, "%c", data_size);
      args{1} = Inf;
    else
      fseek (fid, st_pos, "bof");
      str = fread (fid, "char=>char").';
    endif
  endif

  ## Strip trailing EOL to avoid returning stray missing values (f. strread).
  ## However, in case of CollectOutput request, presence of EOL is required;
  ## also in case of deliberately entered empty format string
  eol_at_end = strcmp (str(end-length (eol_char) + 1 : end), eol_char);
  if (collop || emptfmt)
    if (! eol_at_end)
      str(end+1 : end+length (eol_char)) = eol_char;
    endif
  elseif (eol_at_end)
     str(end-length (eol_char) + 1 : end) = "";
    ## A corner case: str may now be empty....
    if (isempty (str)); return; endif
   endif

  ## Call strread to make it do the real work
  C = cell (1, num_fields);
  [C{:}] = strread (str, format, args{:});

  ## I.c.o. empty format, match nr. of cols to nr. of fields on first read line
  if (emptfmt)
    ## Find end of first line
    eoi = index (str, eol_char);
    if (eoi)
      ## str contains an EOL, proceed with assessing nr. of columns
      ncols = countcols (C, str(1 : eoi-1), delimiter, whitespace);
      ## See if lowermost data row must be completed
      pad = mod (numel (C{1}), ncols);
      if (pad)
        ## Pad output with emptyvalues (rest has been done by stread.m)
        emptv = find (strcmpi (args, "emptyvalue"));
        if (isempty (emptv))
          ## By default textscan returns NaNs for empty fields
          C(1) = [C{1}; NaN(ncols - pad, 1)];
        else
          ## Otherwise return supplied emptyvalue. Pick last occurrence
          C(1) = [C{1}; repmat(args{emptv(end)+1}, ncols - pad, 1)];
        endif
      endif
      ## Compute nr. of rows
      nrows = floor (numel (C{1}) / ncols);
      ## Reshape C; watch out, transpose needed
      C(1) = reshape (C{1}, ncols, numel (C{1}) / ncols).';
      ## Distribute columns over C and wipe cols 2:end of C{1}
      for ii=2:ncols
        C(ii) = C{1}(:, ii);
      endfor
      C{1} = C{1}(:, 1);
    endif
  endif

  ## If requested, collect output columns of same class
  if (collop)
    C = colloutp (C);
  endif

  if (nargout == 2)
    ## Remember file position (persistent var)
    position = ftell (fid);
  endif

endfunction


## Assess nr of data fields on first line of data
function ncols = countcols (C, str, dlm, wsp)

  if (isempty (dlm))
    ## Field separator = whitespace. Fold multiple whitespace into one
    str = regexprep (str, sprintf ("[%s]", wsp), " ");
    str = strtrim (str);
    ncols = numel (strfind (str, " ")) + 1;
  else
    ncols = numel (regexp (str, sprintf ("[%s]", dlm))) + 1;
  endif

endfunction


## Collect consecutive columns of same class into one cell column
function C = colloutp (C)

  ## Start at rightmost column and work backwards to avoid ptr mixup
  ii = numel (C);
  while (ii > 1)
    clss1 = class (C{ii});
    jj = ii;
    while (jj > 1 && strcmp (clss1, class (C{jj - 1})))
      ## Column to the left is still same class; check next column to the left
      --jj;
    endwhile
    if (jj < ii)
      ## Concatenate columns into current column
      C{jj} = [C{jj : ii}];
      ## Wipe concatenated columns to the right, resume search to the left
      C(jj+1 : ii) = [];
      ii = jj - 1;
    else
      ## No similar class in column to the left, search from there
      --ii;
    endif
  endwhile

endfunction


%!test
%! str = "1,  2,  3,  4\n 5,  ,  ,  8\n 9, 10, 11, 12";
%! fmtstr = "%f %d %f %s";
%! c = textscan (str, fmtstr, 2, "delimiter", ",", "emptyvalue", -Inf);
%! assert (isequal (c{1}, [1;5]));
%! assert (length (c{1}), 2);
%! assert (iscellstr (c{4}));
%! assert (isequal (c{3}, [3; -Inf]));

%!test
%! b = [10:10:100];
%! b = [b; 8*b/5];
%! str = sprintf ("%g miles/hr = %g kilometers/hr\n", b);
%! fmt = "%f miles/hr = %f kilometers/hr";
%! c = textscan (str, fmt);
%! assert (b(1,:)', c{1}, 1e-5);
%! assert (b(2,:)', c{2}, 1e-5);

%!test
%! str = "13, 72, NA, str1, 25\r\n// Middle line\r\n36, na, 05, str3, 6";
%! a = textscan (str, "%d %n %f %s %n", "delimiter", ",","treatAsEmpty", {"NA", "na"},"commentStyle", "//");
%! assert (a{1}, int32 ([13; 36]));
%! assert (a{2}, [72; NaN]);
%! assert (a{3}, [NaN; 5]);
%! assert (a{4}, {"str1"; "str3"});
%! assert (a{5}, [25; 6]);

%!test
%! str = "Km:10 = hhhBjjj miles16hour\r\n";
%! str = [str "Km:15 = hhhJjjj miles241hour\r\n"];
%! str = [str "Km:2 = hhhRjjj miles3hour\r\n"];
%! str = [str "Km:25 = hhhZ\r\n"];
%! fmt = "Km:%d = hhh%1sjjj miles%dhour";
%! a = textscan (str, fmt, "delimiter", " ");
%! assert (a{1}', int32 ([10 15 2 25]));
%! assert (a{2}', {'B' 'J' 'R' 'Z'});
%! assert (a{3}', int32 ([16 241 3 0]));

## Test with default endofline parameter
%!test
%! c = textscan ("L1\nL2", "%s");
%! assert (c{:}, {"L1"; "L2"});

## Test with endofline parameter set to "" (empty) - newline should be in word
%!test
%! c = textscan ("L1\nL2", "%s", "endofline", "");
%! assert (int8 (c{:}{:}), int8 ([ 76,  49,  10,  76,  50 ]));

%!test
%! ## No delimiters at all besides EOL.  Skip fields, even empty fields
%! str = "Text1Text2Text\nTextText4Text\nText57Text";
%! c = textscan (str, "Text%*dText%dText");
%! assert (c{1}, int32 ([2; 4; 0]));

## CollectOutput test
%!test
%! b = [10:10:100];
%! b = [b; 8*b/5; 8*b*1000/5];
%! str = sprintf ("%g miles/hr = %g (%g) kilometers (meters)/hr\n", b);
%! fmt = "%f miles%s %s %f (%f) kilometers %*s";
%! c = textscan (str, fmt, "collectoutput", 1);
%! assert (size (c{3}), [10, 2]);
%! assert (size (c{2}), [10, 2]);

## CollectOutput test with uneven column length files
%!test
%! b = [10:10:100];
%! b = [b; 8*b/5; 8*b*1000/5];
%! str = sprintf ("%g miles/hr = %g (%g) kilometers (meters)/hr\n", b);
%! str = [str "110 miles/hr"];
%! fmt = "%f miles%s %s %f (%f) kilometers %*s";
%! c = textscan (str, fmt, "collectoutput", 1);
%! assert (size (c{1}), [11, 1]);
%! assert (size (c{3}), [11, 2]);
%! assert (size (c{2}), [11, 2]);
%! assert (c{3}(end), NaN);
%! assert (c{2}{11, 1}, "/hr");
%! assert (isempty (c{2}{11, 2}), true);

## Test input validation
%!error textscan ()
%!error textscan (single (4))
%!error textscan ({4})
%!error <must be a string> textscan ("Hello World", 2)
%!error <cannot provide position information> [C, pos] = textscan ("Hello World")
%!error <character value required> textscan ("Hello World", '%s', 'EndOfLine', 3)

%! Test incomplete first data line
%! R = textscan (['Empty1' char(10)], 'Empty%d %f');
%! assert (R{1}, int32 (1));
%! assert (isempty (R{2}), true);

## bug #37023 (actually a strread test)
%!test
%! data = textscan("   1. 1 \n 2 3\n", '%f %f');
%! assert (data{1}, [1; 2], 1e-15);
%! assert (data{2}, [1; 3], 1e-15);

## Whitespace test (bug #37333) using delimiter ";"
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = "C:/code/meas;";
%! tc{1, end+1} = " C:/code/sim;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";");
%! for k = 1:numel (c{1})
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! end

## Whitespace test (bug #37333), adding multipleDelimsAsOne true arg
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "multipleDelimsAsOne", 1);
%! for k = 1:numel (c{1})
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! end

## Whitespace test (bug #37333), adding multipleDelimsAsOne false arg
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;;";
%! tc{1, end+1} = "";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "multipleDelimsAsOne", 0);
%! for k = 1:numel (c{1})
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! end

## Whitespace test (bug #37333) whitespace "" arg
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "whitespace", "");
%! for k = 1:numel (c{1})
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   assert (strcmp (lh, rh));
%! end

## Whitespace test (bug #37333), whitespace " " arg
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "whitespace", " ");
%! for k = 1:numel (c{1})
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! end

## Test reading from a real file
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! d = rand (1, 4);
%! fprintf (fid, "  %f %f   %f  %f ", d);
%! fseek (fid, 0, "bof");
%! A = textscan (fid, "%f %f");
%! fclose (fid);
%! unlink (f);
%! assert (A{1}, [d(1); d(3)], 1e-6);
%! assert (A{2}, [d(2); d(4)], 1e-6);

## Tests reading with empty format, should return proper nr of columns
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, " 1 2 3 4\n5 6 7 8");
%! fseek (fid, 0, "bof");
%! A = textscan (fid, "");
%! fclose (fid);
%! unlink (f);
%! assert (A{1}, [1 ; 5], 1e-6);
%! assert (A{2}, [2 ; 6], 1e-6);
%! assert (A{3}, [3 ; 7], 1e-6);
%! assert (A{4}, [4 ; 8], 1e-6);

## Tests reading with empty format; empty fields & incomplete lower row
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, " ,2,,4\n5,6");
%! fseek (fid, 0, "bof");
%! A = textscan (fid, "", "delimiter", ",", "EmptyValue", 999, "CollectOutput" , 1);
%! fclose (fid);
%! unlink (f);
%! assert (A{1}, [999, 2, 999, 4; 5, 6, 999, 999], 1e-6);

## Error message tests

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! msg1 = "Missing or illegal value for 'headerlines'";
%! try
%! A = textscan (fid, "", "headerlines");
%! end_try_catch;
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! msg1 = "Missing or illegal value for 'headerlines'";
%! try
%! A = textscan (fid, "", "headerlines", "hh");
%! end_try_catch;
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"some_string");
%! fseek (fid, 0, "bof");
%! msg1 = "textscan: illegal EndOfLine character value specified";
%! try
%! A = textscan (fid, "%f", "EndOfLine", "\n\r");
%! end_try_catch;
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"some_string");
%! fseek (fid, 0, "bof");
%! msg1 = "textscan: character value required for EndOfLine";
%! try
%! A = textscan (fid, "%f", "EndOfLine", 33);
%! end_try_catch;
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

## Bug #41824
%!test
%! assert (textscan ("123", "", "whitespace", " "){:}, 123);

## Bug #42343-1, just test supplied emptyvalue (actually done by strread.m)
%!test
%! assert (textscan (",NaN", "", "delimiter", "," ,"emptyValue" ,Inf), {Inf, NaN});

## Bug #42343-2, test padding with supplied emptyvalue (done by textscan.m)
%!test
%! a = textscan (",1,,4\nInf,  ,NaN", "", "delimiter", ",", "emptyvalue", -10);
%! assert (cell2mat (a), [-10, 1, -10, 4; Inf, -10, NaN, -10]);

## Bug #42528
%!test
%! assert (textscan ("1i", ""){1},  0+1i);
%! assert (cell2mat (textscan ("3, 2-4i, NaN\n -i, 1, 23.4+2.2i", "")), [3+0i, 2-4i, NaN+0i; 0-i,  1+0i, 23.4+2.2i]);

## Illegal format specifiers
%!test
%!error <no valid format conversion specifiers> textscan ("1.0", "%z");

## Properly process single-quoted EOL args (bug #46477)
%!test
%! C = textscan ("hello, world!", "%s%s", "endofline", '\n');
%! assert (C{1}{1}, "hello,");
%! assert (C{2}{1}, "world!");

