## Copyright (C) 2009-2015 John W. Eaton
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

args = argv ();

if (nargin < 2)
  error ("usage: mk_doc_cache OUTPUT-FILE DOCSTRINGS-FILE ...");
endif

output_file = args{1};
docstrings_files = args(2:end);

## Special character used as break between DOCSTRINGS
doc_delim = char (0x1d);

## Read the contents of all the DOCSTRINGS files into TEXT.
## It is more efficient to fork to shell for makeinfo only once on large data

nfiles = numel (docstrings_files);
text = cell (1, nfiles);
for i = 1:nfiles
  file = docstrings_files{i};
  fid = fopen (file, "r");
  if (fid < 0)
    error ("unable to open %s for reading", file);
  else
    tmp = fread (fid, Inf, "*char")';
    if (isempty (strfind (tmp, doc_delim)))
      ## No delimiter, copy verbatim (this is the case for the file
      ## containing macro definitions, for example).
      text{i} = tmp;
    else
      ## Strip off header lines
      [~, text{i}] = strtok (tmp, doc_delim);
    endif
  endif
endfor
text = [text{:}];

## Strip Texinfo marker
text = regexprep (text, "-\\*- texinfo -\\*-[ \t]*[\r\n]*", "");

## Add keywords and operators
other_docstrings = [__keywords__; __operators__];
for i = 1 : numel (other_docstrings)
  name = other_docstrings{i};
  ## Special handling of block comment operators such as '#{'
  esc_name = regexprep (name, '([{}])', '@$1');
  text = [text doc_delim esc_name get_help_text(name) "\n"];
endfor
text(end+1) = doc_delim;

## Double '@' symbol for Texinfo
text = strrep (text, [doc_delim "@"], [doc_delim "@@"]);

## Write data to temporary file for input to makeinfo
[fid, name, msg] = mkstemp ("octave_doc_XXXXXX", true);
if (fid < 0)
  error ("%s: %s\n", name, msg);
endif
fwrite (fid, text, "char");
fclose (fid);

cmd = [makeinfo_program() " --no-headers --no-warn --force --no-validate --fill-column=1024 " name];

[status, formatted_text] = system (cmd);

## Did we get the help text?
if (status != 0)
  error ("makeinfo failed with exit status %d!", status);
elseif (isempty (formatted_text))
  error ("makeinfo produced no output!");
endif

## Break apart output and store in cache variable
delim_idx = find (formatted_text == doc_delim);
n = length (delim_idx);

cache = cell (3, n);    # pre-allocate storage for efficiency
k = 1;

for i = 2:n

  block = formatted_text(delim_idx(i-1)+1:delim_idx(i)-1);

  [symbol, doc] = strtok (block, "\r\n");

  ## Skip internal functions that start with __ as these aren't
  ## indexed by lookfor.
  if (length (symbol) > 2 && regexp (symbol, '^__.+__$'))
    continue;
  endif

  doc = regexprep (doc, "^[\r\n]+", '', 'once');

  if (isempty (doc))
    continue;
  endif

  tmp = regexprep (doc, "^ -- .*$[\r\n]", '', 'lineanchors', 'dotexceptnewline');

  if (isempty (tmp))
    continue;
  endif

  end_of_first_sentence = regexp (tmp, "(\\.|[\r\n][\r\n])", "once");
  if (isempty (end_of_first_sentence))
    end_of_first_sentence = length (tmp);
  endif

  first_sentence = tmp(1:end_of_first_sentence);
  first_sentence = regexprep (first_sentence, "([\r\n]| {2,})", " ");
  first_sentence = regexprep (first_sentence, '^ +', "", 'once');

  cache{1,k} = symbol;
  cache{2,k} = doc;
  cache{3,k} = first_sentence;
  k++;
endfor

cache(:,k:end) = [];    # delete unused pre-allocated entries

save_header_format_string (["# doc-cache created by Octave " OCTAVE_VERSION]);
save ("-text", output_file, "cache");
