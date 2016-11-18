## Copyright (C) 2009-2015 Søren Hauberg
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
## @deftypefn  {Function File} {@var{text} =} get_first_help_sentence (@var{name})
## @deftypefnx {Function File} {@var{text} =} get_first_help_sentence (@var{name}, @var{max_len})
## @deftypefnx {Function File} {[@var{text}, @var{status}] =} get_first_help_sentence (@dots{})
## Return the first sentence of a function's help text.
##
## The first sentence is defined as the text after the function declaration
## until either the first period (".") or the first appearance of two
## consecutive newlines ("\n\n").  The text is truncated to a maximum length of
## @var{max_len}, which defaults to 80.
##
## The optional output argument @var{status} returns the status reported by
## @code{makeinfo}.  If only one output argument is requested, and @var{status}
## is nonzero, a warning is displayed.
##
## As an example, the first sentence of this help text is
##
## @example
## @group
## get_first_help_sentence ("get_first_help_sentence")
## @print{} ans = Return the first sentence of a function's help text.
## @end group
## @end example
## @end deftypefn

function [text, status] = get_first_help_sentence (name, max_len = 80)
  ## Check input
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! ischar (name))
    error ("get_first_help_sentence: NAME must be a string");
  endif

  if (! isnumeric (max_len) || max_len <= 0 || max_len != fix (max_len))
    error ("get_first_help_sentence: MAX_LEN must be positive integer");
  endif

  ## First, we get the raw help text
  [help_text, format] = get_help_text (name);

  ## Then, we take action depending on the format
  switch (lower (format))
    case "plain text"
      [text, status] = first_sentence_plain_text (help_text, max_len);
    case "texinfo"
      [text, status] = first_sentence_texinfo (help_text, max_len);
    case "html"
      [text, status] = first_sentence_html (help_text, max_len);
    case "not documented"
      error ("get_first_help_sentence: '%s' is not documented\n", name);
    case "not found"
      error ("get_first_help_sentence: '%s' not found\n", name);
    otherwise
      error ("get_first_help_sentence: internal error: unsupported help text format: '%s'\n", format);
  endswitch

  if (nargout <= 1 && status != 0)
    warning ("get_first_help_sentence: couldn't run makeinfo on '%s'", name);
  endif
endfunction

## This function extracts the first sentence from a plain text help text
function [text, status] = first_sentence_plain_text (help_text, max_len)
  ## Extract first line by searching for a period followed by a space class
  ## character (to support periods in numbers or words) ...
  period_idx = regexp (help_text, '\.\s', "once");
  ## ... or a double end-of-line (we subtract 1 because we are not interested
  ## in capturing the first newline).
  line_end_idx = regexp (help_text, "\n\n", "once") - 1;
  text = help_text (1:min ([period_idx; line_end_idx; max_len; length(help_text)]));
  status = 0;
endfunction

## This function extracts the first sentence from a Texinfo help text.
## The function works by removing @def* from the texinfo text. After this, we
## render the text to plain text using makeinfo, and then extract the first
## line.
function [text, status] = first_sentence_texinfo (help_text, max_len)
  ## Lines ending with "@\n" are continuation lines, so they should be
  ## concatenated with the following line.
  help_text = strrep (help_text, "@\n", " ");

  ## Find, and remove, lines that start with @def. This should remove things
  ## such as @deftypefn, @deftypefnx, @defvar, etc.
  keep = true (size (help_text));
  def_idx = strfind (help_text, "@def");
  if (! isempty (def_idx))
    endl_idx = find (help_text == "\n");
    for k = 1:length (def_idx)
      endl = endl_idx(find (endl_idx > def_idx(k), 1));
      if (isempty (endl))
        endl = numel (keep);
      endif
      keep(def_idx(k):endl) = false;
    endfor

    ## Remove the @end ... that corresponds to the @def we removed above
    def1 = def_idx(1);
    space_idx = find (help_text == " ");
    space_idx = space_idx (find (space_idx > def1, 1));
    bracket_idx = find (help_text == "{" | help_text == "}");
    bracket_idx = bracket_idx (find (bracket_idx > def1, 1));
    if (isempty (space_idx) && isempty (bracket_idx))
      error ("get_first_help_sentence: couldn't parse texinfo");
    endif
    sep_idx = min (space_idx, bracket_idx);
    def_type = help_text(def1+1:sep_idx-1);

    end_idx = strfind (help_text, sprintf ("@end %s", def_type));
    if (isempty (end_idx))
      error ("get_first_help_sentence: couldn't parse texinfo");
    endif
    keep(end_idx(1):end) = false;

    help_text = help_text(keep);
  endif

  ## Run makeinfo to generate plain text
  [help_text, status] = __makeinfo__ (help_text, "plain text");

  ## Extract first line with plain text method.
  text = first_sentence_plain_text (help_text, max_len);
endfunction

## This function extracts the first sentence from a html help text.
## The function simply removes the tags and treats the text as plain text.
function [text, status] = first_sentence_html (help_text, max_len)
  ## Strip tags
  [help_text, status] = strip_html_tags (help_text);

  ## Extract first line with plain text method.
  text = first_sentence_plain_text (help_text, max_len);
endfunction


%!assert (get_first_help_sentence ('get_first_help_sentence'), ...
%!        "Return the first sentence of a function's help text.")

## Test input validation
%!error get_first_help_sentence ()
%!error get_first_help_sentence (1, 2, 3)
%!error <NAME must be a string> get_first_help_sentence (1)
%!error <MAX_LEN must be positive integer> get_first_help_sentence ("ls", "a")
%!error <MAX_LEN must be positive integer> get_first_help_sentence ("ls", 0)
%!error <MAX_LEN must be positive integer> get_first_help_sentence ("ls", 80.1)

