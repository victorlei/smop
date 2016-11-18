/*

Copyright (C) 2005-2015 David Bateman
Copyright (C) 2002-2005 Paul Kienzle

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <list>
#include <sstream>

#include <pcre.h>

#include "base-list.h"
#include "oct-locbuf.h"
#include "quit.h"
#include "lo-regexp.h"
#include "str-vec.h"

#include "defun.h"
#include "Cell.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "utils.h"

// Replace backslash escapes in a string with the real values.  We need
// two special functions instead of the one in utils.cc because the set
// of escape sequences used for regexp patterns and replacement strings
// is different from those used in the *printf functions.

static std::string
do_regexp_ptn_string_escapes (const std::string& s)
{
  std::string retval;

  size_t i = 0;
  size_t j = 0;
  size_t len = s.length ();

  retval.resize (len);

  while (j < len)
    {
      if (s[j] == '\\' && j+1 < len)
        {
          switch (s[++j])
            {
            case 'b': // backspace
              retval[i] = '\b';
              break;

            // Translate \< and \> to PCRE word boundary
            case '<': // begin word boundary
            case '>': // end word boundary
              retval[i] = '\\';
              retval[++i] = 'b';
              break;

#if 0
// FIXME: To be complete, we need to handle \oN, \o{N}.
//        The PCRE library already handles \N where N
//        is an octal number.  New code needs to merely
//        replace \oN or \o{N} with \N.
            case 'o': // octal number
#endif

            default:  // pass escape sequence through
              retval[i] = '\\';
              retval[++i] = s[j];
              break;
            }
        }
      else
        {
          retval[i] = s[j];
        }

      i++;
      j++;
    }

  retval.resize (i);

  return retval;
}

static std::string
do_regexp_rep_string_escapes (const std::string& s)
{
  std::string retval;

  size_t i = 0;
  size_t j = 0;
  size_t len = s.length ();

  retval.resize (len);

  while (j < len)
    {
      if (s[j] == '\\' && j+1 < len)
        {
          switch (s[++j])
            {
            case 'a': // alarm
              retval[i] = '\a';
              break;

            case 'b': // backspace
              retval[i] = '\b';
              break;

            case 'f': // formfeed
              retval[i] = '\f';
              break;

            case 'n': // newline
              retval[i] = '\n';
              break;

            case 'r': // carriage return
              retval[i] = '\r';
              break;

            case 't': // horizontal tab
              retval[i] = '\t';
              break;

            case 'v': // vertical tab
              retval[i] = '\v';
              break;

#if 0
// FIXME: to be complete, we need to handle \oN, \o{N}, \xN, and
// \x{N}.  Hex digits may be upper or lower case.  Brackets are
// optional, so \x5Bz is the same as \x{5B}z.

            case 'o': // octal number
            case 'x': // hex number
#endif

            default:  // pass escape sequence through
              retval[i] = '\\';
              retval[++i] = s[j];
              break;
            }
        }
      else
        {
          retval[i] = s[j];
        }

      i++;
      j++;
    }

  retval.resize (i);

  return retval;
}

static void
parse_options (regexp::opts& options, const octave_value_list& args,
               const std::string& who, int skip, bool& extra_args)
{
  int nargin = args.length ();

  extra_args = false;

  for (int i = skip; i < nargin; i++)
    {
      std::string str;

      if (args(i).is_string ())
        str = args(i).string_value ();
      else
        {
          error ("%s: optional arguments must be strings", who.c_str ());
          break;
        }

      std::transform (str.begin (), str.end (), str.begin (), tolower);

      if (str.find ("once", 0) == 0)
        options.once (true);
      else if (str.find ("matchcase", 0) == 0)
        options.case_insensitive (false);
      else if (str.find ("ignorecase", 0) == 0)
        options.case_insensitive (true);
      else if (str.find ("dotall", 0) == 0)
        options.dotexceptnewline (false);
      else if (str.find ("stringanchors", 0) == 0)
        options.lineanchors (false);
      else if (str.find ("literalspacing", 0) == 0)
        options.freespacing (false);
      else if (str.find ("noemptymatch", 0) == 0)
        options.emptymatch (false);
      else if (str.find ("dotexceptnewline", 0) == 0)
        options.dotexceptnewline (true);
      else if (str.find ("lineanchors", 0) == 0)
        options.lineanchors (true);
      else if (str.find ("freespacing", 0) == 0)
        options.freespacing (true);
      else if (str.find ("emptymatch", 0) == 0)
        options.emptymatch (true);
      else if (str.find ("start", 0) == 0
               || str.find ("end", 0) == 0
               || str.find ("tokenextents", 0) == 0
               || str.find ("match", 0) == 0
               || str.find ("tokens", 0) == 0
               || str.find ("names", 0) == 0
               || str.find ("split", 0) == 0)
        extra_args = true;
      else
        error ("%s: unrecognized option", who.c_str ());
    }
}

static octave_value_list
octregexp (const octave_value_list &args, int nargout,
           const std::string &who, bool case_insensitive = false)
{
  octave_value_list retval;

  int nargin = args.length ();

  // Make sure we have string, pattern
  const std::string buffer = args(0).string_value ();
  if (error_state)
    return retval;

  std::string pattern = args(1).string_value ();
  if (error_state)
    return retval;
  // Matlab compatibility.
  if (args(1).is_sq_string ())
    pattern = do_regexp_ptn_string_escapes (pattern);

  regexp::opts options;
  options.case_insensitive (case_insensitive);
  bool extra_options = false;
  parse_options (options, args, who, 2, extra_options);
  if (error_state)
    return retval;

  regexp::match_data rx_lst = regexp_match (pattern, buffer, options, who);

  string_vector named_pats = rx_lst.named_patterns ();

  size_t sz = rx_lst.size ();

  if (! error_state)
    {
      // Converted the linked list in the correct form for the return values

      octave_idx_type i = 0;
      octave_scalar_map nmap;

      retval.resize (7);

      if (sz == 1)
        {
          string_vector named_tokens = rx_lst.begin ()->named_tokens ();

          for (int j = 0; j < named_pats.length (); j++)
            nmap.assign (named_pats(j), named_tokens(j));

          retval(5) = nmap;
        }
      else
        {
          for (int j = 0; j < named_pats.length (); j++)
            {
              Cell tmp (dim_vector (1, sz));

              i = 0;
              for (regexp::match_data::const_iterator p = rx_lst.begin ();
                   p != rx_lst.end (); p++)
                {
                  string_vector named_tokens = p->named_tokens ();

                  tmp(i++) = named_tokens(j);
                }

              nmap.assign (named_pats(j), octave_value (tmp));
            }

          retval(5) = nmap;
        }

      if (options.once ())
        {
          regexp::match_data::const_iterator p = rx_lst.begin ();

          retval(4) = sz ? p->tokens () : Cell ();
          retval(3) = sz ? p->match_string () : std::string ();
          retval(2) = sz ? p->token_extents () : Matrix ();

          if (sz)
            {
              double start = p->start ();
              double end = p->end ();

              Cell split (dim_vector (1, 2));
              split(0) = buffer.substr (0, start-1);
              split(1) = buffer.substr (end);

              retval(6) = split;
              retval(1) = end;
              retval(0) = start;
            }
          else
            {
              retval(6) = buffer;
              retval(1) = Matrix ();
              retval(0) = Matrix ();
            }
        }
      else
        {
          Cell tokens (dim_vector (1, sz));
          Cell match_string (dim_vector (1, sz));
          Cell token_extents (dim_vector (1, sz));
          NDArray end (dim_vector (1, sz));
          NDArray start (dim_vector (1, sz));
          Cell split (dim_vector (1, sz+1));
          size_t sp_start = 0;

          i = 0;
          for (regexp::match_data::const_iterator p = rx_lst.begin ();
               p != rx_lst.end (); p++)
            {
              double s = p->start ();
              double e = p->end ();

              string_vector tmp = p->tokens ();
              tokens(i) = Cell (dim_vector (1, tmp.length ()), tmp);
              match_string(i) = p->match_string ();
              token_extents(i) = p->token_extents ();
              end(i) = e;
              start(i) = s;
              split(i) = buffer.substr (sp_start, s-sp_start-1);
              sp_start = e;
              i++;
            }

          split(i) = buffer.substr (sp_start);

          retval(6) = split;
          retval(4) = tokens;
          retval(3) = match_string;
          retval(2) = token_extents;
          retval(1) = end;
          retval(0) = start;
        }

      // Alter the order of the output arguments

      if (extra_options)
        {
          int n = 0;
          octave_value_list new_retval;
          new_retval.resize (nargout);

          OCTAVE_LOCAL_BUFFER (int, arg_used, 6);
          for (int j = 0; j < 6; j++)
            arg_used[j] = false;

          for (int j = 2; j < nargin; j++)
            {
              int k = 0;
              std::string str = args(j).string_value ();
              std::transform (str.begin (), str.end (), str.begin (), tolower);

              if (str.find ("once", 0) == 0
                  || str.find ("stringanchors", 0) == 0
                  || str.find ("lineanchors", 0) == 0
                  || str.find ("matchcase", 0) == 0
                  || str.find ("ignorecase", 0) == 0
                  || str.find ("dotall", 0) == 0
                  || str.find ("dotexceptnewline", 0) == 0
                  || str.find ("literalspacing", 0) == 0
                  || str.find ("freespacing", 0) == 0
                  || str.find ("noemptymatch", 0) == 0
                  || str.find ("emptymatch", 0) == 0)
                continue;
              else if (str.find ("start", 0) == 0)
                k = 0;
              else if (str.find ("end", 0) == 0)
                k = 1;
              else if (str.find ("tokenextents", 0) == 0)
                k = 2;
              else if (str.find ("match", 0) == 0)
                k = 3;
              else if (str.find ("tokens", 0) == 0)
                k = 4;
              else if (str.find ("names", 0) == 0)
                k = 5;
              else if (str.find ("split", 0) == 0)
                k = 6;

              new_retval(n++) = retval(k);
              arg_used[k] = true;

              if (n == nargout)
                break;
            }

          // Fill in the rest of the arguments
          if (n < nargout)
            {
              for (int j = 0; j < 6; j++)
                {
                  if (! arg_used[j])
                    new_retval(n++) = retval(j);
                }
            }

          retval = new_retval;
        }
    }

  return retval;
}

static octave_value_list
octcellregexp (const octave_value_list &args, int nargout,
               const std::string &who, bool case_insensitive = false)
{
  octave_value_list retval;

  if (args(0).is_cell ())
    {
      OCTAVE_LOCAL_BUFFER (Cell, newretval, nargout);
      octave_value_list new_args = args;
      Cell cellstr = args(0).cell_value ();
      if (args(1).is_cell ())
        {
          Cell cellpat = args(1).cell_value ();

          if (cellpat.numel () == 1)
            {
              for (int j = 0; j < nargout; j++)
                newretval[j].resize (cellstr.dims ());

              new_args(1) = cellpat(0);

              for (octave_idx_type i = 0; i < cellstr.numel (); i++)
                {
                  new_args(0) = cellstr(i);
                  octave_value_list tmp = octregexp (new_args, nargout, who,
                                                     case_insensitive);

                  if (error_state)
                    break;

                  for (int j = 0; j < nargout; j++)
                    newretval[j](i) = tmp(j);
                }
            }
          else if (cellstr.numel () == 1)
            {
              for (int j = 0; j < nargout; j++)
                newretval[j].resize (cellpat.dims ());

              new_args(0) = cellstr(0);

              for (octave_idx_type i = 0; i < cellpat.numel (); i++)
                {
                  new_args(1) = cellpat(i);
                  octave_value_list tmp = octregexp (new_args, nargout, who,
                                                     case_insensitive);

                  if (error_state)
                    break;

                  for (int j = 0; j < nargout; j++)
                    newretval[j](i) = tmp(j);
                }
            }
          else if (cellstr.numel () == cellpat.numel ())
            {

              if (cellstr.dims () != cellpat.dims ())
                error ("%s: inconsistent cell array dimensions", who.c_str ());
              else
                {
                  for (int j = 0; j < nargout; j++)
                    newretval[j].resize (cellstr.dims ());

                  for (octave_idx_type i = 0; i < cellstr.numel (); i++)
                    {
                      new_args(0) = cellstr(i);
                      new_args(1) = cellpat(i);

                      octave_value_list tmp = octregexp (new_args, nargout, who,
                                                         case_insensitive);

                      if (error_state)
                        break;

                      for (int j = 0; j < nargout; j++)
                        newretval[j](i) = tmp(j);
                    }
                }
            }
          else
            error ("regexp: cell array arguments must be scalar or equal size");
        }
      else
        {
          for (int j = 0; j < nargout; j++)
            newretval[j].resize (cellstr.dims ());

          for (octave_idx_type i = 0; i < cellstr.numel (); i++)
            {
              new_args(0) = cellstr(i);
              octave_value_list tmp = octregexp (new_args, nargout, who,
                                                 case_insensitive);

              if (error_state)
                break;

              for (int j = 0; j < nargout; j++)
                newretval[j](i) = tmp(j);
            }
        }

      if (!error_state)
        for (int j = 0; j < nargout; j++)
          retval(j) = octave_value (newretval[j]);
    }
  else if (args(1).is_cell ())
    {
      OCTAVE_LOCAL_BUFFER (Cell, newretval, nargout);
      octave_value_list new_args = args;
      Cell cellpat = args(1).cell_value ();

      for (int j = 0; j < nargout; j++)
        newretval[j].resize (cellpat.dims ());

      for (octave_idx_type i = 0; i < cellpat.numel (); i++)
        {
          new_args(1) = cellpat(i);
          octave_value_list tmp = octregexp (new_args, nargout, who,
                                             case_insensitive);

          if (error_state)
            break;

          for (int j = 0; j < nargout; j++)
            newretval[j](i) = tmp(j);
        }

      if (!error_state)
        {
          for (int j = 0; j < nargout; j++)
            retval(j) = octave_value (newretval[j]);
        }
    }
  else
    retval = octregexp (args, nargout, who, case_insensitive);

  return retval;

}

DEFUN (regexp, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{s}, @var{e}, @var{te}, @var{m}, @var{t}, @var{nm}, @var{sp}] =} regexp (@var{str}, @var{pat})\n\
@deftypefnx {Built-in Function} {[@dots{}] =} regexp (@var{str}, @var{pat}, \"@var{opt1}\", @dots{})\n\
Regular expression string matching.\n\
\n\
Search for @var{pat} in @var{str} and return the positions and substrings of\n\
any matches, or empty values if there are none.\n\
\n\
The matched pattern @var{pat} can include any of the standard regex\n\
operators, including:\n\
\n\
@table @code\n\
@item .\n\
Match any character\n\
\n\
@item * + ? @{@}\n\
Repetition operators, representing\n\
\n\
@table @code\n\
@item *\n\
Match zero or more times\n\
\n\
@item +\n\
Match one or more times\n\
\n\
@item ?\n\
Match zero or one times\n\
\n\
@item @{@var{n}@}\n\
Match exactly @var{n} times\n\
\n\
@item @{@var{n},@}\n\
Match @var{n} or more times\n\
\n\
@item @{@var{m},@var{n}@}\n\
Match between @var{m} and @var{n} times\n\
@end table\n\
\n\
@item [@dots{}] [^@dots{}]\n\
\n\
List operators.  The pattern will match any character listed between \"[\"\n\
and \"]\".  If the first character is \"^\" then the pattern is inverted and\n\
any character except those listed between brackets will match.\n\
\n\
Escape sequences defined below can also be used inside list operators.  For\n\
example, a template for a floating point number might be @code{[-+.\\d]+}.\n\
\n\
@item () (?:)\n\
Grouping operator.  The first form, parentheses only, also creates a token.\n\
\n\
@item |\n\
Alternation operator.  Match one of a choice of regular expressions.  The\n\
alternatives must be delimited by the grouping operator @code{()} above.\n\
\n\
@item ^ $\n\
Anchoring operators.  Requires pattern to occur at the start (@code{^}) or\n\
end (@code{$}) of the string.\n\
@end table\n\
\n\
In addition, the following escaped characters have special meaning.\n\
\n\
@table @code\n\
\n\
@item \\d\n\
Match any digit\n\
\n\
@item \\D\n\
Match any non-digit\n\
\n\
@item \\s\n\
Match any whitespace character\n\
\n\
@item \\S\n\
Match any non-whitespace character\n\
\n\
@item \\w\n\
Match any word character\n\
\n\
@item \\W\n\
Match any non-word character\n\
\n\
@item \\<\n\
Match the beginning of a word\n\
\n\
@item \\>\n\
Match the end of a word\n\
\n\
@item \\B\n\
Match within a word\n\
@end table\n\
\n\
Implementation Note: For compatibility with @sc{matlab}, escape sequences\n\
in @var{pat} (e.g., @qcode{\"@xbackslashchar{}n\"} => newline) are expanded\n\
even when @var{pat} has been defined with single quotes.  To disable\n\
expansion use a second backslash before the escape sequence (e.g.,\n\
\"@xbackslashchar{}@xbackslashchar{}n\") or use the @code{regexptranslate}\n\
function.\n\
\n\
The outputs of @code{regexp} default to the order given below\n\
\n\
@table @var\n\
@item s\n\
The start indices of each matching substring\n\
\n\
@item e\n\
The end indices of each matching substring\n\
\n\
@item te\n\
The extents of each matched token surrounded by @code{(@dots{})} in\n\
@var{pat}\n\
\n\
@item m\n\
A cell array of the text of each match\n\
\n\
@item t\n\
A cell array of the text of each token matched\n\
\n\
@item nm\n\
A structure containing the text of each matched named token, with the name\n\
being used as the fieldname.  A named token is denoted by\n\
@code{(?<name>@dots{})}.\n\
\n\
@item sp\n\
A cell array of the text not returned by match, i.e., what remains if you\n\
split the string based on @var{pat}.\n\
@end table\n\
\n\
Particular output arguments, or the order of the output arguments, can be\n\
selected by additional @var{opt} arguments.  These are strings and the\n\
correspondence between the output arguments and the optional argument\n\
are\n\
\n\
@multitable @columnfractions 0.2 0.3 0.3 0.2\n\
@item @tab @qcode{'start'}        @tab @var{s}  @tab\n\
@item @tab @qcode{'end'}          @tab @var{e}  @tab\n\
@item @tab @qcode{'tokenExtents'} @tab @var{te} @tab\n\
@item @tab @qcode{'match'}        @tab @var{m}  @tab\n\
@item @tab @qcode{'tokens'}       @tab @var{t}  @tab\n\
@item @tab @qcode{'names'}        @tab @var{nm} @tab\n\
@item @tab @qcode{'split'}        @tab @var{sp} @tab\n\
@end multitable\n\
\n\
Additional arguments are summarized below.\n\
\n\
@table @samp\n\
@item once\n\
Return only the first occurrence of the pattern.\n\
\n\
@item matchcase\n\
Make the matching case sensitive.  (default)\n\
\n\
Alternatively, use (?-i) in the pattern.\n\
\n\
@item ignorecase\n\
Ignore case when matching the pattern to the string.\n\
\n\
Alternatively, use (?i) in the pattern.\n\
\n\
@item stringanchors\n\
Match the anchor characters at the beginning and end of the string.\n\
(default)\n\
\n\
Alternatively, use (?-m) in the pattern.\n\
\n\
@item lineanchors\n\
Match the anchor characters at the beginning and end of the line.\n\
\n\
Alternatively, use (?m) in the pattern.\n\
\n\
@item dotall\n\
The pattern @code{.} matches all characters including the newline character.\n\
 (default)\n\
\n\
Alternatively, use (?s) in the pattern.\n\
\n\
@item dotexceptnewline\n\
The pattern @code{.} matches all characters except the newline character.\n\
\n\
Alternatively, use (?-s) in the pattern.\n\
\n\
@item literalspacing\n\
All characters in the pattern, including whitespace, are significant and are\n\
used in pattern matching.  (default)\n\
\n\
Alternatively, use (?-x) in the pattern.\n\
\n\
@item freespacing\n\
The pattern may include arbitrary whitespace and also comments beginning with\n\
the character @samp{#}.\n\
\n\
Alternatively, use (?x) in the pattern.\n\
\n\
@item noemptymatch\n\
Zero-length matches are not returned.  (default)\n\
\n\
@item emptymatch\n\
Return zero-length matches.\n\
\n\
@code{regexp ('a', 'b*', 'emptymatch')} returns @code{[1 2]} because there\n\
are zero or more @qcode{'b'} characters at positions 1 and end-of-string.\n\
\n\
@end table\n\
@seealso{regexpi, strfind, regexprep}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();
  else if (args(0).is_cell () || args(1).is_cell ())
    retval = octcellregexp (args, (nargout > 0 ? nargout : 1), "regexp");
  else
    retval = octregexp (args, nargout, "regexp");

  return retval;
}

/*
## PCRE_ERROR_MATCHLIMIT test
%!test
%! s = sprintf ('\t4\n0000\t-0.00\t-0.0000\t4\t-0.00\t-0.0000\t4\n0000\t-0.00\t-0.0000\t0\t-0.00\t-');
%! ws = warning ("query");
%! unwind_protect
%!   warning ("off");
%!   regexp (s, '(\s*-*\d+[.]*\d*\s*)+\n');
%! unwind_protect_cleanup
%!   warning (ws);
%! end_unwind_protect

## segfault test
%!assert (regexp ("abcde", "."), [1,2,3,4,5])
## Infinite loop test
%!assert (isempty (regexp ("abcde", "")))

## Check that anchoring of pattern works correctly
%!assert (regexp ('abcabc', '^abc'), 1)
%!assert (regexp ('abcabc', 'abc$'), 4)
%!assert (regexp ('abcabc', '^abc$'), zeros (1,0))

%!test
%! [s, e, te, m, t] = regexp (' No Match ', 'f(.*)uck');
%! assert (s, zeros (1,0));
%! assert (e, zeros (1,0));
%! assert (te, cell (1,0));
%! assert (m, cell (1,0));
%! assert (t, cell (1,0));

%!test
%! [s, e, te, m, t] = regexp (' FiRetrUck ', 'f(.*)uck');
%! assert (s, zeros (1,0));
%! assert (e, zeros (1,0));
%! assert (te, cell (1,0));
%! assert (m, cell (1,0));
%! assert (t, cell (1,0));

%!test
%! [s, e, te, m, t] = regexp (' firetruck ', 'f(.*)uck');
%! assert (s, 2);
%! assert (e, 10);
%! assert (te{1}, [3, 7]);
%! assert (m{1}, 'firetruck');
%! assert (t{1}{1}, 'iretr');

%!test
%! [s, e, te, m, t] = regexp ('short test string', '\w*r\w*');
%! assert (s, [1, 12]);
%! assert (e, [5, 17]);
%! assert (size (te), [1, 2]);
%! assert (isempty (te{1}));
%! assert (isempty (te{2}));
%! assert (m{1}, 'short');
%! assert (m{2}, 'string');
%! assert (size (t), [1, 2]);
%! assert (isempty (t{1}));
%! assert (isempty (t{2}));

%!test
%! [s, e, te, m, t] = regexp ('short test string', '\w*r\w*', 'once');
%! assert (s, 1);
%! assert (e, 5);
%! assert (isempty (te));
%! assert (m, 'short');
%! assert (isempty (t));

%!test
%! [m, te, e, s, t] = regexp ('short test string', '\w*r\w*', 'once', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s, 1);
%! assert (e, 5);
%! assert (isempty (te));
%! assert (m, 'short');
%! assert (isempty (t));

%!test
%! [s, e, te, m, t, nm] = regexp ('short test string', '(?<word1>\w*t)\s*(?<word2>\w*t)');
%! assert (s, 1);
%! assert (e, 10);
%! assert (size (te), [1, 1]);
%! assert (te{1}, [1,5; 7,10]);
%! assert (m{1}, 'short test');
%! assert (size (t), [1, 1]);
%! assert (t{1}{1}, 'short');
%! assert (t{1}{2}, 'test');
%! assert (size (nm), [1, 1]);
%! assert (! isempty (fieldnames (nm)));
%! assert (sort (fieldnames (nm)), {'word1';'word2'});
%! assert (nm.word1, 'short');
%! assert (nm.word2, 'test');

%!test
%! [nm, m, te, e, s, t] = regexp ('short test string', '(?<word1>\w*t)\s*(?<word2>\w*t)', 'names', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s, 1);
%! assert (e, 10);
%! assert (size (te), [1, 1]);
%! assert (te{1}, [1,5; 7,10]);
%! assert (m{1}, 'short test');
%! assert (size (t), [1, 1]);
%! assert (t{1}{1}, 'short');
%! assert (t{1}{2}, 'test');
%! assert (size (nm), [1, 1]);
%! assert (!isempty (fieldnames (nm)));
%! assert (sort (fieldnames (nm)), {'word1';'word2'});
%! assert (nm.word1, 'short');
%! assert (nm.word2, 'test');

%!test
%! [t, nm] = regexp ("John Davis\nRogers, James", '(?<first>\w+)\s+(?<last>\w+)|(?<last>\w+),\s+(?<first>\w+)', 'tokens', 'names');
%! assert (size (t), [1, 2]);
%! assert (t{1}{1}, 'John');
%! assert (t{1}{2}, 'Davis');
%! assert (t{2}{1}, 'Rogers');
%! assert (t{2}{2}, 'James');
%! assert (size (nm), [1, 1]);
%! assert (nm.first{1}, 'John');
%! assert (nm.first{2}, 'James');
%! assert (nm.last{1}, 'Davis');
%! assert (nm.last{2}, 'Rogers');

## Tests for named tokens
%!test
%! ## Parenthesis in named token (ie (int)) causes a problem
%! assert (regexp ('qwe int asd', ['(?<typestr>(int))'], 'names'), struct ('typestr', 'int'));

%!test
%! ## Mix of named and unnamed tokens can cause segfault (bug #35683)
%! str = "abcde";
%! ptn = '(?<T1>a)(\w+)(?<T2>d\w+)';
%! tokens = regexp (str, ptn, "names");
%! assert (isstruct (tokens) && numel (tokens) == 1);
%! assert (tokens.T1, "a");
%! assert (tokens.T2, "de");

%!assert (regexp ("abc\nabc", '.'), [1:7])
%!assert (regexp ("abc\nabc", '.', 'dotall'), [1:7])
%!test
%! assert (regexp ("abc\nabc", '(?s).'), [1:7]);
%! assert (regexp ("abc\nabc", '.', 'dotexceptnewline'), [1,2,3,5,6,7]);
%! assert (regexp ("abc\nabc", '(?-s).'), [1,2,3,5,6,7]);

%!assert (regexp ("caseCaSe", 'case'), 1)
%!assert (regexp ("caseCaSe", 'case', "matchcase"), 1)
%!assert (regexp ("caseCaSe", 'case', "ignorecase"), [1,5])
%!test
%! assert (regexp ("caseCaSe", '(?-i)case'), 1);
%! assert (regexp ("caseCaSe", '(?i)case'), [1, 5]);

%!assert (regexp ("abc\nabc", 'c$'), 7)
%!assert (regexp ("abc\nabc", 'c$', "stringanchors"), 7)
%!test
%! assert (regexp ("abc\nabc", '(?-m)c$'), 7);
%! assert (regexp ("abc\nabc", 'c$',"lineanchors"), [3, 7]);
%! assert (regexp ("abc\nabc", '(?m)c$'), [3,7]);

%!assert (regexp ("this word", 's w'), 4)
%!assert (regexp ("this word", 's w', 'literalspacing'), 4)
%!test
%! assert (regexp ("this word", '(?-x)s w', 'literalspacing'), 4);
%! assert (regexp ("this word", 's w', 'freespacing'), zeros (1,0));
%! assert (regexp ("this word", '(?x)s w'), zeros (1,0));

%!test
%! [s, e, te, m, t, nm, sp] = regexp ('OCTAVE', '[VOCT]*', 'noemptymatch');
%! assert (s, [1 5]);
%! assert (e, [3 5]);
%! assert (te, { zeros(0,2), zeros(0,2) });
%! assert (m, { "OCT", "V" });
%! assert (t, { cell(1,0), cell(1,0) });
%! assert (isempty (fieldnames (nm)));
%! assert (sp, { "", "A", "E" });

%!test
%! [s, e, te, m, t, nm, sp] = regexp ('OCTAVE', '([VOCT]*)', 'noemptymatch');
%! assert (s, [1 5]);
%! assert (e, [3 5]);
%! assert (te, { [1 3], [5 5] });
%! assert (m, { "OCT", "V" });
%! assert (t, { {"OCT"}, {"V"} });
%! assert (isempty (fieldnames (nm)));
%! assert (sp, { "", "A", "E" });

%!test
%! [s, e, te, m, t, nm, sp] = regexp ('OCTAVE', '[VOCT]*', 'emptymatch');
%! assert (s, [1 4 5 6 7]);
%! assert (e, [3 3 5 5 6]);
%! assert (te, repmat ({zeros(0,2)}, [1, 5]));
%! assert (m, { "OCT", "", "V", "", "" });
%! assert (t, repmat({cell(1,0)}, [1, 5]));
%! assert (isempty (fieldnames (nm)));
%! assert (sp, { "", "", "A", "", "E", "" });

%!test
%! [s, e, te, m, t, nm, sp] = regexp ('OCTAVE', '([VOCT]*)', 'emptymatch');
%! assert (s, [1 4 5 6 7]);
%! assert (e, [3 3 5 5 6]);
%! assert (te, { [1 3], [4 3], [5 5], [6 5], [7 6] });
%! assert (m, { "OCT", "", "V", "", "" });
%! assert (t, { {"OCT"}, {""}, {"V"}, {""}, {""} });
%! assert (isempty (fieldnames (nm)));
%! assert (sp, { "", "", "A", "", "E", "" });

%!error regexp ('string', 'tri', 'BadArg')
%!error regexp ('string')

%!assert (regexp ({'asdfg-dfd';'-dfd-dfd-';'qasfdfdaq'}, '-'), {6;[1,5,9];zeros(1,0)})
%!assert (regexp ({'asdfg-dfd';'-dfd-dfd-';'qasfdfdaq'}, {'-';'f';'q'}), {6;[3,7];[1,9]})
%!assert (regexp ('Strings', {'t','s'}), {2, 7})

## Test case for lookaround operators
%!test
%! assert (regexp ('Iraq', 'q(?!u)'), 4);
%! assert (regexp ('quit', 'q(?!u)'), zeros (1, 0));
%! assert (regexp ('quit', 'q(?=u)' , 'match'), {'q'});
%! assert (regexp ("quit", 'q(?=u+)', 'match'), {'q'});
%! assert (regexp ("qit",  'q(?=u+)', 'match'), cell (1, 0));
%! assert (regexp ("qit",  'q(?=u*)', 'match'), {'q'});
%! assert (regexp ('thingamabob', '(?<=a)b'), 9);

## Tests for split option.
%!shared str
%! str = "foo bar foo";
%!test
%! [a, b] = regexp (str, "f..", "match", "split");
%! assert (a, {"foo", "foo"});
%! assert (b, {"", " bar ", ""});
%!test
%! [a, b] = regexp (str, "f..", "match", "split", "once");
%! assert (a, "foo");
%! assert (b, {"", " bar foo"});
%!test
%! [a, b] = regexp (str, "fx.", "match", "split");
%! assert (a, cell (1, 0));
%! assert (b, {"foo bar foo"});
%!test
%! [a, b] = regexp (str, "fx.", "match", "split", "once");
%! assert (a, "");;
%! assert (b, "foo bar foo");

%!shared str
%! str = "foo bar";
%!test
%! [a, b] = regexp (str, "f..", "match", "split");
%! assert (a, {"foo"});
%! assert (b, {"", " bar"});
%!test
%! [a, b] = regexp (str, "b..", "match", "split");
%! assert (a, {"bar"});
%! assert (b, {"foo ", ""});
%!test
%! [a, b] = regexp (str, "x", "match", "split");
%! assert (a, cell (1, 0));
%! assert (b, {"foo bar"});
%!test
%! [a, b] = regexp (str, "[o]+", "match", "split");
%! assert (a, {"oo"});
%! assert (b, {"f", " bar"});

%!assert (regexp ("\n", '\n'), 1);
%!assert (regexp ("\n", "\n"), 1);
*/

DEFUN (regexpi, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{s}, @var{e}, @var{te}, @var{m}, @var{t}, @var{nm}, @var{sp}] =} regexpi (@var{str}, @var{pat})\n\
@deftypefnx {Built-in Function} {[@dots{}] =} regexpi (@var{str}, @var{pat}, \"@var{opt1}\", @dots{})\n\
\n\
Case insensitive regular expression string matching.\n\
\n\
Search for @var{pat} in @var{str} and return the positions and substrings of\n\
any matches, or empty values if there are none.  @xref{XREFregexp,,regexp},\n\
for details on the syntax of the search pattern.\n\
@seealso{regexp}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();
  else if (args(0).is_cell () || args(1).is_cell ())
    retval = octcellregexp (args, (nargout > 0 ? nargout : 1), "regexpi", true);
  else
    retval = octregexp (args, nargout, "regexpi", true);

  return retval;
}

/*
## segfault test
%!assert (regexpi ("abcde", "."), [1,2,3,4,5])

## Check that anchoring of pattern works correctly
%!assert (regexpi ('abcabc', '^ABC'), 1)
%!assert (regexpi ('abcabc', 'ABC$'), 4)
%!assert (regexpi ('abcabc', '^ABC$'), zeros (1,0))

%!test
%! [s, e, te, m, t] = regexpi (' No Match ', 'f(.*)uck');
%! assert (s, zeros (1,0));
%! assert (e, zeros (1,0));
%! assert (te, cell (1,0));
%! assert (m, cell (1,0));
%! assert (t, cell (1,0));

%!test
%! [s, e, te, m, t] = regexpi (' FiRetrUck ', 'f(.*)uck');
%! assert (s, 2);
%! assert (e, 10);
%! assert (te{1}, [3, 7]);
%! assert (m{1}, 'FiRetrUck');
%! assert (t{1}{1}, 'iRetr');

%!test
%! [s, e, te, m, t] = regexpi (' firetruck ', 'f(.*)uck');
%! assert (s, 2);
%! assert (e, 10);
%! assert (te{1}, [3, 7]);
%! assert (m{1}, 'firetruck');
%! assert (t{1}{1}, 'iretr');

%!test
%! [s, e, te, m, t] = regexpi ('ShoRt Test String', '\w*r\w*');
%! assert (s, [1, 12]);
%! assert (e, [5, 17]);
%! assert (size (te), [1, 2]);
%! assert (isempty (te{1}));
%! assert (isempty (te{2}));
%! assert (m{1}, 'ShoRt');
%! assert (m{2}, 'String');
%! assert (size (t), [1, 2]);
%! assert (isempty (t{1}));
%! assert (isempty (t{2}));

%!test
%! [s, e, te, m, t] = regexpi ('ShoRt Test String', '\w*r\w*', 'once');
%! assert (s, 1);
%! assert (e, 5);
%! assert (isempty (te));
%! assert (m, 'ShoRt');
%! assert (isempty (t));

%!test
%! [m, te, e, s, t] = regexpi ('ShoRt Test String', '\w*r\w*', 'once', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s, 1);
%! assert (e, 5);
%! assert (isempty (te));
%! assert (m, 'ShoRt');
%! assert (isempty (t));

%!test
%! [s, e, te, m, t, nm] = regexpi ('ShoRt Test String', '(?<word1>\w*t)\s*(?<word2>\w*t)');
%! assert (s, 1);
%! assert (e, 10);
%! assert (size (te), [1, 1]);
%! assert (te{1}, [1,5; 7,10]);
%! assert (m{1}, 'ShoRt Test');
%! assert (size (t), [1, 1]);
%! assert (t{1}{1}, 'ShoRt');
%! assert (t{1}{2}, 'Test');
%! assert (size (nm), [1, 1]);
%! assert (! isempty (fieldnames (nm)));
%! assert (sort (fieldnames (nm)), {'word1';'word2'});
%! assert (nm.word1, 'ShoRt');
%! assert (nm.word2, 'Test');

%!test
%! [nm, m, te, e, s, t] = regexpi ('ShoRt Test String', '(?<word1>\w*t)\s*(?<word2>\w*t)', 'names', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s, 1);
%! assert (e, 10);
%! assert (size (te), [1, 1]);
%! assert (te{1}, [1,5; 7,10]);
%! assert (m{1}, 'ShoRt Test');
%! assert (size (t), [1, 1]);
%! assert (t{1}{1}, 'ShoRt');
%! assert (t{1}{2}, 'Test');
%! assert (size (nm), [1, 1]);
%! assert (!isempty (fieldnames (nm)));
%! assert (sort (fieldnames (nm)), {'word1';'word2'});
%! assert (nm.word1, 'ShoRt');
%! assert (nm.word2, 'Test');

%!assert (regexpi ("abc\nabc", '.'), [1:7])
%!assert (regexpi ("abc\nabc", '.', 'dotall'), [1:7])
%!test
%! assert (regexpi ("abc\nabc", '(?s).'), [1:7]);
%! assert (regexpi ("abc\nabc", '.', 'dotexceptnewline'), [1,2,3,5,6,7]);
%! assert (regexpi ("abc\nabc", '(?-s).'), [1,2,3,5,6,7]);

%!assert (regexpi ("caseCaSe", 'case'), [1, 5])
%!assert (regexpi ("caseCaSe", 'case', "matchcase"), 1)
%!assert (regexpi ("caseCaSe", 'case', "ignorecase"), [1, 5])
%!test
%! assert (regexpi ("caseCaSe", '(?-i)case'), 1);
%! assert (regexpi ("caseCaSe", '(?i)case'), [1, 5]);

%!assert (regexpi ("abc\nabc", 'C$'), 7)
%!assert (regexpi ("abc\nabc", 'C$', "stringanchors"), 7)
%!test
%! assert (regexpi ("abc\nabc", '(?-m)C$'), 7);
%! assert (regexpi ("abc\nabc", 'C$', "lineanchors"), [3, 7]);
%! assert (regexpi ("abc\nabc", '(?m)C$'), [3, 7]);

%!assert (regexpi ("this word", 'S w'), 4)
%!assert (regexpi ("this word", 'S w', 'literalspacing'), 4)
%!test
%! assert (regexpi ("this word", '(?-x)S w', 'literalspacing'), 4);
%! assert (regexpi ("this word", 'S w', 'freespacing'), zeros (1,0));
%! assert (regexpi ("this word", '(?x)S w'), zeros (1,0));

%!error regexpi ('string', 'tri', 'BadArg')
%!error regexpi ('string')

%!assert (regexpi ({'asdfg-dfd';'-dfd-dfd-';'qasfdfdaq'}, '-'), {6;[1,5,9];zeros(1, 0)})
%!assert (regexpi ({'asdfg-dfd', '-dfd-dfd-', 'qasfdfdaq'}, '-'), {6, [1,5,9], zeros(1,0)})
%!assert (regexpi ({'asdfg-dfd';'-dfd-dfd-';'qasfdfdaq'}, {'-';'f';'q'}), {6;[3,7];[1,9]})
%!assert (regexpi ('Strings', {'t', 's'}), {2, [1, 7]})

%!assert (regexpi ("\n", '\n'), 1);
%!assert (regexpi ("\n", "\n"), 1);
*/

static octave_value
octregexprep (const octave_value_list &args, const std::string &who)
{
  octave_value retval;

  int nargin = args.length ();

  // Make sure we have string, pattern, replacement
  const std::string buffer = args(0).string_value ();
  if (error_state)
    return retval;

  std::string pattern = args(1).string_value ();
  if (error_state)
    return retval;
  // Matlab compatibility.
  if (args(1).is_sq_string ())
    pattern = do_regexp_ptn_string_escapes (pattern);

  std::string replacement = args(2).string_value ();
  if (error_state)
    return retval;
  // Matlab compatibility.
  if (args(2).is_sq_string ())
    replacement = do_regexp_rep_string_escapes (replacement);

  // Pack options excluding 'tokenize' and various output
  // reordering strings into regexp arg list
  octave_value_list regexpargs (nargin-3, octave_value ());

  int len = 0;
  for (int i = 3; i < nargin; i++)
    {
      const std::string opt = args(i).string_value ();
      if (opt != "tokenize" && opt != "start" && opt != "end"
          && opt != "tokenextents" && opt != "match" && opt != "tokens"
          && opt != "names"  && opt != "split" && opt != "warnings")
        {
          regexpargs(len++) = args(i);
        }
    }
  regexpargs.resize (len);

  regexp::opts options;
  bool extra_args = false;
  parse_options (options, regexpargs, who, 0, extra_args);
  if (error_state)
    return retval;

  return regexp_replace (pattern, buffer, replacement, options, who);
}

DEFUN (regexprep, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{outstr} =} regexprep (@var{string}, @var{pat}, @var{repstr})\n\
@deftypefnx {Built-in Function} {@var{outstr} =} regexprep (@var{string}, @var{pat}, @var{repstr}, \"@var{opt1}\", @dots{})\n\
Replace occurrences of pattern @var{pat} in @var{string} with @var{repstr}.\n\
\n\
The pattern is a regular expression as documented for @code{regexp}.\n\
@xref{XREFregexp,,regexp}.\n\
\n\
The replacement string may contain @code{$i}, which substitutes for the ith\n\
set of parentheses in the match string.  For example,\n\
\n\
@example\n\
regexprep (\"Bill Dunn\", '(\\w+) (\\w+)', '$2, $1')\n\
@end example\n\
\n\
@noindent\n\
returns \"Dunn, Bill\"\n\
\n\
Options in addition to those of @code{regexp} are\n\
\n\
@table @samp\n\
\n\
@item once\n\
Replace only the first occurrence of @var{pat} in the result.\n\
\n\
@item warnings\n\
This option is present for compatibility but is ignored.\n\
\n\
@end table\n\
\n\
Implementation Note: For compatibility with @sc{matlab}, escape sequences\n\
in @var{pat} (e.g., @qcode{\"@xbackslashchar{}n\"} => newline) are expanded\n\
even when @var{pat} has been defined with single quotes.  To disable\n\
expansion use a second backslash before the escape sequence (e.g.,\n\
\"@xbackslashchar{}@xbackslashchar{}n\") or use the @code{regexptranslate}\n\
function.\n\
@seealso{regexp, regexpi, strrep}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin < 3)
    {
      print_usage ();
      return retval;
    }

  if (args(0).is_cell () || args(1).is_cell () || args(2).is_cell ())
    {
      Cell str;
      Cell pat;
      Cell rep;
      dim_vector dv0;
      dim_vector dv1 (1, 1);

      if (args(0).is_cell ())
        str = args(0).cell_value ();
      else
        str = Cell (args(0));

      if (args(1).is_cell ())
        pat = args(1).cell_value ();
      else
        pat = Cell (args(1));

      if (args(2).is_cell ())
        rep = args(2).cell_value ();
      else
        rep = Cell (args(2));

      dv0 = str.dims ();
      if (pat.numel () != 1)
        {
          dv1 = pat.dims ();
          if (rep.numel () != 1 && dv1 != rep.dims ())
            error ("regexprep: inconsistent cell array dimensions");
        }
      else if (rep.numel () != 1)
        dv1 = rep.dims ();

      if (!error_state)
        {
          Cell ret (dv0);
          octave_value_list new_args = args;

          for (octave_idx_type i = 0; i < dv0.numel (); i++)
            {
              new_args(0) = str(i);
              if (pat.numel () == 1)
                new_args(1) = pat(0);
              if (rep.numel () == 1)
                new_args(2) = rep(0);

              for (octave_idx_type j = 0; j < dv1.numel (); j++)
                {
                  if (pat.numel () != 1)
                    new_args(1) = pat(j);
                  if (rep.numel () != 1)
                    new_args(2) = rep(j);
                  new_args(0) = octregexprep (new_args, "regexprep");

                  if (error_state)
                    break;
                }

              if (error_state)
                break;

              ret(i) = new_args(0);
            }

          if (!error_state)
            retval = args(0).is_cell () ? octave_value (ret)
                                        : octave_value (ret(0));
        }
    }
  else
    retval = octregexprep (args, "regexprep");

  return retval;
}

/*
%!test  # Replace with empty
%! xml = '<!-- This is some XML --> <tag v="hello">some stuff<!-- sample tag--></tag>';
%! t = regexprep (xml, '<[!?][^>]*>', '');
%! assert (t, ' <tag v="hello">some stuff</tag>');

%!test  # Replace with non-empty
%! xml = '<!-- This is some XML --> <tag v="hello">some stuff<!-- sample tag--></tag>';
%! t = regexprep (xml, '<[!?][^>]*>', '?');
%! assert (t, '? <tag v="hello">some stuff?</tag>');

%!test  # Check that 'tokenize' is ignored
%! xml = '<!-- This is some XML --> <tag v="hello">some stuff<!-- sample tag--></tag>';
%! t = regexprep (xml, '<[!?][^>]*>', '', 'tokenize');
%! assert (t, ' <tag v="hello">some stuff</tag>');

## Test capture replacement
%!test
%! data = "Bob Smith\nDavid Hollerith\nSam Jenkins";
%! result = "Smith, Bob\nHollerith, David\nJenkins, Sam";
%! t = regexprep (data, '(?m)^(\w+)\s+(\w+)$', '$2, $1');
%! assert (t, result);

## Return the original if no match
%!assert (regexprep ('hello', 'world', 'earth'), 'hello')

## Test emptymatch
%!assert (regexprep ('World', '^', 'Hello '), 'World')
%!assert (regexprep ('World', '^', 'Hello ', 'emptymatch'), 'Hello World')

## Test a general replacement
%!assert (regexprep ("a[b]c{d}e-f=g", "[^A-Za-z0-9_]", "_"), "a_b_c_d_e_f_g")

## Make sure it works at the beginning and end
%!assert (regexprep ("a[b]c{d}e-f=g", "a", "_"), "_[b]c{d}e-f=g")
%!assert (regexprep ("a[b]c{d}e-f=g", "g", "_"), "a[b]c{d}e-f=_")

## Options
%!assert (regexprep ("a[b]c{d}e-f=g", "[^A-Za-z0-9_]", "_", "once"), "a_b]c{d}e-f=g")
%!assert (regexprep ("a[b]c{d}e-f=g", "[^A-Z0-9_]", "_", "ignorecase"), "a_b_c_d_e_f_g")

## Option combinations
%!assert (regexprep ("a[b]c{d}e-f=g", "[^A-Z0-9_]", "_", "once", "ignorecase"), "a_b]c{d}e-f=g")

## End conditions on replacement
%!assert (regexprep ("abc", "(b)", ".$1"), "a.bc");
%!assert (regexprep ("abc", "(b)", "$1"), "abc");
%!assert (regexprep ("abc", "(b)", "$1."), "ab.c");
%!assert (regexprep ("abc", "(b)", "$1.."), "ab..c");

## Test cell array arguments
%!assert (regexprep ("abc", {"b","a"}, "?"), "??c")
%!assert (regexprep ({"abc","cba"}, "b", "?"), {"a?c","c?a"})
%!assert (regexprep ({"abc","cba"}, {"b","a"}, {"?","!"}), {"!?c","c?!"})

# Nasty lookbehind expression
%!assert (regexprep ('x^(-1)+y(-1)+z(-1)=0', '(?<=[a-z]+)\(\-[1-9]*\)', '_minus1'),'x^(-1)+y_minus1+z_minus1=0')

%!assert (regexprep ("\n", '\n', "X"), "X");
%!assert (regexprep ("\n", "\n", "X"), "X");
*/
