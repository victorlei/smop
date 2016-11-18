/*

Copyright (C) 2012 John W. Eaton
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
#include <string>
#include <vector>

#if defined (HAVE_PCRE_H)
#include <pcre.h>
#elif defined (HAVE_PCRE_PCRE_H)
#include <pcre/pcre.h>
#endif

#include "Matrix.h"
#include "base-list.h"
#include "lo-error.h"
#include "oct-locbuf.h"
#include "quit.h"
#include "lo-regexp.h"
#include "str-vec.h"

// Define the maximum number of retries for a pattern
// that possibly results in an infinite recursion.
#define PCRE_MATCHLIMIT_MAX 10

// FIXME: should this be configurable?
#define MAXLOOKBEHIND 10

static bool lookbehind_warned = false;

// FIXME: don't bother collecting and composing return values
//        the user doesn't want.

void
regexp::free (void)
{
  if (data)
    pcre_free (static_cast<pcre *> (data));
}

void
regexp::compile_internal (void)
{
  // If we had a previously compiled pattern, release it.
  free ();

  size_t max_length = MAXLOOKBEHIND;

  size_t pos = 0;
  size_t new_pos;
  int inames = 0;
  std::ostringstream buf;

  while ((new_pos = pattern.find ("(?", pos)) != std::string::npos)
    {
      if (pattern.at (new_pos + 2) == '<'
          && !(pattern.at (new_pos + 3) == '='
               || pattern.at (new_pos + 3) == '!'))
        {
          // The syntax of named tokens in pcre is "(?P<name>...)" while
          // we need a syntax "(?<name>...)", so fix that here. Also an
          // expression like
          // "(?<first>\w+)\s+(?<last>\w+)|(?<last>\w+),\s+(?<first>\w+)"
          // should be perfectly legal, while pcre does not allow the same
          // named token name on both sides of the alternative. Also fix
          // that here by replacing name tokens by dummy names, and dealing
          // with the dummy names later.

          size_t tmp_pos = pattern.find_first_of ('>', new_pos);

          if (tmp_pos == std::string::npos)
            {
              (*current_liboctave_error_handler)
                ("regexp: syntax error in pattern");
              return;
            }

          std::string tmp_name =
            pattern.substr (new_pos+3, tmp_pos-new_pos-3);

          bool found = false;

          for (int i = 0; i < nnames; i++)
            {
              if (named_pats(i) == tmp_name)
                {
                  named_idx.resize (dim_vector (inames+1, 1));
                  named_idx(inames) = i;
                  found = true;
                  break;
                }
            }

          if (! found)
            {
              named_idx.resize (dim_vector (inames+1, 1));
              named_idx(inames) = nnames;
              named_pats.append (tmp_name);
              nnames++;
            }

          if (new_pos - pos > 0)
            buf << pattern.substr (pos, new_pos-pos);
          if (inames < 10)
            buf << "(?P<n00" << inames++;
          else if (inames < 100)
            buf << "(?P<n0" << inames++;
          else
            buf << "(?P<n" << inames++;

          pos = tmp_pos;
        }
      else if (pattern.at (new_pos + 2) == '<')
        {
          // Find lookbehind operators of arbitrary length (ie like
          // "(?<=[a-z]*)") and replace with a maximum length operator
          // as PCRE can not yet handle arbitrary length lookahead
          // operators. Use the string length as the maximum length to
          // avoid issues.

          int brackets = 1;
          size_t tmp_pos1 = new_pos + 2;
          size_t tmp_pos2 = tmp_pos1;

          while (tmp_pos1 < pattern.length () && brackets > 0)
            {
              char ch = pattern.at (tmp_pos1);

              if (ch == '(')
                brackets++;
              else if (ch == ')')
                {
                  if (brackets > 1)
                    tmp_pos2 = tmp_pos1;

                  brackets--;
                }

              tmp_pos1++;
            }

          if (brackets != 0)
            {
              buf << pattern.substr (pos, new_pos - pos) << "(?";
              pos = new_pos + 2;
            }
          else
            {
              size_t tmp_pos3 = pattern.find_first_of ("*+", tmp_pos2);

              if (tmp_pos3 != std::string::npos && tmp_pos3 < tmp_pos1)
                {
                  if (!lookbehind_warned)
                    {
                      lookbehind_warned = true;
                      (*current_liboctave_warning_with_id_handler)
                        ("Octave:regexp-lookbehind-limit",
                         "%s: arbitrary length lookbehind patterns are only supported up to length %d",
                         who.c_str (), MAXLOOKBEHIND);
                    }

                  buf << pattern.substr (pos, new_pos - pos) << "(";

                  size_t i;

                  if (pattern.at (tmp_pos3) == '*')
                    i = 0;
                  else
                    i = 1;

                  for (; i < max_length + 1; i++)
                    {
                      buf << pattern.substr (new_pos, tmp_pos3 - new_pos)
                          << "{" << i << "}";
                      buf << pattern.substr (tmp_pos3 + 1,
                                             tmp_pos1 - tmp_pos3 - 1);
                      if (i != max_length)
                        buf << "|";
                    }
                  buf << ")";
                }
              else
                buf << pattern.substr (pos, tmp_pos1 - pos);

              pos = tmp_pos1;
            }
        }
      else
        {
          buf << pattern.substr (pos, new_pos - pos) << "(?";
          pos = new_pos + 2;
        }

    }

  buf << pattern.substr (pos);

  const char *err;
  int erroffset;
  std::string buf_str = buf.str ();

  int pcre_options
    = ((options.case_insensitive () ? PCRE_CASELESS : 0)
       | (options.dotexceptnewline () ? 0 : PCRE_DOTALL)
       | (options.lineanchors () ? PCRE_MULTILINE : 0)
       | (options.freespacing () ? PCRE_EXTENDED : 0));

  data = pcre_compile (buf_str.c_str (), pcre_options, &err, &erroffset, 0);

  if (! data)
    (*current_liboctave_error_handler)
      ("%s: %s at position %d of expression", who.c_str (),
       err, erroffset);
}

regexp::match_data
regexp::match (const std::string& buffer)
{
  regexp::match_data retval;

  std::list<regexp::match_element> lst;

  int subpatterns;
  int namecount;
  int nameentrysize;
  char *nametable;
  size_t idx = 0;

  pcre *re = static_cast<pcre *> (data);

  pcre_fullinfo (re, 0, PCRE_INFO_CAPTURECOUNT,  &subpatterns);
  pcre_fullinfo (re, 0, PCRE_INFO_NAMECOUNT, &namecount);
  pcre_fullinfo (re, 0, PCRE_INFO_NAMEENTRYSIZE, &nameentrysize);
  pcre_fullinfo (re, 0, PCRE_INFO_NAMETABLE, &nametable);

  OCTAVE_LOCAL_BUFFER (int, ovector, (subpatterns+1)*3);
  OCTAVE_LOCAL_BUFFER (int, nidx, namecount);

  for (int i = 0; i < namecount; i++)
    {
      // Index of subpattern in first two bytes MSB first of name.
      // Extract index.
      nidx[i] = (static_cast<int> (nametable[i*nameentrysize])) << 8
                | static_cast<int> (nametable[i*nameentrysize+1]);
    }

  while (true)
    {
      OCTAVE_QUIT;

      int matches = pcre_exec (re, 0, buffer.c_str (),
                               buffer.length (), idx,
                               (idx ? PCRE_NOTBOL : 0),
                               ovector, (subpatterns+1)*3);

      if (matches == PCRE_ERROR_MATCHLIMIT)
        {
          // Try harder; start with default value for MATCH_LIMIT
          // and increase it.
          (*current_liboctave_warning_with_id_handler)
            ("Octave:regexp-match-limit",
             "your pattern caused PCRE to hit its MATCH_LIMIT; trying harder now, but this will be slow");

          pcre_extra pe;

          pcre_config (PCRE_CONFIG_MATCH_LIMIT,
                       static_cast<void *> (&pe.match_limit));

          pe.flags = PCRE_EXTRA_MATCH_LIMIT;

          int i = 0;
          while (matches == PCRE_ERROR_MATCHLIMIT
                 && i++ < PCRE_MATCHLIMIT_MAX)
            {
              OCTAVE_QUIT;

              pe.match_limit *= 10;
              matches = pcre_exec (re, &pe, buffer.c_str (),
                                   buffer.length (), idx,
                                   (idx ? PCRE_NOTBOL : 0),
                                   ovector, (subpatterns+1)*3);
            }
        }

      if (matches < 0 && matches != PCRE_ERROR_NOMATCH)
        {
          (*current_liboctave_error_handler)
            ("%s: internal error calling pcre_exec; error code from pcre_exec is %i",
             who.c_str (), matches);
          return retval;
        }
      else if (matches == PCRE_ERROR_NOMATCH)
        break;
      else if (ovector[1] <= ovector[0] && ! options.emptymatch ())
        {
          // Zero length match.  Skip to next char.
          idx = ovector[0] + 1;
          if (idx < buffer.length ())
            continue;
          else
            break;
        }
      else
        {
          int pos_match = 0;
          Matrix token_extents (matches-1, 2);

          for (int i = 1; i < matches; i++)
            {
              if (ovector[2*i] >= 0 && ovector[2*i+1] > 0
                  && (i == 1 || ovector[2*i] != ovector[2*i-2]
                      || ovector[2*i-1] != ovector[2*i+1]))
                {
                  token_extents(pos_match,0) = double (ovector[2*i]+1);
                  token_extents(pos_match++,1) = double (ovector[2*i+1]);
                }
            }

          token_extents.resize (pos_match, 2);

          double start = double (ovector[0]+1);
          double end = double (ovector[1]);

          const char **listptr;
          int status = pcre_get_substring_list (buffer.c_str (), ovector,
                                                matches, &listptr);

          if (status == PCRE_ERROR_NOMEMORY)
            {
              (*current_liboctave_error_handler)
                ("%s: cannot allocate memory in pcre_get_substring_list",
                 who.c_str ());
              return retval;
            }

          string_vector tokens (pos_match);
          string_vector named_tokens (nnames);
          int pos_offset = 0;
          pos_match = 0;

          for (int i = 1; i < matches; i++)
            {
              if (ovector[2*i] >= 0 && ovector[2*i+1] > 0)
                {
                  if (i == 1 || ovector[2*i] != ovector[2*i-2]
                      || ovector[2*i-1] != ovector[2*i+1])
                    {
                      if (namecount > 0)
                        {
                          // FIXME: Should probably do this with a map()
                          //        rather than a linear search.  However,
                          //        the number of captured, named expressions
                          //        is usually pretty small (< 4)
                          for (int j = 0; j < namecount; j++)
                            {
                              if (nidx[j] == i)
                                {
                                  named_tokens(named_idx(j)) =
                                    std::string (*(listptr+i-pos_offset));
                                  break;
                                }
                            }
                        }

                      tokens(pos_match++) = std::string (*(listptr+i));
                    }
                  else
                    pos_offset++;
                }
            }

          std::string match_string = std::string (*listptr);

          pcre_free_substring_list (listptr);

          regexp::match_element new_elem (named_tokens, tokens, match_string,
                                          token_extents, start, end);
          lst.push_back (new_elem);

          if (ovector[1] <= ovector[0])
            {
              // Zero length match.  Skip to next char.
              idx = ovector[0] + 1;
              if (idx <= buffer.length ())
                continue;
            }
          else
            idx = ovector[1];

          if (options.once () || idx >= buffer.length ())
            break;
        }
    }

  retval = regexp::match_data (lst, named_pats);

  return retval;
}

bool
regexp::is_match (const std::string& buffer)
{
  regexp::match_data rx_lst = match (buffer);

  return rx_lst.size () > 0;
}

Array<bool>
regexp::is_match (const string_vector& buffer)
{
  octave_idx_type len = buffer.length ();

  Array<bool> retval (dim_vector (len, 1));

  for (octave_idx_type i = 0; i < buffer.length (); i++)
    retval(i) = is_match (buffer(i));

  return retval;
}

// Declare rep_token_t used in processing replacement string
typedef struct
{
  size_t pos;
  int num;
} rep_token_t;


std::string
regexp::replace (const std::string& buffer, const std::string& replacement)
{
  std::string retval;

  regexp::match_data rx_lst = match (buffer);

  size_t num_matches = rx_lst.size ();

  if (num_matches == 0)
    {
      retval = buffer;
      return retval;
    }

  // Identify replacement tokens; build a vector of group numbers in
  // the replacement string so that we can quickly calculate the size
  // of the replacement.

  // FIXME: All code assumes that only 10 tokens ($0-$9) exist.
  //        $11 represents $1 followed by the character '1' rather than
  //        the eleventh capture buffer.

  std::string repstr = replacement;
  std::vector<rep_token_t> tokens;
  tokens.reserve (5);  // Reserve memory for 5 pattern replacements

  for (size_t i=0; i < repstr.size (); i++)
    {
      if (repstr[i] == '\\')
        {
          if (i < repstr.size () - 1 && repstr[i+1] == '$')
            {
              repstr.erase (i,1);  // erase backslash
              i++;                 // skip over '$'
              continue;
            }
          if (i < repstr.size () - 1 && repstr[i+1] == '\\')
            {
              repstr.erase (i,1);  // erase 1st backslash
              continue;
            }
        }
      else if (repstr[i] == '$')
        {
          if (i < repstr.size () - 1 && isdigit (repstr[i+1]))
            {
              rep_token_t tmp_token;

              tmp_token.pos = i;
              tmp_token.num = repstr[i+1]-'0';
              tokens.push_back (tmp_token);
            }
        }
    }

  std::string rep;
  int num_tokens = tokens.size ();

  if (num_tokens > 0)
    {
      // Determine replacement length
      const size_t replen = repstr.size () - 2*num_tokens;
      int delta = 0;
      regexp::match_data::const_iterator p = rx_lst.begin ();
      for (size_t i = 0; i < num_matches; i++)
        {
          OCTAVE_QUIT;

          double start = p->start ();
          double end = p->end ();

          const Matrix pairs (p->token_extents ());
          size_t pairlen = 0;
          for (int j = 0; j < num_tokens; j++)
            {
              if (tokens[j].num == 0)
                pairlen += static_cast<size_t> (end - start) + 1;
              else if (tokens[j].num <= pairs.rows ())
                pairlen += static_cast<size_t> (pairs(tokens[j].num-1,1)
                                                - pairs(tokens[j].num-1,0)) + 1;
            }
          delta += (static_cast<int> (replen + pairlen)
                    - static_cast<int> (end - start + 1));
          p++;
        }

      // Build replacement string
      rep.reserve (buffer.size () + delta);
      size_t from = 0;
      p = rx_lst.begin ();
      for (size_t i = 0; i < num_matches; i++)
        {
          OCTAVE_QUIT;

          double start = p->start ();
          double end = p->end ();

          const Matrix pairs (p->token_extents ());
          rep.append (&buffer[from], static_cast<size_t> (start - 1) - from);
          from = static_cast<size_t> (end);

          size_t cur_pos = 0;

          for (int j = 0; j < num_tokens; j++)
            {
              rep.append (&repstr[cur_pos], (tokens[j].pos) - cur_pos);
              cur_pos = tokens[j].pos+2;

              int k = tokens[j].num;
              if (k == 0)
                {
                  // replace with entire match
                  rep.append (&buffer[static_cast<size_t> (end - 1)],
                              static_cast<size_t> (end - start) + 1);
                }
              else if (k <= pairs.rows ())
                {
                  // replace with group capture
                  rep.append (&buffer[static_cast<size_t> (pairs(k-1,0)-1)],
                              static_cast<size_t> (pairs(k-1,1)
                                                   - pairs(k-1,0)) + 1);
                }
              else
                {
                  // replace with nothing
                }
            }
          if (cur_pos < repstr.size ())
            rep.append (&repstr[cur_pos], repstr.size () - cur_pos);

          p++;
        }
      rep.append (&buffer[from], buffer.size () - from);
    }
  else
    {
      // Determine repstr length
      const size_t replen = repstr.size ();
      int delta = 0;
      regexp::match_data::const_iterator p = rx_lst.begin ();
      for (size_t i = 0; i < num_matches; i++)
        {
          OCTAVE_QUIT;
          delta += static_cast<int> (replen)
                   - static_cast<int> (p->end () - p->start () + 1);
          p++;
        }

      // Build replacement string
      rep.reserve (buffer.size () + delta);
      size_t from = 0;
      p = rx_lst.begin ();
      for (size_t i = 0; i < num_matches; i++)
        {
          OCTAVE_QUIT;
          rep.append (&buffer[from],
                      static_cast<size_t> (p->start () - 1) - from);
          from = static_cast<size_t> (p->end ());
          rep.append (repstr);
          p++;
        }
      rep.append (&buffer[from], buffer.size () - from);
    }

  retval = rep;
  return retval;
}
