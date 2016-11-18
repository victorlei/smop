/*

Copyright (C) 2003-2015 John W. Eaton

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

#if !defined (octave_ls_oct_ascii_h)
#define octave_ls_oct_ascii_h 1

#include <cfloat>

#include <sstream>
#include <string>

#include "str-vec.h"

#include "ls-ascii-helper.h"

// Flag for cell elements
#define CELL_ELT_TAG "<cell-element>"

// Used when converting Inf to something that gnuplot can read.

#ifndef OCT_RBV
#define OCT_RBV (std::numeric_limits<double>::max () / 100.0)
#endif

extern OCTINTERP_API std::string
extract_keyword (std::istream& is, const char *keyword,
                 const bool next_only = false);

extern OCTINTERP_API std::string
read_ascii_data (std::istream& is, const std::string& filename, bool& global,
                 octave_value& tc, octave_idx_type count);

extern OCTINTERP_API bool
save_ascii_data (std::ostream& os, const octave_value& val_arg,
                 const std::string& name, bool mark_as_global, int precision);

extern OCTINTERP_API bool
save_ascii_data_for_plotting (std::ostream& os, const octave_value& t,
                              const std::string& name);

extern OCTINTERP_API bool
save_three_d (std::ostream& os, const octave_value& t,
              bool parametric = false);

// Match KEYWORD on stream IS, placing the associated value in VALUE,
// returning TRUE if successful and FALSE otherwise.
//
// Input should look something like:
//
//  [%#][ \t]*keyword[ \t]*int-value.*\n

template <class T>
bool
extract_keyword (std::istream& is, const char *keyword, T& value,
                 const bool next_only = false)
{
  bool status = false;
  value = T ();

  char c;
  while (is.get (c))
    {
      if (c == '%' || c == '#')
        {
          std::ostringstream buf;

          while (is.get (c) && (c == ' ' || c == '\t' || c == '%' || c == '#'))
            ; // Skip whitespace and comment characters.

          if (isalpha (c))
            buf << c;

          while (is.get (c) && isalpha (c))
            buf << c;

          std::string tmp = buf.str ();
          bool match = (tmp.compare (0, strlen (keyword), keyword) == 0);

          if (match)
            {
              while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
                ; // Skip whitespace and the colon.

              is.putback (c);
              if (c != '\n' && c != '\r')
                is >> value;
              if (is)
                status = true;
              skip_until_newline (is, false);
              break;
            }
          else if (next_only)
            break;
        }
    }
  return status;
}

template <class T>
bool
extract_keyword (std::istream& is, const std::string& kw, T& value,
                 const bool next_only = false)
{
  return extract_keyword (is, kw.c_str (), value, next_only);
}

// Match one of the elements in KEYWORDS on stream IS, placing the
// matched keyword in KW and the associated value in VALUE,
// returning TRUE if successful and FALSE otherwise.
//
// Input should look something like:
//
//  [%#][ \t]*keyword[ \t]*int-value.*\n

template <class T>
bool
extract_keyword (std::istream& is, const string_vector& keywords,
                 std::string& kw, T& value, const bool next_only = false)
{
  bool status = false;
  kw = "";
  value = 0;

  char c;
  while (is.get (c))
    {
      if (c == '%' || c == '#')
        {
          std::ostringstream buf;

          while (is.get (c) && (c == ' ' || c == '\t' || c == '%' || c == '#'))
            ; // Skip whitespace and comment characters.

          if (isalpha (c))
            buf << c;

          while (is.get (c) && isalpha (c))
            buf << c;

          std::string tmp = buf.str ();

          for (int i = 0; i < keywords.length (); i++)
            {
              int match = (tmp == keywords[i]);

              if (match)
                {
                  kw = keywords[i];

                  while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
                    ; // Skip whitespace and the colon.

                  is.putback (c);
                  if (c != '\n' && c != '\r')
                    is >> value;
                  if (is)
                    status = true;
                  skip_until_newline (is, false);
                  return status;
                }
            }

          if (next_only)
            break;
        }
    }
  return status;
}

#endif
